/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2013, 2012, 2011, 2010, 2009  University of Chicago
  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.
*/

#include "NrrdIO.h"
#include "privateNrrd.h"

#include <limits.h>

/* The "/ *Teem:" (without space) comments in here are an experiment */

const char *
nrrdBiffKey = "nrrd";

/*
******** nrrdSpaceDimension
**
** returns expected dimension of given space (from nrrdSpace* enum), or,
** 0 if there is no expected dimension.
**
** The expected behavior here is to return 0 for nrrdSpaceUnknown, because
** that is the right answer, not because its an error of any kind.
*/
unsigned int
nrrdSpaceDimension(int space) {
  static const char me[]="nrrdSpaceDimension";
  unsigned int ret;

  if (!( AIR_IN_OP(nrrdSpaceUnknown, space, nrrdSpaceLast) )) {
    /* they gave us invalid or unknown space */
    return 0;
  }
  switch (space) {
  case nrrdSpaceRightAnteriorSuperior:
  case nrrdSpaceLeftAnteriorSuperior:
  case nrrdSpaceLeftPosteriorSuperior:
  case nrrdSpaceScannerXYZ:
  case nrrdSpace3DRightHanded:
  case nrrdSpace3DLeftHanded:
    ret = 3;
    break;
  case nrrdSpaceRightAnteriorSuperiorTime:
  case nrrdSpaceLeftAnteriorSuperiorTime:
  case nrrdSpaceLeftPosteriorSuperiorTime:
  case nrrdSpaceScannerXYZTime:
  case nrrdSpace3DRightHandedTime:
  case nrrdSpace3DLeftHandedTime:
    ret = 4;
    break;
  default:
    fprintf(stderr, "%s: PANIC: nrrdSpace %d not implemented!\n", me, space);
    ret = UINT_MAX; /* exit(1); */
    break;
  }
  return ret;
}

/*
******** nrrdSpaceSet
**
** What to use to set space, when a value from nrrdSpace enum is known,
** or, to nullify all space-related information when passed nrrdSpaceUnknown
*/
int
nrrdSpaceSet(Nrrd *nrrd, int space) {
  static const char me[]="nrrdSpaceSet";
  unsigned axi, saxi;

  if (!nrrd) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (nrrdSpaceUnknown == space) {
    nrrd->space = nrrdSpaceUnknown;
    nrrd->spaceDim = 0;
    for (axi=0; axi<NRRD_DIM_MAX; axi++) {
      nrrdSpaceVecSetNaN(nrrd->axis[axi].spaceDirection);
    }
    for (saxi=0; saxi<NRRD_SPACE_DIM_MAX; saxi++) {
      airFree(nrrd->spaceUnits[saxi]);
      nrrd->spaceUnits[saxi] = NULL;
    }
    nrrdSpaceVecSetNaN(nrrd->spaceOrigin);
  } else {
    if (airEnumValCheck(nrrdSpace, space)) {
      biffAddf(NRRD, "%s: given space (%d) not valid", me, space);
      return 1;
    }
    nrrd->space = space;
    nrrd->spaceDim = nrrdSpaceDimension(space);
  }
  return 0;
}

/*
******** nrrdSpaceDimensionSet
**
** What to use to set space, based on spaceDim alone (nrrd->space set to
** nrrdSpaceUnknown)
*/
int
nrrdSpaceDimensionSet(Nrrd *nrrd, unsigned int spaceDim) {
  static const char me[]="nrrdSpaceDimensionSet";

  if (!nrrd) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!( spaceDim <= NRRD_SPACE_DIM_MAX )) {
    biffAddf(NRRD, "%s: given spaceDim (%u) not valid", me, spaceDim);
    return 1;
  }
  nrrd->space = nrrdSpaceUnknown;
  nrrd->spaceDim = spaceDim;
  return 0;
}

/*
******** nrrdSpaceOriginGet
**
** retrieves the spaceOrigin from given nrrd, and returns spaceDim
** Indices 0 through spaceDim-1 are set in given vector[] to coords
** of space origin, and all further indices are set to NaN. That is,
** this really does write to all NRRD_SPACE_DIM_MAX elements of vector[]
*/
unsigned int
nrrdSpaceOriginGet(const Nrrd *nrrd,
                   double vector[NRRD_SPACE_DIM_MAX]) {
  unsigned int sdi, ret;

  if (nrrd && vector) {
    for (sdi=0; sdi<nrrd->spaceDim; sdi++) {
      vector[sdi] = nrrd->spaceOrigin[sdi];
    }
    for (sdi=nrrd->spaceDim; sdi<NRRD_SPACE_DIM_MAX; sdi++) {
      vector[sdi] = AIR_NAN;
    }
    ret = nrrd->spaceDim;
  } else {
    ret = 0;
  }
  return ret;
}

/*
******** nrrdSpaceOriginSet
**
** convenience function for setting spaceOrigin.
** Note: space (or spaceDim) must be already set.
** The given "vector" is only read for spaceDim elements
**
** returns 1 if there were problems, 0 otherwise
*/
int
nrrdSpaceOriginSet(Nrrd *nrrd, const double *vector) {
  static const char me[]="nrrdSpaceOriginSet";
  unsigned int sdi;

  if (!( nrrd && vector )) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!( 0 < nrrd->spaceDim && nrrd->spaceDim <= NRRD_SPACE_DIM_MAX )) {
    biffAddf(NRRD, "%s: set spaceDim %d not valid", me, nrrd->spaceDim);
    return 1;
  }

  for (sdi=0; sdi<nrrd->spaceDim; sdi++) {
    nrrd->spaceOrigin[sdi] = vector[sdi];
  }
  for (sdi=nrrd->spaceDim; sdi<NRRD_SPACE_DIM_MAX; sdi++) {
    nrrd->spaceOrigin[sdi] = AIR_NAN;
  }
  return 0;
}

/*
******** nrrdOriginCalculate
**
** makes an effort to calculate something like an "origin" (as in
** nrrd->spaceOrigin) from the per-axis min, max, or spacing, when
** there is no real space information.  Like the spaceOrigin, this
** location is supposed to be THE CENTER of the first sample.  To
** avoid making assumptions about the nrrd or the caller, a default
** sample centering (defaultCenter) has to be provided (use either
** nrrdCenterNode or nrrdCenterCell).  The axes that are used
** for the origin calculation have to be given explicitly- but they
** are likely the return of nrrdDomainAxesGet
**
** The computed origin is put into the given vector (origin).  The return
** value takes on values from the nrrdOriginStatus* enum:
**
** nrrdOriginStatusUnknown:        invalid arguments (e.g. NULL pointer, or
**                                 axis values out of range)
**
** nrrdOriginStatusDirection:      the chosen axes have spaceDirection set,
**                                 which means caller should instead be using
**                                 nrrdSpaceOriginGet
**
** nrrdOriginStatusNoMin:          can't compute "origin" without axis->min
**
** nrrdOriginStatusNoMaxOrSpacing: can't compute origin without (axis->min
**                                 and) either axis->max or axis->spacing
**
** nrrdOriginStatusOkay:           all is well
*/
int
nrrdOriginCalculate(const Nrrd *nrrd,
                    unsigned int *axisIdx, unsigned int axisIdxNum,
                    int defaultCenter, double *origin) {
  const NrrdAxisInfo *axis[NRRD_SPACE_DIM_MAX];
  int center, okay, gotSpace, gotMin, gotMaxOrSpacing;
  unsigned int ai;
  double min, spacing;

#define ERROR \
  if (origin) { \
    for (ai=0; ai<axisIdxNum; ai++) { \
      origin[ai] = AIR_NAN; \
    } \
  }

  if (!( nrrd
         && (nrrdCenterCell == defaultCenter
             || nrrdCenterNode == defaultCenter)
         && origin )) {
    ERROR;
    return nrrdOriginStatusUnknown;
  }

  okay = AIR_TRUE;
  for (ai=0; ai<axisIdxNum; ai++) {
    okay &= axisIdx[ai] < nrrd->dim;
  }
  if (!okay) {
    ERROR;
    return nrrdOriginStatusUnknown;
  }

  /* learn axisInfo pointers */
  for (ai=0; ai<axisIdxNum; ai++) {
    axis[ai] = nrrd->axis + axisIdx[ai];
  }

  gotSpace = AIR_FALSE;
  for (ai=0; ai<axisIdxNum; ai++) {
    gotSpace |= AIR_EXISTS(axis[ai]->spaceDirection[0]);
  }
  if (nrrd->spaceDim > 0 && gotSpace) {
    ERROR;
    return nrrdOriginStatusDirection;
  }

  gotMin = AIR_TRUE;
  for (ai=0; ai<axisIdxNum; ai++) {
    gotMin &= AIR_EXISTS(axis[0]->min);
  }
  if (!gotMin) {
    ERROR;
    return nrrdOriginStatusNoMin;
  }

  gotMaxOrSpacing = AIR_TRUE;
  for (ai=0; ai<axisIdxNum; ai++) {
    gotMaxOrSpacing &= (AIR_EXISTS(axis[ai]->max)
                        || AIR_EXISTS(axis[ai]->spacing));
  }
  if (!gotMaxOrSpacing) {
    ERROR;
    return nrrdOriginStatusNoMaxOrSpacing;
  }

  for (ai=0; ai<axisIdxNum; ai++) {
    size_t size;
    double denom;
    size = axis[ai]->size;
    min = axis[ai]->min;
    center = (nrrdCenterUnknown != axis[ai]->center
              ? axis[ai]->center
              : defaultCenter);
    denom = AIR_CAST(double, nrrdCenterCell == center ? size : size-1);
    spacing = (AIR_EXISTS(axis[ai]->spacing)
               ? axis[ai]->spacing
               : (axis[ai]->max - min)/denom);
    origin[ai] = min + (nrrdCenterCell == center ? spacing/2 : 0);
  }
  return nrrdOriginStatusOkay;
}

void
nrrdSpaceVecCopy(double dst[NRRD_SPACE_DIM_MAX],
                  const double src[NRRD_SPACE_DIM_MAX]) {
  unsigned int ii;

  for (ii=0; ii<NRRD_SPACE_DIM_MAX; ii++) {
    dst[ii] = src[ii];
  }
}

/*
** NOTE: since this was created until Wed Sep 21 13:34:17 EDT 2005,
** nrrdSpaceVecScaleAdd2 and nrrdSpaceVecScale would treat a
** non-existent vector coefficient as 0.0.  The reason for this had
** to do with how the function is used.  For example, in nrrdCrop
**
**   _nrrdSpaceVecCopy(nout->spaceOrigin, nin->spaceOrigin);
**   for (ai=0; ai<nin->dim; ai++) {
**      _nrrdSpaceVecScaleAdd2(nout->spaceOrigin,
**                             1.0, nout->spaceOrigin,
**                             min[ai], nin->axis[ai].spaceDirection);
**   }
**
** but the problem with this is that non-spatial axes end up clobbering
** the existance of otherwise existing spaceOrigin and spaceDirections.
** It was decided, however, that this logic should be outside the
** arithmetic functions below, not inside.  NOTE: the old functionality
** is stuck in ITK 2.2, via NrrdIO.
*/

void
nrrdSpaceVecScaleAdd2(double sum[NRRD_SPACE_DIM_MAX],
                       double sclA, const double vecA[NRRD_SPACE_DIM_MAX],
                       double sclB, const double vecB[NRRD_SPACE_DIM_MAX]) {
  unsigned int ii;

  for (ii=0; ii<NRRD_SPACE_DIM_MAX; ii++) {
    sum[ii] = sclA*vecA[ii] + sclB*vecB[ii];
  }
}

void
nrrdSpaceVecScale(double out[NRRD_SPACE_DIM_MAX],
                   double scl, const double vec[NRRD_SPACE_DIM_MAX]) {
  unsigned int ii;

  for (ii=0; ii<NRRD_SPACE_DIM_MAX; ii++) {
    out[ii] = scl*vec[ii];
  }
}

double
nrrdSpaceVecNorm(unsigned int sdim, const double vec[NRRD_SPACE_DIM_MAX]) {
  unsigned int di;
  double nn;

  nn = 0;
  for (di=0; di<sdim; di++) {
    nn += vec[di]*vec[di];
  }
  return sqrt(nn);
}

void
nrrdSpaceVecSetNaN(double vec[NRRD_SPACE_DIM_MAX]) {
  unsigned int di;

  for (di=0; di<NRRD_SPACE_DIM_MAX; di++) {
    vec[di] = AIR_NAN;
  }
  return;
}


/*
** _nrrdContentGet
**
** ALLOCATES a string for the content of a given nrrd
** panics if allocation failed
*/
char *
_nrrdContentGet(const Nrrd *nin) {
  static const char me[]="_nrrdContentGet";
  char *ret;

  ret = ((nin && nin->content)
         ? airStrdup(nin->content)
         : airStrdup(nrrdStateUnknownContent));
  if (!ret) {
    fprintf(stderr, "%s: PANIC: content strdup failed!\n", me);
    return NULL;
  }
  return ret;
}

int
_nrrdContentSet_nva(Nrrd *nout, const char *func,
                    char *content, const char *format, va_list arg) {
  static const char me[]="_nrrdContentSet_nva";
  char *buff;

  if (nrrdStateDisableContent) {
    /* we kill content always */
    nout->content = (char *)airFree(nout->content);
    return 0;
  }
  buff = (char *)malloc(128*AIR_STRLEN_HUGE);
  if (!buff) {
    biffAddf(NRRD, "%s: couln't alloc buffer!", me);
    return 1;
  }
  nout->content = (char *)airFree(nout->content);

  /* we are currently praying that this won't overflow the "buff" array */
  /* HEY: replace with vsnprintf or whatever when its available */
  vsprintf(buff, format, arg);

  nout->content = (char *)calloc(strlen("(,)")
                                 + airStrlen(func)
                                 + 1                      /* '(' */
                                 + airStrlen(content)
                                 + 1                      /* ',' */
                                 + airStrlen(buff)
                                 + 1                      /* ')' */
                                 + 1, sizeof(char));      /* '\0' */
  if (!nout->content) {
    biffAddf(NRRD, "%s: couln't alloc output content!", me);
    airFree(buff); return 1;
  }
  sprintf(nout->content, "%s(%s%s%s)", func, content,
          airStrlen(buff) ? "," : "", buff);
  airFree(buff);  /* no NULL assignment, else compile warnings */
  return 0;
}

int
_nrrdContentSet_va(Nrrd *nout, const char *func,
                   char *content, const char *format, ...) {
  static const char me[]="_nrrdContentSet_va";
  va_list ap;

  va_start(ap, format);
  if (_nrrdContentSet_nva(nout, func, content, format, ap)) {
    biffAddf(NRRD, "%s:", me);
    free(content); return 1;
  }
  va_end(ap);

  /* free(content);  */
  return 0;
}

/*
******** nrrdContentSet
**
** Kind of like sprintf, but for the content string of the nrrd.
**
** Whether or not we write a new content for an old nrrd ("nin") with
** NULL content is decided here, according to
** nrrdStateAlwaysSetContent.
**
** Does the string allocation and some attempts at error detection.
** Does allow nout==nin, which requires some care.
*/
int
nrrdContentSet_va(Nrrd *nout, const char *func,
                  const Nrrd *nin, const char *format, ...) {
  static const char me[]="nrrdContentSet_va";
  va_list ap;
  char *content;

  if (!(nout && func && nin && format)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (nrrdStateDisableContent) {
    /* we kill content always */
    nout->content = (char *)airFree(nout->content);
    return 0;
  }
  if (!nin->content && !nrrdStateAlwaysSetContent) {
    /* there's no input content, and we're not supposed to invent any
       content, so after freeing nout's content we're done */
    nout->content = (char *)airFree(nout->content);
    return 0;
  }
  /* we copy the input nrrd content first, before blowing away the
     output content, in case nout == nin */
  content = _nrrdContentGet(nin);
  va_start(ap, format);
  if (_nrrdContentSet_nva(nout, func, content, format, ap)) {
    biffAddf(NRRD, "%s:", me);
    va_end(ap); free(content); return 1;
  }
  va_end(ap);
  free(content);

  return 0;
}

/*
******** nrrdDescribe
**
** writes verbose description of nrrd to given file
*/
void
nrrdDescribe(FILE *file, const Nrrd *nrrd) {
  unsigned int ai;
  char stmp[AIR_STRLEN_SMALL];

  if (file && nrrd) {
    fprintf(file, "Nrrd at 0x%p:\n", AIR_CVOIDP(nrrd));
    fprintf(file, "Data at 0x%p is %s elements of type %s.\n", nrrd->data,
            airSprintSize_t(stmp, nrrdElementNumber(nrrd)),
            airEnumStr(nrrdType, nrrd->type));
    if (nrrdTypeBlock == nrrd->type) {
      fprintf(file, "The blocks have size %s\n",
              airSprintSize_t(stmp, nrrd->blockSize));
    }
    if (airStrlen(nrrd->content)) {
      fprintf(file, "Content = \"%s\"\n", nrrd->content);
    }
    fprintf(file, "%d-dimensional array, with axes:\n", nrrd->dim);
    for (ai=0; ai<nrrd->dim; ai++) {
      if (airStrlen(nrrd->axis[ai].label)) {
        fprintf(file, "%d: (\"%s\") ", ai, nrrd->axis[ai].label);
      } else {
        fprintf(file, "%d: ", ai);
      }
      fprintf(file, "%s-centered, size=%s, ",
              airEnumStr(nrrdCenter, nrrd->axis[ai].center),
              airSprintSize_t(stmp, nrrd->axis[ai].size));
      airSinglePrintf(file, NULL, "spacing=%lg, \n", nrrd->axis[ai].spacing);
      airSinglePrintf(file, NULL, "thickness=%lg, \n",
                      nrrd->axis[ai].thickness);
      airSinglePrintf(file, NULL, "    axis(Min,Max) = (%lg,",
                       nrrd->axis[ai].min);
      airSinglePrintf(file, NULL, "%lg)\n", nrrd->axis[ai].max);
      if (airStrlen(nrrd->axis[ai].units)) {
        fprintf(file, "units=%s, \n", nrrd->axis[ai].units);
      }
    }
    /*
    airSinglePrintf(file, NULL, "The min, max values are %lg",
                     nrrd->min);
    airSinglePrintf(file, NULL, ", %lg\n", nrrd->max);
    */
    airSinglePrintf(file, NULL, "The old min, old max values are %lg",
                     nrrd->oldMin);
    airSinglePrintf(file, NULL, ", %lg\n", nrrd->oldMax);
    /* fprintf(file, "hasNonExist = %d\n", nrrd->hasNonExist); */
    if (nrrd->cmtArr->len) {
      fprintf(file, "Comments:\n");
      for (ai=0; ai<nrrd->cmtArr->len; ai++) {
        fprintf(file, "%s\n", nrrd->cmt[ai]);
      }
    }
    fprintf(file, "\n");
  }
}

int
nrrdSpaceVecExists(unsigned int sdim, double vec[NRRD_SPACE_DIM_MAX]) {
  int exists;
  unsigned int ii;

  exists = AIR_EXISTS(vec[0]);
  for (ii=1; ii<sdim; ii++) {
    exists &= AIR_EXISTS(vec[ii]);
  }
  return exists;
}

/*
** asserts all the properties associated with orientation information
**
** The most important part of this is asserting the per-axis mutual
** exclusion of min/max/spacing/units versus using spaceDirection.
*/
static int
_nrrdFieldCheckSpaceInfo(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheckSpaceInfo";
  unsigned int dd, ii;
  int exists;

  if (!( !nrrd->space || !airEnumValCheck(nrrdSpace, nrrd->space) )) {
    biffMaybeAddf(useBiff, NRRD, "%s: space %d invalid",
                  me, nrrd->space);
    return 1;
  }
  if (!( nrrd->spaceDim <= NRRD_SPACE_DIM_MAX )) {
    biffMaybeAddf(useBiff, NRRD, "%s: space dimension %d is outside "
                  "valid range [0,NRRD_SPACE_DIM_MAX] = [0,%d]",
                  me, nrrd->dim, NRRD_SPACE_DIM_MAX);
    return 1;
  }
  if (nrrd->spaceDim) {
    if (nrrd->space) {
      if (nrrdSpaceDimension(nrrd->space) != nrrd->spaceDim) {
        biffMaybeAddf(useBiff, NRRD,
                      "%s: space %s has dimension %d but spaceDim is %d",
                      me, airEnumStr(nrrdSpace, nrrd->space),
                      nrrdSpaceDimension(nrrd->space), nrrd->spaceDim);
        return 1;
      }
    }
    /* check that all coeffs of spaceOrigin have consistent existance */
    exists = AIR_EXISTS(nrrd->spaceOrigin[0]);
    for (ii=0; ii<nrrd->spaceDim; ii++) {
      if (exists ^ AIR_EXISTS(nrrd->spaceOrigin[ii])) {
        biffMaybeAddf(useBiff, NRRD,
                      "%s: existance of space origin coefficients must "
                      "be consistent (val[0] not like val[%d])", me, ii);
        return 1;
      }
    }
    /* check that all coeffs of measurementFrame have consistent existance */
    exists = AIR_EXISTS(nrrd->measurementFrame[0][0]);
    for (dd=0; dd<nrrd->spaceDim; dd++) {
      for (ii=0; ii<nrrd->spaceDim; ii++) {
        if (exists ^ AIR_EXISTS(nrrd->measurementFrame[dd][ii])) {
          biffMaybeAddf(useBiff, NRRD,
                        "%s: existance of measurement frame coefficients "
                        "must be consistent: [col][row] [%d][%d] not "
                        "like [0][0])", me, dd, ii);
          return 1;
        }
      }
    }
    /* check on space directions */
    for (dd=0; dd<nrrd->dim; dd++) {
      exists = AIR_EXISTS(nrrd->axis[dd].spaceDirection[0]);
      for (ii=1; ii<nrrd->spaceDim; ii++) {
        if (exists ^ AIR_EXISTS(nrrd->axis[dd].spaceDirection[ii])) {
          biffMaybeAddf(useBiff, NRRD,
                        "%s: existance of space direction %d coefficients "
                        "must be consistent (val[0] not like val[%d])", me,
                        dd, ii); return 1;
        }
      }
      if (exists) {
        if (AIR_EXISTS(nrrd->axis[dd].min)
            || AIR_EXISTS(nrrd->axis[dd].max)
            || AIR_EXISTS(nrrd->axis[dd].spacing)
            || !!airStrlen(nrrd->axis[dd].units)) {
          biffMaybeAddf(useBiff, NRRD,
                        "%s: axis[%d] has a direction vector, and so can't "
                        "have min, max, spacing, or units set", me, dd);
          return 1;
        }
      }
    }
  } else {
    /* else there's not supposed to be anything in "space" */
    if (nrrd->space) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: space %s can't be set with spaceDim %d",
                    me, airEnumStr(nrrdSpace, nrrd->space),
                    nrrd->spaceDim);
      return 1;
    }
    /* -------- */
    exists = AIR_FALSE;
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      exists |= !!airStrlen(nrrd->spaceUnits[dd]);
    }
    if (exists) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: spaceDim is 0, but space units is set", me);
      return 1;
    }
    /* -------- */
    exists = AIR_FALSE;
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      exists |= AIR_EXISTS(nrrd->spaceOrigin[dd]);
    }
    if (exists) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: spaceDim is 0, but space origin is set", me);
      return 1;
    }
    /* -------- */
    exists = AIR_FALSE;
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      for (ii=0; ii<NRRD_DIM_MAX; ii++) {
        exists |= AIR_EXISTS(nrrd->axis[ii].spaceDirection[dd]);
      }
    }
    if (exists) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: spaceDim is 0, but space directions are set", me);
      return 1;
    }
  }
  return 0;
}

/* --------------------- per-field checks ----------------
**
** Strictly speacking, these checks only apply to the nrrd itself, not
** to a potentially incomplete nrrd in the process of being read, so
** the NrrdIoState stuff is not an issue.  This limits the utility of
** these to the field parsers for handling the more complex state
** involved in parsing some of the NRRD fields (like units).
**
** return 0 if it is valid, and 1 if there is an error
*/

static int
_nrrdFieldCheck_noop(const Nrrd *nrrd, int useBiff) {

  AIR_UNUSED(nrrd);
  AIR_UNUSED(useBiff);
  return 0;
}

static int
_nrrdFieldCheck_type(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_type";

  if (airEnumValCheck(nrrdType, nrrd->type)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: type (%d) is not valid", me, nrrd->type);
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_block_size(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_block_size";
  char stmp[AIR_STRLEN_SMALL];

  if (nrrdTypeBlock == nrrd->type && (!(0 < nrrd->blockSize)) ) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: type is %s but nrrd->blockSize (%s) invalid", me,
                  airEnumStr(nrrdType, nrrdTypeBlock),
                  airSprintSize_t(stmp, nrrd->blockSize)); return 1;
  }
  if (nrrdTypeBlock != nrrd->type && (0 < nrrd->blockSize)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: type is %s (not block) but blockSize is %s", me,
                  airEnumStr(nrrdType, nrrd->type),
                  airSprintSize_t(stmp, nrrd->blockSize));
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_dimension(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_dimension";

  if (!AIR_IN_CL(1, nrrd->dim, NRRD_DIM_MAX)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: dimension %u is outside valid range [1,%d]",
                  me, nrrd->dim, NRRD_DIM_MAX);
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_space(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_space";

  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_space_dimension(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_space_dimension";

  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_sizes(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_sizes";
  size_t size[NRRD_DIM_MAX];

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoSize, size);
  if (_nrrdSizeCheck(size, nrrd->dim, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble with array sizes", me);
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_spacings(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_spacings";
  double val[NRRD_DIM_MAX];
  unsigned int ai;

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoSpacing, val);
  for (ai=0; ai<nrrd->dim; ai++) {
    if (!( !airIsInf_d(val[ai]) && (airIsNaN(val[ai]) || (0 != val[ai])) )) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: axis %d spacing (%g) invalid", me, ai, val[ai]);
      return 1;
    }
  }
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_thicknesses(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_thicknesses";
  double val[NRRD_DIM_MAX];
  unsigned int ai;

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoThickness, val);
  for (ai=0; ai<nrrd->dim; ai++) {
    /* note that unlike spacing, we allow zero thickness,
       but it makes no sense to be negative */
    if (!( !airIsInf_d(val[ai]) && (airIsNaN(val[ai]) || (0 <= val[ai])) )) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: axis %d thickness (%g) invalid", me, ai, val[ai]);
      return 1;
    }
  }
  return 0;
}

static int
_nrrdFieldCheck_axis_mins(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_axis_mins";
  double val[NRRD_DIM_MAX];
  unsigned int ai;
  int ret;

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoMin, val);
  for (ai=0; ai<nrrd->dim; ai++) {
    if ((ret=airIsInf_d(val[ai]))) {
      biffMaybeAddf(useBiff, NRRD, "%s: axis %d min %sinf invalid",
                    me, ai, 1==ret ? "+" : "-");
      return 1;
    }
  }
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  /* HEY: contemplate checking min != max, but what about stub axes ... */
  return 0;
}

static int
_nrrdFieldCheck_axis_maxs(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_axis_maxs";
  double val[NRRD_DIM_MAX];
  unsigned int ai;
  int ret;

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoMax, val);
  for (ai=0; ai<nrrd->dim; ai++) {
    if ((ret=airIsInf_d(val[ai]))) {
      biffMaybeAddf(useBiff, NRRD, "%s: axis %d max %sinf invalid",
                    me, ai, 1==ret ? "+" : "-");
      return 1;
    }
  }
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  /* HEY: contemplate checking min != max, but what about stub axes ... */
  return 0;
}

static int
_nrrdFieldCheck_space_directions(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_space_directions";

  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: space info problem", me);
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_centers(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_centers";
  unsigned int ai;
  int val[NRRD_DIM_MAX];

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoCenter, val);
  for (ai=0; ai<nrrd->dim; ai++) {
    if (!( nrrdCenterUnknown == val[ai]
           || !airEnumValCheck(nrrdCenter, val[ai]) )) {
      biffMaybeAddf(useBiff, NRRD, "%s: axis %d center %d invalid",
                    me, ai, val[ai]);
      return 1;
    }
  }
  return 0;
}

static int
_nrrdFieldCheck_kinds(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_kinds";
  int val[NRRD_DIM_MAX];
  unsigned int wantLen, ai;

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoKind, val);
  for (ai=0; ai<nrrd->dim; ai++) {
    if (!( nrrdKindUnknown == val[ai]
           || !airEnumValCheck(nrrdKind, val[ai]) )) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: axis %d kind %d invalid", me, ai, val[ai]);
      return 1;
    }
    wantLen = nrrdKindSize(val[ai]);
    if (wantLen && wantLen != nrrd->axis[ai].size) {
      char stmp[AIR_STRLEN_SMALL];
      biffMaybeAddf(useBiff, NRRD,
                    "%s: axis %d kind %s requires size %u, but have %s", me,
                    ai, airEnumStr(nrrdKind, val[ai]), wantLen,
                    airSprintSize_t(stmp, nrrd->axis[ai].size));
      return 1;
    }
  }
  return 0;
}

static int
_nrrdFieldCheck_labels(const Nrrd *nrrd, int useBiff) {
  /* char me[]="_nrrdFieldCheck_labels"; */

  AIR_UNUSED(nrrd);
  AIR_UNUSED(useBiff);

  /* don't think there's anything to do here: the label strings are
     either NULL (which is okay) or non-NULL, but we have no restrictions
     on the validity of the strings */

  return 0;
}

static int
_nrrdFieldCheck_units(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_units";

  /* as with labels- the strings themselves don't need checking themselves */
  /* but per-axis units cannot be set for axes with space directions ... */
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: space info problem", me);
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_old_min(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_old_min";
  int ret;

  if ((ret=airIsInf_d(nrrd->oldMin))) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: old min %sinf invalid", me, 1==ret ? "+" : "-");
    return 1;
  }
  /* oldMin == oldMax is perfectly valid */
  return 0;
}

static int
_nrrdFieldCheck_old_max(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_old_max";
  int ret;

  if ((ret=airIsInf_d(nrrd->oldMax))) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: old max %sinf invalid", me, 1==ret ? "+" : "-");
    return 1;
  }
  /* oldMin == oldMax is perfectly valid */
  return 0;
}

static int
_nrrdFieldCheck_keyvalue(const Nrrd *nrrd, int useBiff) {
  /* char me[]="_nrrdFieldCheck_keyvalue"; */

  AIR_UNUSED(nrrd);
  AIR_UNUSED(useBiff);

  /* nrrdKeyValueAdd() ensures that keys aren't repeated,
     not sure what other kind of checking can be done */

  return 0;
}

static int
_nrrdFieldCheck_space_units(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_space_units";

  /* not sure if there's anything to specifically check for the
     space units themselves ... */
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: space info problem", me);
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_space_origin(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_space_origin";

  /* pre-Fri Feb 11 04:25:36 EST 2005, I thought that
     the spaceOrigin must be known to describe the
     space/orientation stuff, but that's too restrictive,
     which is why below says AIR_FALSE instead of AIR_TRUE */
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: space info problem", me);
    return 1;
  }
  return 0;
}

static int
_nrrdFieldCheck_measurement_frame(const Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdFieldCheck_measurement_frame";

  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: space info problem", me);
    return 1;
  }
  return 0;
}

int
(*_nrrdFieldCheck[NRRD_FIELD_MAX+1])(const Nrrd *, int useBiff) = {
  _nrrdFieldCheck_noop,           /* nonfield */
  _nrrdFieldCheck_noop,           /* comment */
  _nrrdFieldCheck_noop,           /* content */
  _nrrdFieldCheck_noop,           /* number */
  _nrrdFieldCheck_type,
  _nrrdFieldCheck_block_size,
  _nrrdFieldCheck_dimension,
  _nrrdFieldCheck_space,
  _nrrdFieldCheck_space_dimension,
  _nrrdFieldCheck_sizes,
  _nrrdFieldCheck_spacings,
  _nrrdFieldCheck_thicknesses,
  _nrrdFieldCheck_axis_mins,
  _nrrdFieldCheck_axis_maxs,
  _nrrdFieldCheck_space_directions,
  _nrrdFieldCheck_centers,
  _nrrdFieldCheck_kinds,
  _nrrdFieldCheck_labels,
  _nrrdFieldCheck_units,
  _nrrdFieldCheck_noop,           /* min */
  _nrrdFieldCheck_noop,           /* max */
  _nrrdFieldCheck_old_min,
  _nrrdFieldCheck_old_max,
  _nrrdFieldCheck_noop,           /* endian */
  _nrrdFieldCheck_noop,           /* encoding */
  _nrrdFieldCheck_noop,           /* line_skip */
  _nrrdFieldCheck_noop,           /* byte_skip */
  _nrrdFieldCheck_keyvalue,
  _nrrdFieldCheck_noop,           /* sample units */
  _nrrdFieldCheck_space_units,
  _nrrdFieldCheck_space_origin,
  _nrrdFieldCheck_measurement_frame,
  _nrrdFieldCheck_noop,           /* data_file */
};

int
_nrrdCheck(const Nrrd *nrrd, int checkData, int useBiff) {
  static const char me[]="_nrrdCheck";
  int fi;

  if (!nrrd) {
    biffMaybeAddf(useBiff, NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (checkData) {
    if (!(nrrd->data)) {
      biffMaybeAddf(useBiff, NRRD, "%s: nrrd %p has NULL data pointer",
                    me, AIR_CVOIDP(nrrd));
      return 1;
    }
  }
  for (fi=nrrdField_unknown+1; fi<nrrdField_last; fi++) {
    /* yes, this will call _nrrdFieldCheckSpaceInfo() many many times */
    if (_nrrdFieldCheck[fi](nrrd, AIR_TRUE)) {
      biffMaybeAddf(useBiff, NRRD, "%s: trouble with %s field", me,
                    airEnumStr(nrrdField, fi));
      return 1;
    }
  }
  return 0;
}

/*
******** nrrdCheck()
**
** does some consistency checks for things that can go wrong in a nrrd
** returns non-zero if there is a problem, zero if no problem.
**
** You might think that this should be merged with _nrrdHeaderCheck(),
** but that is really only for testing sufficiency of information
** required to do the data reading.
*/
int /*Teem: biff if (ret) */
nrrdCheck(const Nrrd *nrrd) {
  static const char me[]="nrrdCheck";

  if (_nrrdCheck(nrrd, AIR_TRUE, AIR_TRUE)) {
    biffAddf(NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

/*
******** nrrdSameSize()
**
** returns 1 iff given two nrrds have same dimension and axes sizes.
** This does NOT look at the type of the elements.
**
** The intended user of this is someone who really wants the nrrds to be
** the same size, so that if they aren't, some descriptive (error) message
** can be generated according to useBiff
*/
int /*Teem: biff?2 if (!ret) */
nrrdSameSize(const Nrrd *n1, const Nrrd *n2, int useBiff) {
  static const char me[]="nrrdSameSize";
  unsigned int ai;
  char stmp[2][AIR_STRLEN_SMALL];

  if (!(n1 && n2)) {
    biffMaybeAddf(useBiff, NRRD, "%s: got NULL pointer", me);
    return 0;
  }
  if (n1->dim != n2->dim) {
    biffMaybeAddf(useBiff, NRRD, "%s: n1->dim (%u) != n2->dim (%u)",
                  me, n1->dim, n2->dim);
    return 0;
  }
  for (ai=0; ai<n1->dim; ai++) {
    if (n1->axis[ai].size != n2->axis[ai].size) {
      biffMaybeAddf(useBiff, NRRD, "%s: n1->axis[%d].size (%s) "
                    "!= n2->axis[%d].size (%s)", me, ai,
                    airSprintSize_t(stmp[0], n1->axis[ai].size), ai,
                    airSprintSize_t(stmp[1], n2->axis[ai].size));
      return 0;
    }
  }
  return 1;
}

/*
******** nrrdElementSize()
**
** So just how many bytes long is one element in this nrrd?  This is
** needed (over the simple nrrdTypeSize[] array) because some nrrds
** may be of "block" type, and because it does bounds checking on
** nrrd->type.  Returns 0 if given a bogus nrrd->type, or if the block
** size isn't greater than zero (in which case it sets nrrd->blockSize
** to 0, just out of spite).  This function never returns a negative
** value; using (!nrrdElementSize(nrrd)) is a sufficient check for
** invalidity.
**
** Besides learning how many bytes long one element is, this function
** is useful as a way of detecting an invalid blocksize on a block nrrd.
*/
size_t
nrrdElementSize (const Nrrd *nrrd) {

  if (!( nrrd && !airEnumValCheck(nrrdType, nrrd->type) )) {
    return 0;
  }
  if (nrrdTypeBlock != nrrd->type) {
    return nrrdTypeSize[nrrd->type];
  }
  /* else its block type */
  if (nrrd->blockSize > 0) {
    return nrrd->blockSize;
  }
  /* else we got an invalid block size */
  /* nrrd->blockSize = 0; */
  return 0;
}

/*
******** nrrdElementNumber()
**
** takes the place of old "nrrd->num": the number of elements in the
** nrrd, which is just the product of the axis sizes.  A return of 0
** means there's a problem.  Negative numbers are never returned.
**
** does NOT use biff
*/
size_t
nrrdElementNumber (const Nrrd *nrrd) {
  size_t num, size[NRRD_DIM_MAX];
  unsigned int ai;

  if (!nrrd) {
    return 0;
  }
  /* else */
  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoSize, size);
  if (_nrrdSizeCheck(size, nrrd->dim, AIR_FALSE)) {
    /* the nrrd's size information is invalid, can't proceed */
    return 0;
  }
  num = 1;
  for (ai=0; ai<nrrd->dim; ai++) {
    /* negative numbers and overflow were caught by _nrrdSizeCheck() */
    num *= size[ai];
  }
  return num;
}

/*
** obviously, this requires that the per-axis size fields have been set
*/
void
_nrrdSplitSizes(size_t *pieceSize, size_t *pieceNum, Nrrd *nrrd,
                unsigned int split) {
  unsigned int ai;
  size_t size[NRRD_DIM_MAX];

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoSize, size);
  *pieceSize = 1;
  for (ai=0; ai<split; ai++) {
    *pieceSize *= size[ai];
  }
  *pieceNum = 1;
  for (ai=split; ai<nrrd->dim; ai++) {
    *pieceNum *= size[ai];
  }
  return;
}

/*
******** nrrdHasNonExistSet()
**
** This function will always (assuming type is valid) set the value of
** nrrd->hasNonExist to either nrrdNonExistTrue or nrrdNonExistFalse,
** and it will return that value.  For lack of a more sophisticated
** policy, blocks are currently always considered to be existent
** values (because nrrdTypeIsIntegral[nrrdTypeBlock] is currently true).
** This function will ALWAYS determine the correct answer and set the
** value of nrrd->hasNonExist: it ignores the value of
** nrrd->hasNonExist on the input nrrd.  Exception: if nrrd is null or
** type is bogus, no action is taken and nrrdNonExistUnknown is
** returned.
**
** Because this will return either nrrdNonExistTrue or nrrdNonExistFalse,
** and because the C boolean value of these are true and false (respectively),
** it is possible (and encouraged) to use the return of this function
** as the expression of a conditional:
**
**   if (nrrdHasNonExistSet(nrrd)) {
**     ... handle existance of non-existent values ...
**   }
*/
/*
int
nrrdHasNonExistSet(Nrrd *nrrd) {
  size_t I, N;
  float val;

  if (!( nrrd && !airEnumValCheck(nrrdType, nrrd->type) ))
    return nrrdNonExistUnknown;

  if (nrrdTypeIsIntegral[nrrd->type]) {
    nrrd->hasNonExist = nrrdNonExistFalse;
  } else {
    nrrd->hasNonExist = nrrdNonExistFalse;
    N = nrrdElementNumber(nrrd);
    for (I=0; I<N; I++) {
      val = nrrdFLookup[nrrd->type](nrrd->data, I);
      if (!AIR_EXISTS(val)) {
        nrrd->hasNonExist = nrrdNonExistTrue;
        break;
      }
    }
  }
  return nrrd->hasNonExist;
}
*/

static int
_nrrdCheckEnums(void) {
  static const char me[]="_nrrdCheckEnums";
  char which[AIR_STRLEN_SMALL];

  if (nrrdFormatTypeLast-1 != NRRD_FORMAT_TYPE_MAX) {
    strcpy(which, "nrrdFormat"); goto err;
  }
  if (nrrdTypeLast-1 != NRRD_TYPE_MAX) {
    strcpy(which, "nrrdType"); goto err;
  }
  if (nrrdEncodingTypeLast-1 != NRRD_ENCODING_TYPE_MAX) {
    strcpy(which, "nrrdEncodingType"); goto err;
  }
  if (nrrdCenterLast-1 != NRRD_CENTER_MAX) {
    strcpy(which, "nrrdCenter"); goto err;
  }
  if (nrrdAxisInfoLast-1 != NRRD_AXIS_INFO_MAX) {
    strcpy(which, "nrrdAxisInfo"); goto err;
  }
  /* can't really check on endian enum */
  if (nrrdField_last-1 != NRRD_FIELD_MAX) {
    strcpy(which, "nrrdField"); goto err;
  }
  if (nrrdHasNonExistLast-1 != NRRD_HAS_NON_EXIST_MAX) {
    strcpy(which, "nrrdHasNonExist"); goto err;
  }

  /* no errors so far */
  return 0;

 err:
  biffAddf(NRRD, "%s: Last vs. MAX incompatibility for %s enum", me, which);
  return 1;
}

/*
****** nrrdSanity
**
** makes sure that all the basic assumptions of nrrd hold for
** the architecture/etc which we're currently running on.
**
** returns 1 if all is okay, 0 if there is a problem
*/
int /*Teem: biff if (!ret) */
nrrdSanity(void) {
  static const char me[]="nrrdSanity";
  int aret, type;
  size_t maxsize;
  airLLong tmpLLI;
  airULLong tmpULLI;
  static int _nrrdSanity = 0;

  if (_nrrdSanity) {
    /* we've been through this once before and things looked okay ... */
    /* Is this thread-safe?  I think so.  If we assume that any two
       threads are going to compute the same value, isn't it the case
       that, at worse, both of them will go through all the tests and
       then set _nrrdSanity to the same thing? */
    return 1;
  }

  aret = airSanity();
  if (aret != airInsane_not) {
    biffAddf(NRRD, "%s: airSanity() failed: %s", me,
             airInsaneErr(aret));
    return 0;
  }


  if (airEnumValCheck(nrrdEncodingType, nrrdDefaultWriteEncodingType)) {
    biffAddf(NRRD,
             "%s: nrrdDefaultWriteEncodingType (%d) not in valid "
             "range [%d,%d]", me, nrrdDefaultWriteEncodingType,
             nrrdEncodingTypeUnknown+1, nrrdEncodingTypeLast-1);
    return 0;
  }
  if (airEnumValCheck(nrrdCenter, nrrdDefaultCenter)) {
    biffAddf(NRRD,
             "%s: nrrdDefaultCenter (%d) not in valid range [%d,%d]",
             me, nrrdDefaultCenter,
             nrrdCenterUnknown+1, nrrdCenterLast-1);
    return 0;
  }

  if (!( nrrdTypeSize[nrrdTypeChar] == sizeof(char)
         && nrrdTypeSize[nrrdTypeUChar] == sizeof(unsigned char)
         && nrrdTypeSize[nrrdTypeShort] == sizeof(short)
         && nrrdTypeSize[nrrdTypeUShort] == sizeof(unsigned short)
         && nrrdTypeSize[nrrdTypeInt] == sizeof(int)
         && nrrdTypeSize[nrrdTypeUInt] == sizeof(unsigned int)
         && nrrdTypeSize[nrrdTypeLLong] == sizeof(airLLong)
         && nrrdTypeSize[nrrdTypeULLong] == sizeof(airULLong)
         && nrrdTypeSize[nrrdTypeFloat] == sizeof(float)
         && nrrdTypeSize[nrrdTypeDouble] == sizeof(double) )) {
    biffAddf(NRRD, "%s: sizeof() for nrrd types has problem: "
             "expected (%u,%u,%u,%u,%u,%u,%u,%u,%u,%u) "
             "but got (%u,%u,%u,%u,%u,%u,%u,%u,%u,%u)", me,
             AIR_CAST(unsigned int, nrrdTypeSize[nrrdTypeChar]),
             AIR_CAST(unsigned int, nrrdTypeSize[nrrdTypeUChar]),
             AIR_CAST(unsigned int, nrrdTypeSize[nrrdTypeShort]),
             AIR_CAST(unsigned int, nrrdTypeSize[nrrdTypeUShort]),
             AIR_CAST(unsigned int, nrrdTypeSize[nrrdTypeInt]),
             AIR_CAST(unsigned int, nrrdTypeSize[nrrdTypeUInt]),
             AIR_CAST(unsigned int, nrrdTypeSize[nrrdTypeLLong]),
             AIR_CAST(unsigned int, nrrdTypeSize[nrrdTypeULLong]),
             AIR_CAST(unsigned int, nrrdTypeSize[nrrdTypeFloat]),
             AIR_CAST(unsigned int, nrrdTypeSize[nrrdTypeDouble]),
             AIR_CAST(unsigned int, sizeof(char)),
             AIR_CAST(unsigned int, sizeof(unsigned char)),
             AIR_CAST(unsigned int, sizeof(short)),
             AIR_CAST(unsigned int, sizeof(unsigned short)),
             AIR_CAST(unsigned int, sizeof(int)),
             AIR_CAST(unsigned int, sizeof(unsigned int)),
             AIR_CAST(unsigned int, sizeof(airLLong)),
             AIR_CAST(unsigned int, sizeof(airULLong)),
             AIR_CAST(unsigned int, sizeof(float)),
             AIR_CAST(unsigned int, sizeof(double)));
    return 0;
  }

  /* check on NRRD_TYPE_SIZE_MAX */
  maxsize = 0;
  for (type=nrrdTypeUnknown+1; type<=nrrdTypeLast-2; type++) {
    maxsize = AIR_MAX(maxsize, nrrdTypeSize[type]);
  }
  if (maxsize != NRRD_TYPE_SIZE_MAX) {
    biffAddf(NRRD,
             "%s: actual max type size is %u != %u == NRRD_TYPE_SIZE_MAX",
             me, AIR_CAST(unsigned int, maxsize), NRRD_TYPE_SIZE_MAX);
    return 0;
  }

  /* check on NRRD_TYPE_BIGGEST */
  if (maxsize != sizeof(NRRD_TYPE_BIGGEST)) {
    biffAddf(NRRD, "%s: actual max type size is %u != "
             "%u == sizeof(NRRD_TYPE_BIGGEST)",
             me, AIR_CAST(unsigned int, maxsize),
             AIR_CAST(unsigned int, sizeof(NRRD_TYPE_BIGGEST)));
    return 0;
  }

  /* nrrd-defined min/max values for 64-bit integral types */
  /* NOTE: because signed integral overflow is undefined in C, the tests for
     signed long long no longer use overflow (and an assumption of two's
     complement representation) to assess the correctness of NRRD_LLONG_MAX
     and NRRD_LLONG_MIN.  We merely test that these values can be stored,
     which we do via indirect (perhaps needlessly so) means.
     (h/t Sean McBride for pointing this out) */
  tmpLLI = _nrrdLLongMaxHelp(_nrrdLLongMaxHelp(_NRRD_LLONG_MAX_HELP));
  if (!( tmpLLI > 0 && NRRD_LLONG_MAX == tmpLLI )) {
    biffAddf(NRRD, "%s: long long int can't hold NRRD_LLONG_MAX ("
             AIR_LLONG_FMT ")", me,
             NRRD_LLONG_MAX);
    return 0;
  }
  tmpLLI = _nrrdLLongMinHelp(_nrrdLLongMinHelp(_NRRD_LLONG_MIN_HELP));
  if (!( tmpLLI < 0 && NRRD_LLONG_MIN == tmpLLI )) {
    biffAddf(NRRD, "%s: long long int can't hold NRRD_LLONG_MIN ("
             AIR_LLONG_FMT ")", me,
             NRRD_LLONG_MIN);
    return 0;
  }
  tmpULLI = _nrrdULLongMaxHelp(NRRD_ULLONG_MAX);
  if (tmpULLI != 0) {
    biffAddf(NRRD,
             "%s: unsigned long long int max (" AIR_ULLONG_FMT
             ") incorrect", me, NRRD_ULLONG_MAX);
    return 0;
  }

  if (_nrrdCheckEnums()) {
    biffAddf(NRRD, "%s: problem with enum definition", me);
    return 0;
  }

  if (!( NRRD_DIM_MAX >= 3 )) {
    biffAddf(NRRD,
             "%s: NRRD_DIM_MAX == %u seems awfully small, doesn't it?",
             me, NRRD_DIM_MAX);
    return 0;
  }

  if (!nrrdTypeIsIntegral[nrrdTypeBlock]) {
    biffAddf(NRRD,
             "%s: nrrdTypeInteger[nrrdTypeBlock] is not true, things "
             "could get wacky", me);
    return 0;
  }

  /* HEY: any other assumptions built into Teem? */

  _nrrdSanity = 1;
  return 1;
}


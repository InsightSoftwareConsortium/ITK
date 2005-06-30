/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2005  Gordon Kindlmann
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

#include "teem32bit.h"
#include <limits.h>

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
int
nrrdSpaceDimension(int space) {
  char me[]="nrrdSpaceDimension";
  int ret;

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
    exit(1);
    break;
  }
  return ret;
}

/*
******** nrrdSpaceSet
**
** What to use to set space, when a value from nrrdSpace enum is known
*/
int
nrrdSpaceSet(Nrrd *nrrd, int space) {
  char me[]="nrrdSpaceSet", err[AIR_STRLEN_MED];
  
  if (!nrrd) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nrrdSpaceUnknown != space) {
    if (airEnumValCheck(nrrdSpace, space)) {
      sprintf(err, "%s: given space (%d) not valid", me, space);
      biffAdd(NRRD, err); return 1;
    }
  }
  nrrd->space = space;
  nrrd->spaceDim = nrrdSpaceDimension(space);
  return 0;
}

/*
******** nrrdSpaceDimensionSet
**
** What to use to set space, based on spaceDim alone (nrrd->space set to
** nrrdSpaceUnknown)
*/
int
nrrdSpaceDimensionSet(Nrrd *nrrd, int spaceDim) {
  char me[]="nrrdSpaceDimensionSet", err[AIR_STRLEN_MED];
  
  if (!nrrd) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (!( spaceDim > 0 )) {
    sprintf(err, "%s: given spaceDim (%d) not valid", me, spaceDim);
    biffAdd(NRRD, err); return 1;
  }
  nrrd->space = nrrdSpaceUnknown;
  nrrd->spaceDim = spaceDim;
  return 0;
}

/*
******** nrrdSpaceKnown
**
** boolean test to see if given nrrd is said to live in some surrounding space 
*/
int
nrrdSpaceKnown(const Nrrd *nrrd) {

  return (nrrd && nrrd->spaceDim > 0);
}

/*
******** nrrdSpaceGet
**
** retrieves the space and spaceDim from given nrrd.
*/
void
nrrdSpaceGet(const Nrrd *nrrd, int *space, int *spaceDim) {
  
  if (nrrd && space && spaceDim) {
    *space = nrrd->space;
    if (nrrdSpaceUnknown != *space) {
      *spaceDim = nrrd->spaceDim;
    } else {
      *spaceDim = 0;
    }
  }
  return;
}

/*
******** nrrdSpaceOriginGet
**
** retrieves the spaceOrigin (and spaceDim) from given nrrd
*/
void
nrrdSpaceOriginGet(const Nrrd *nrrd,
                   double vector[NRRD_SPACE_DIM_MAX]) {
  int sdi;

  if (nrrd && vector) {
    for (sdi=0; sdi<nrrd->spaceDim; sdi++) {
      vector[sdi] = nrrd->spaceOrigin[sdi];
    }
    for (sdi=nrrd->spaceDim; sdi<NRRD_SPACE_DIM_MAX; sdi++) {
      vector[sdi] = AIR_NAN;
    }
  }
  return;
}

/*
******** nrrdOriginCalculate3D
**
** makes an effort to calculate something like an "origin" (as in
** nrrd->spaceOrigin) from the per-axis min, max, or spacing, when
** there is no real space information.  Like the spaceOrigin, this
** location is supposed to be THE CENTER of the first sample.  To
** avoid making assumptions about the nrrd or the caller, a default
** sample centering (defaultCenter) has to be provided (use either
** nrrdCenterNode or nrrdCenterCell).  Also, the three axes (ax0, ax1,
** ax2) that are to be used for the origin calculation have to be
** given explicitly- this puts the burden of figuring out the
** semantics of nrrdKinds and such on the caller.
**
** The computed origin is put into the given vector (origin).  The return
** value takes on values from the nrrdOriginStatus* enum:
**
** nrrdOriginStatusUnknown:        invalid arguments (e.g. NULL pointer, or 
**                                 axis values out of range)
**
** nrrdOriginStatusDirection:      the chosen axes have spaceDirection set, 
**                                 which means caller should be instead using
**                                 nrrdSpaceOriginGet
**
** nrrdOriginStatusNoMin:          can't compute "origin" without axis->min
**
** nrrdOriginStatusNoMaxOrSpacing: can't compute origin without either
**                                 axis->max or axis->spacing
**
** nrrdOriginStatusOkay:           all is well
*/
int
nrrdOriginCalculate3D(const Nrrd *nrrd, int ax0, int ax1, int ax2,
                      int defaultCenter, double origin[3]) {
  const NrrdAxisInfo *axis[3];
  int ai, center, size;
  double min, spacing;

  if (!( nrrd 
         && AIR_IN_CL(0, ax0, nrrd->dim-1)
         && AIR_IN_CL(0, ax1, nrrd->dim-1)
         && AIR_IN_CL(0, ax2, nrrd->dim-1)
         && (nrrdCenterCell == defaultCenter
             || nrrdCenterNode == defaultCenter)
         && origin )) {
    if (origin) {
      origin[0] = origin[1] = origin[2] = AIR_NAN;
    }
    return nrrdOriginStatusUnknown;
  }

  axis[0] = nrrd->axis + ax0;
  axis[1] = nrrd->axis + ax1;
  axis[2] = nrrd->axis + ax2;
  if (nrrd->spaceDim > 0 
      && (AIR_EXISTS(axis[0]->spaceDirection[0])
          || AIR_EXISTS(axis[1]->spaceDirection[0])
          || AIR_EXISTS(axis[2]->spaceDirection[0]))) {
    origin[0] = origin[1] = origin[2] = AIR_NAN;
    return nrrdOriginStatusDirection;
  }

  if (!( AIR_EXISTS(axis[0]->min)
         && AIR_EXISTS(axis[1]->min)
         && AIR_EXISTS(axis[2]->min) )) {
    origin[0] = origin[1] = origin[2] = AIR_NAN;
    return nrrdOriginStatusNoMin;
  }

  if (!( (AIR_EXISTS(axis[0]->max) || AIR_EXISTS(axis[0]->spacing))
         && (AIR_EXISTS(axis[1]->max) || AIR_EXISTS(axis[1]->spacing))
         && (AIR_EXISTS(axis[2]->max) || AIR_EXISTS(axis[2]->spacing)) )) {
    origin[0] = origin[1] = origin[2] = AIR_NAN;
    return nrrdOriginStatusNoMaxOrSpacing;
  }

  for (ai=0; ai<3; ai++) {
    size = axis[ai]->size;
    min = axis[ai]->min;
    center = (nrrdCenterUnknown != axis[ai]->center
              ? axis[ai]->center
              : defaultCenter);
    spacing = (AIR_EXISTS(axis[ai]->spacing)
               ? axis[ai]->spacing
               : ((axis[ai]->max - min)
                  /(nrrdCenterCell == center ? size : size-1)));
    origin[ai] = min + (nrrdCenterCell == center ? spacing/2 : 0);
  }
  return nrrdOriginStatusOkay;
}


void
_nrrdSpaceVecScale(double out[NRRD_SPACE_DIM_MAX], 
                   double scl, const double vec[NRRD_SPACE_DIM_MAX]) {
  int ii;
  double v;
  
  for (ii=0; ii<NRRD_SPACE_DIM_MAX; ii++) {
    v = AIR_EXISTS(vec[ii]) ? vec[ii] : 0;
    out[ii] = scl*v;
  }
}

double
_nrrdSpaceVecNorm(int sdim, const double vec[NRRD_SPACE_DIM_MAX]) {
  int di;
  double nn;

  nn = 0;
  for (di=0; di<sdim; di++) {
    nn += vec[di]*vec[di];
  }
  return sqrt(nn);
}

void
_nrrdSpaceVecSetNaN(double vec[NRRD_SPACE_DIM_MAX]) {
  int di;

  for (di=0; di<NRRD_SPACE_DIM_MAX; di++) {
    vec[di] = AIR_NAN;
  }
  return;
}

/*
** _nrrdContentGet
**
** ALLOCATES a string for the content of a given nrrd
** panics and exits if allocation failed
*/
char *
_nrrdContentGet(const Nrrd *nin) {
  char me[]="_nrrdContentGet";
  char *ret;
  
  ret = ((nin && nin->content) ? 
         airStrdup(nin->content) : 
         airStrdup(nrrdStateUnknownContent));
  if (!ret) {
    fprintf(stderr, "%s: PANIC: content strdup failed!\n", me);
    exit(1);
  }
  return ret;
}

int
_nrrdContentSet_nva (Nrrd *nout, const char *func,
                     char *content, const char *format, va_list arg) {
  char me[]="_nrrdContentSet_nva", err[AIR_STRLEN_MED],
    *buff;

  buff = malloc(128*AIR_STRLEN_HUGE);
  if (!buff) {
    sprintf(err, "%s: couln't alloc buffer!", me);
    biffAdd(NRRD, err); return 1;
  }
  nout->content = airFree(nout->content);

  /* we are currently praying that this won't overflow the "buff" array */
  /* HEY: replace with vsnprintf or whatever when its available */
  vsprintf(buff, format, arg);

  nout->content = calloc(strlen("(,)")
                         + airStrlen(func)
                         + 1                      /* '(' */
                         + airStrlen(content)
                         + 1                      /* ',' */
                         + airStrlen(buff)
                         + 1                      /* ')' */
                         + 1, sizeof(char));      /* '\0' */
  if (!nout->content) {
    sprintf(err, "%s: couln't alloc output content!", me);
    biffAdd(NRRD, err); airFree(buff); return 1;
  }
  sprintf(nout->content, "%s(%s%s%s)", func, content,
          airStrlen(buff) ? "," : "", buff);
  airFree(buff);  /* no NULL assignment, else compile warnings */
  return 0;
}

int
_nrrdContentSet (Nrrd *nout, const char *func,
                 char *content, const char *format, ...) {
  char me[]="_nrrdContentSet", err[AIR_STRLEN_MED];
  va_list ap;
  
  va_start(ap, format);
  if (_nrrdContentSet_nva(nout, func, content, format, ap)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); free(content); return 1;
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
nrrdContentSet (Nrrd *nout, const char *func,
                const Nrrd *nin, const char *format, ...) {
  char me[]="nrrdContentSet", err[AIR_STRLEN_MED];
  va_list ap;
  char *content;
  
  if (!(nout && func && nin && format)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nrrdStateDisableContent) {
    /* we kill content always */
    nout->content = airFree(nout->content);
    return 0;
  }
  if (!nin->content && !nrrdStateAlwaysSetContent) {
    /* there's no input content, and we're not supposed to invent any
       content, so after freeing nout's content we're done */
    nout->content = airFree(nout->content);
    return 0;
  }
  /* we copy the input nrrd content first, before blowing away the
     output content, in case nout == nin */
  content = _nrrdContentGet(nin);
  va_start(ap, format);
  if (_nrrdContentSet_nva(nout, func, content, format, ap)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); va_end(ap); free(content); return 1;
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
nrrdDescribe (FILE *file, const Nrrd *nrrd) {
  int i;

  if (file && nrrd) {
    fprintf(file, "Nrrd at 0x%p:\n", (void*)nrrd);
    fprintf(file, "Data at 0x%p is " _AIR_SIZE_T_FMT
            " elements of type %s.\n",
            nrrd->data, nrrdElementNumber(nrrd), 
            airEnumStr(nrrdType, nrrd->type));
    if (nrrdTypeBlock == nrrd->type) 
      fprintf(file, "The blocks have size %d\n", nrrd->blockSize);
    if (airStrlen(nrrd->content))
      fprintf(file, "Content = \"%s\"\n", nrrd->content);
    fprintf(file, "%d-dimensional array, with axes:\n", nrrd->dim);
    for (i=0; i<nrrd->dim; i++) {
      if (airStrlen(nrrd->axis[i].label)) {
        fprintf(file, "%d: (\"%s\") ", i, nrrd->axis[i].label);
      } else {
        fprintf(file, "%d: ", i);
      }
      fprintf(file, "%s-centered, size=%d, ",
              airEnumStr(nrrdCenter, nrrd->axis[i].center),
              nrrd->axis[i].size);
      airSinglePrintf(file, NULL, "spacing=%lg, \n", nrrd->axis[i].spacing);
      airSinglePrintf(file, NULL, "thickness=%lg, \n",
                      nrrd->axis[i].thickness);
      airSinglePrintf(file, NULL, "    axis(Min,Max) = (%lg,",
                       nrrd->axis[i].min);
      airSinglePrintf(file, NULL, "%lg)\n", nrrd->axis[i].max);
      if (airStrlen(nrrd->axis[i].units)) {
        fprintf(file, "units=%s, \n", nrrd->axis[i].units);
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
      for (i=0; i<nrrd->cmtArr->len; i++) {
        fprintf(file, "%s\n", nrrd->cmt[i]);
      }
    }
    fprintf(file, "\n");
  }
}

/*
** asserts all the properties associated with orientation information
**
** The most important part of this is asserting the per-axis mutual 
** exclusion of min/max/spacing/units versus using spaceDirection.
*/
int
_nrrdFieldCheckSpaceInfo(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheckSpaceInfo", err[AIR_STRLEN_MED];
  int dd, ii, exists;

  if (!( !nrrd->space || !airEnumValCheck(nrrdSpace, nrrd->space) )) {
    sprintf(err, "%s: space %d invalid", me, nrrd->space);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  if (!AIR_IN_CL(0, nrrd->spaceDim, NRRD_SPACE_DIM_MAX)) {
    sprintf(err, "%s: space dimension %d is outside valid range "
            "[0,NRRD_SPACE_DIM_MAX] = [0,%d]",
            me, nrrd->dim, NRRD_SPACE_DIM_MAX);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  if (nrrd->spaceDim) {
    if (nrrd->space) {
      if (nrrdSpaceDimension(nrrd->space) != nrrd->spaceDim) {
        sprintf(err, "%s: space %s has dimension %d but spaceDim is %d", 
                me, airEnumStr(nrrdSpace, nrrd->space),
                nrrdSpaceDimension(nrrd->space), nrrd->spaceDim);
        biffMaybeAdd(NRRD, err, useBiff); return 1;
      }
    }
    /* check that all coeffs of spaceOrigin have consistent existance */
    exists = AIR_EXISTS(nrrd->spaceOrigin[0]);
    for (ii=0; ii<nrrd->spaceDim; ii++) {
      if (exists ^ AIR_EXISTS(nrrd->spaceOrigin[ii])) {
        sprintf(err, "%s: existance of space origin coefficients must "
                "be consistent (val[0] not like val[%d])", me, ii);
        biffMaybeAdd(NRRD, err, useBiff); return 1;
      }
    }
    /* check that all coeffs of measurementFrame have consistent existance */
    exists = AIR_EXISTS(nrrd->measurementFrame[0][0]);
    for (dd=0; dd<nrrd->spaceDim; dd++) {
      for (ii=0; ii<nrrd->spaceDim; ii++) {
        if (exists ^ AIR_EXISTS(nrrd->measurementFrame[dd][ii])) {
          sprintf(err, "%s: existance of measurement frame coefficients must "
                  "be consistent: [col][row] [%d][%d] not like [0][0])",
                  me, dd, ii);
          biffMaybeAdd(NRRD, err, useBiff); return 1;
        }
      }
    }
    /* check on space directions */
    for (dd=0; dd<nrrd->dim; dd++) {
      exists = AIR_EXISTS(nrrd->axis[dd].spaceDirection[0]);
      for (ii=1; ii<nrrd->spaceDim; ii++) {
        if (exists ^ AIR_EXISTS(nrrd->axis[dd].spaceDirection[ii])) {
          sprintf(err, "%s: existance of space direction %d coefficients "
                  "must be consistent (val[0] not like val[%d])", me,
                  dd, ii);
          biffMaybeAdd(NRRD, err, useBiff); return 1;
        }
      }
      if (exists) {
        if (AIR_EXISTS(nrrd->axis[dd].min)
            || AIR_EXISTS(nrrd->axis[dd].max)
            || AIR_EXISTS(nrrd->axis[dd].spacing)
            || airStrlen(nrrd->axis[dd].units)) {
          sprintf(err, "%s: axis[%d] has a direction vector, and so can't "
                  "have min, max, spacing, or units set", me, dd);
          biffMaybeAdd(NRRD, err, useBiff); return 1;
        }
      }
    }
  } else {
    /* else there's not supposed to be anything in "space" */
    if (nrrd->space) {
      sprintf(err, "%s: space %s can't be set with spaceDim %d", 
              me, airEnumStr(nrrdSpace, nrrd->space), nrrd->spaceDim);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
    /* -------- */
    exists = AIR_FALSE;
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      exists |= airStrlen(nrrd->spaceUnits[dd]);
    }
    if (exists) {
      sprintf(err, "%s: spaceDim is 0, but space units is set", me);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
    /* -------- */
    exists = AIR_FALSE;
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      exists |= AIR_EXISTS(nrrd->spaceOrigin[dd]);
    }
    if (exists) {
      sprintf(err, "%s: spaceDim is 0, but space origin is set", me);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
    /* -------- */
    exists = AIR_FALSE;
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      for (ii=0; ii<NRRD_DIM_MAX; ii++) {
        exists |= AIR_EXISTS(nrrd->axis[ii].spaceDirection[dd]);
      }
    }
    if (exists) {
      sprintf(err, "%s: spaceDim is 0, but space directions are set", me);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
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

int
_nrrdFieldCheck_noop(const Nrrd *nrrd, int useBiff) {

  return 0;
}

int
_nrrdFieldCheck_type(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_type", err[AIR_STRLEN_MED];
  
  if (airEnumValCheck(nrrdType, nrrd->type)) {
    sprintf(err, "%s: type (%d) is not valid", me, nrrd->type);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_block_size(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_block_size", err[AIR_STRLEN_MED];
  
  if (nrrdTypeBlock == nrrd->type && (!(0 < nrrd->blockSize)) ) {
    sprintf(err, "%s: type is %s but nrrd->blockSize (%d) invalid", me,
            airEnumStr(nrrdType, nrrdTypeBlock),
            nrrd->blockSize);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  if (nrrdTypeBlock != nrrd->type && (0 < nrrd->blockSize)) {
    sprintf(err, "%s: type is %s (not block) but blockSize is %d", me,
            airEnumStr(nrrdType, nrrd->type), nrrd->blockSize);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_dimension(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_dimension", err[AIR_STRLEN_MED];
  
  if (!AIR_IN_CL(1, nrrd->dim, NRRD_DIM_MAX)) {
    sprintf(err, "%s: dimension %d is outside valid range [1,%d]",
            me, nrrd->dim, NRRD_DIM_MAX);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_space(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_space", err[AIR_STRLEN_MED];

  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    sprintf(err, "%s: trouble", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_space_dimension(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_space_dimension", err[AIR_STRLEN_MED];
  
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    sprintf(err, "%s: trouble", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_sizes(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_sizes", err[AIR_STRLEN_MED];
  int size[NRRD_DIM_MAX];
  
  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoSize, size);
  if (_nrrdSizeCheck(nrrd->dim, size, useBiff)) {
    sprintf(err, "%s: trouble with array sizes", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_spacings(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_spacings", err[AIR_STRLEN_MED];
  double val[NRRD_DIM_MAX];
  int i;

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoSpacing, val);
  for (i=0; i<nrrd->dim; i++) {
    if (!( !airIsInf_d(val[i]) && (airIsNaN(val[i]) || (0 != val[i])) )) {
      sprintf(err, "%s: axis %d spacing (%g) invalid", me, i, val[i]);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
  }
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    sprintf(err, "%s: trouble", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_thicknesses(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_thicknesses", err[AIR_STRLEN_MED];
  double val[NRRD_DIM_MAX];
  int i;

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoThickness, val);
  for (i=0; i<nrrd->dim; i++) {
    /* note that unlike spacing, we allow zero thickness, 
       but it makes no sense to be negative */
    if (!( !airIsInf_d(val[i]) && (airIsNaN(val[i]) || (0 <= val[i])) )) {
      sprintf(err, "%s: axis %d thickness (%g) invalid", me, i, val[i]);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
  }
  return 0;
}

int
_nrrdFieldCheck_axis_mins(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_axis_mins", err[AIR_STRLEN_MED];
  double val[NRRD_DIM_MAX];
  int i, ret;

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoMin, val);
  for (i=0; i<nrrd->dim; i++) {
    if ((ret=airIsInf_d(val[i]))) {
      sprintf(err, "%s: axis %d min %sinf invalid", me, i, 1==ret ? "+" : "-");
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
  }
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    sprintf(err, "%s: trouble", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  /* HEY: contemplate checking min != max, but what about stub axes ... */
  return 0;
}

int
_nrrdFieldCheck_axis_maxs(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_axis_maxs", err[AIR_STRLEN_MED];
  double val[NRRD_DIM_MAX];
  int i, ret;

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoMax, val);
  for (i=0; i<nrrd->dim; i++) {
    if ((ret=airIsInf_d(val[i]))) {
      sprintf(err, "%s: axis %d max %sinf invalid", me, i, 1==ret ? "+" : "-");
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
  }
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    sprintf(err, "%s: trouble", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  /* HEY: contemplate checking min != max, but what about stub axes ... */
  return 0;
}

int
_nrrdFieldCheck_space_directions(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_space_directions", err[AIR_STRLEN_MED];

  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    sprintf(err, "%s: space info problem", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_centers(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_centers", err[AIR_STRLEN_MED];
  int i, val[NRRD_DIM_MAX];

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoCenter, val);
  for (i=0; i<nrrd->dim; i++) {
    if (!( nrrdCenterUnknown == val[i]
           || !airEnumValCheck(nrrdCenter, val[i]) )) {
      sprintf(err, "%s: axis %d center %d invalid", me, i, val[i]);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
  }
  return 0;
}

int
_nrrdFieldCheck_kinds(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_kinds", err[AIR_STRLEN_MED];
  int i, wantLen, val[NRRD_DIM_MAX];

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoKind, val);
  for (i=0; i<nrrd->dim; i++) {
    if (!( nrrdKindUnknown == val[i]
           || !airEnumValCheck(nrrdKind, val[i]) )) {
      sprintf(err, "%s: axis %d kind %d invalid", me, i, val[i]);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
    wantLen = nrrdKindSize(val[i]);
    if (wantLen && wantLen != nrrd->axis[i].size) {
      sprintf(err, "%s: axis %d kind %s requires size %d, but have %d", me,
              i, airEnumStr(nrrdKind, val[i]), wantLen, nrrd->axis[i].size);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
  }
  return 0;
}

int
_nrrdFieldCheck_labels(const Nrrd *nrrd, int useBiff) {
  /* char me[]="_nrrdFieldCheck_labels", err[AIR_STRLEN_MED]; */

  /* don't think there's anything to do here: the label strings are
     either NULL (which is okay) or non-NULL, but we have no restrictions
     on the validity of the strings */

  return 0;
}

int
_nrrdFieldCheck_units(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_units", err[AIR_STRLEN_MED];

  /* as with labels- the strings themselves don't need checking themselves */
  /* but per-axis units cannot be set for axes with space directions ... */
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    sprintf(err, "%s: space info problem", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_old_min(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_old_min", err[AIR_STRLEN_MED];
  int ret;

  if ((ret=airIsInf_d(nrrd->oldMin))) {
    sprintf(err, "%s: old min %sinf invalid", me, 1==ret ? "+" : "-");
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  /* oldMin == oldMax is perfectly valid */
  return 0;
}

int
_nrrdFieldCheck_old_max(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_old_max", err[AIR_STRLEN_MED];
  int ret;

  if ((ret=airIsInf_d(nrrd->oldMax))) {
    sprintf(err, "%s: old max %sinf invalid", me, 1==ret ? "+" : "-");
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  /* oldMin == oldMax is perfectly valid */
  return 0;
}

int
_nrrdFieldCheck_keyvalue(const Nrrd *nrrd, int useBiff) {
  /* char me[]="_nrrdFieldCheck_keyvalue", err[AIR_STRLEN_MED]; */

  /* nrrdKeyValueAdd() ensures that keys aren't repeated, 
     not sure what other kind of checking can be done */

  return 0;
}

int
_nrrdFieldCheck_space_units(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_space_units", err[AIR_STRLEN_MED];

  /* not sure if there's anything to specifically check for the
     space units themselves ... */
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    sprintf(err, "%s: space info problem", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_space_origin(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_space_origin", err[AIR_STRLEN_MED];

  /* pre-Fri Feb 11 04:25:36 EST 2005, I thought that 
     the spaceOrigin must be known to describe the 
     space/orientation stuff, but that's too restrictive,
     which is why below says AIR_FALSE instead of AIR_TRUE */
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    sprintf(err, "%s: space info problem", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  return 0;
}

int
_nrrdFieldCheck_measurement_frame(const Nrrd *nrrd, int useBiff) {
  char me[]="_nrrdFieldCheck_measurement_frame", err[AIR_STRLEN_MED];
  
  if (_nrrdFieldCheckSpaceInfo(nrrd, useBiff)) {
    sprintf(err, "%s: space info problem", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
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
_nrrdCheck (const Nrrd *nrrd, int checkData, int useBiff) {
  char me[]="_nrrdCheck", err[AIR_STRLEN_MED];
  int fi;

  if (!nrrd) {
    sprintf(err, "%s: got NULL pointer", me);
    biffMaybeAdd(NRRD, err, useBiff); return 1;
  }
  if (checkData) {
    if (!(nrrd->data)) {
      sprintf(err, "%s: nrrd has NULL data pointer", me);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
  }
  for (fi=nrrdField_unknown+1; fi<nrrdField_last; fi++) {
    /* yes, this will call _nrrdFieldCheckSpaceInfo() many many times */
    if (_nrrdFieldCheck[fi](nrrd, AIR_TRUE)) {
      sprintf(err, "%s: trouble with %s field", me,
              airEnumStr(nrrdField, fi));
      biffMaybeAdd(NRRD, err, useBiff); return 1;
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
int
nrrdCheck (const Nrrd *nrrd) {
  char me[]="nrrdCheck", err[AIR_STRLEN_MED];

  if (_nrrdCheck(nrrd, AIR_TRUE, AIR_TRUE)) {
    sprintf(err, "%s: trouble", me);
    biffAdd(NRRD, err); return 1;
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
int
nrrdSameSize (const Nrrd *n1, const Nrrd *n2, int useBiff) {
  char me[]="nrrdSameSize", err[AIR_STRLEN_MED];
  int i;

  if (!(n1 && n2)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffMaybeAdd(NRRD, err, useBiff); 
    return 0;
  }
  if (n1->dim != n2->dim) {
    sprintf(err, "%s: n1->dim (%d) != n2->dim (%d)", me, n1->dim, n2->dim);
    biffMaybeAdd(NRRD, err, useBiff); 
    return 0;
  }
  for (i=0; i<n1->dim; i++) {
    if (n1->axis[i].size != n2->axis[i].size) {
      sprintf(err, "%s: n1->axis[%d].size (%d) != n2->axis[%d].size (%d)", 
              me, i, n1->axis[i].size, i, n2->axis[i].size);
      biffMaybeAdd(NRRD, err, useBiff); 
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
int
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
  size_t num;
  int d, size[NRRD_DIM_MAX];

  if (!nrrd) {
    return 0;
  }
  /* else */
  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoSize, size);
  if (_nrrdSizeCheck(nrrd->dim, size, AIR_FALSE)) {
    /* the nrrd's size information is invalid, can't proceed */
    return 0;
  }
  num = 1;
  for (d=0; d<nrrd->dim; d++) {
    /* negative numbers and overflow were caught by _nrrdSizeCheck() */
    num *= size[d];
  }
  return num;
}

void
_nrrdSplitSizes(size_t *pieceSize, size_t *pieceNum, Nrrd *nrrd, int split) {
  int dd, size[NRRD_DIM_MAX];

  nrrdAxisInfoGet_nva(nrrd, nrrdAxisInfoSize, size);
  *pieceSize = 1;
  for (dd=0; dd<split; dd++) {
    *pieceSize *= size[dd];
  }
  *pieceNum = 1;
  for (dd=split; dd<nrrd->dim; dd++) {
    *pieceNum *= size[dd];
  }
  return;
}

/*
******** nrrdHasNonExistSet()
**
** This function will always (assuming type is valid) set the value of
** nrrd->hasNonExist to either nrrdNonExistTrue or nrrdNonExistFalse,
** and it will return that value.  For lack of a more sophisticated
** policy, blocks are currently always considered to be existant
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
**     ... handle existance of non-existant values ...
**   }
*/
/*
int
nrrdHasNonExistSet (Nrrd *nrrd) {
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

int
_nrrdCheckEnums (void) {
  char me[]="_nrrdCheckEnums", err[AIR_STRLEN_MED],
    which[AIR_STRLEN_SMALL];

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
  sprintf(err, "%s: Last vs. MAX incompatibility for %s enum", me, which);
  biffAdd(NRRD, err); return 1;
}

/*
******** nrrdSanity()
**
** makes sure that all the basic assumptions of nrrd hold for
** the architecture/etc which we're currently running on.  
** 
** returns 1 if all is okay, 0 if there is a problem
*/
int
nrrdSanity (void) {
  char me[]="nrrdSanity", err[AIR_STRLEN_MED];
  int aret, type, maxsize;
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
    sprintf(err, "%s: airSanity() failed: %s", me, airInsaneErr(aret));
    biffAdd(NRRD, err); return 0;
  }

  if (!nrrdDefWriteEncoding) {
    sprintf(err, "%s: nrrdDefWriteEncoding is NULL", me);
    biffAdd(NRRD, err); return 0;
  }
  if (airEnumValCheck(nrrdCenter, nrrdDefCenter)) {
    sprintf(err, "%s: nrrdDefCenter (%d) not in valid range [%d,%d]",
            me, nrrdDefCenter,
            nrrdCenterUnknown+1, nrrdCenterLast-1);
    biffAdd(NRRD, err); return 0;
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
    sprintf(err, "%s: sizeof() for nrrd types has problem: "
            "expected (%d,%d,%d,%d,%d,%d,%d,%d,%d,%d) "
            "but got (%d,%d,%d,%d,%d,%d,%d,%d,%d,%d)", me,
            nrrdTypeSize[nrrdTypeChar],
            nrrdTypeSize[nrrdTypeUChar],
            nrrdTypeSize[nrrdTypeShort],
            nrrdTypeSize[nrrdTypeUShort],
            nrrdTypeSize[nrrdTypeInt],
            nrrdTypeSize[nrrdTypeUInt],
            nrrdTypeSize[nrrdTypeLLong],
            nrrdTypeSize[nrrdTypeULLong],
            nrrdTypeSize[nrrdTypeFloat],
            nrrdTypeSize[nrrdTypeDouble],
            (int)sizeof(char),
            (int)sizeof(unsigned char),
            (int)sizeof(short),
            (int)sizeof(unsigned short),
            (int)sizeof(int),
            (int)sizeof(unsigned int),
            (int)sizeof(airLLong),
            (int)sizeof(airULLong),
            (int)sizeof(float),
            (int)sizeof(double));
    biffAdd(NRRD, err); return 0;
  }

  /* check on NRRD_TYPE_SIZE_MAX */
  maxsize = 0;
  for (type=nrrdTypeUnknown+1; type<=nrrdTypeLast-2; type++) {
    maxsize = AIR_MAX(maxsize, nrrdTypeSize[type]);
  }
  if (maxsize != NRRD_TYPE_SIZE_MAX) {
    sprintf(err, "%s: actual max type size is %d != %d == NRRD_TYPE_SIZE_MAX",
            me, maxsize, NRRD_TYPE_SIZE_MAX);
    biffAdd(NRRD, err); return 0;
  }

  /* check on NRRD_TYPE_BIGGEST */
  if (maxsize != sizeof(NRRD_TYPE_BIGGEST)) {
    sprintf(err, "%s: actual max type size is %d != "
            "%d == sizeof(NRRD_TYPE_BIGGEST)",
            me, maxsize, (int)sizeof(NRRD_TYPE_BIGGEST));
    biffAdd(NRRD, err); return 0;
  }
  
  /* nrrd-defined type min/max values */
  tmpLLI = NRRD_LLONG_MAX;
  if (tmpLLI != NRRD_LLONG_MAX) {
    sprintf(err, "%s: long long int can't hold NRRD_LLONG_MAX ("
            AIR_ULLONG_FMT ")", me,
            NRRD_LLONG_MAX);
    biffAdd(NRRD, err); return 0;
  }
  tmpLLI += 1;
  if (NRRD_LLONG_MIN != tmpLLI) {
    sprintf(err, "%s: long long int min (" AIR_LLONG_FMT ") or max ("
            AIR_LLONG_FMT ") incorrect", me,
            NRRD_LLONG_MIN, NRRD_LLONG_MAX);
    biffAdd(NRRD, err); return 0;
  }
  tmpULLI = NRRD_ULLONG_MAX;
  if (tmpULLI != NRRD_ULLONG_MAX) {
    sprintf(err, 
            "%s: unsigned long long int can't hold NRRD_ULLONG_MAX ("
            AIR_ULLONG_FMT ")",
            me, NRRD_ULLONG_MAX);
    biffAdd(NRRD, err); return 0;
  }
  tmpULLI += 1;
  if (tmpULLI != 0) {
    sprintf(err, "%s: unsigned long long int max (" AIR_ULLONG_FMT 
            ") incorrect", me,
            NRRD_ULLONG_MAX);
    biffAdd(NRRD, err); return 0;
  }

  if (_nrrdCheckEnums()) {
    sprintf(err, "%s: problem with enum definition", me);
    biffAdd(NRRD, err); return 0;
  }
  
  if (!( NRRD_DIM_MAX >= 3 )) {
    sprintf(err, "%s: NRRD_DIM_MAX == %d seems awfully small, doesn't it?",
            me, NRRD_DIM_MAX);
    biffAdd(NRRD, err); return 0;
  }

  if (!nrrdTypeIsIntegral[nrrdTypeBlock]) {
    sprintf(err, "%s: nrrdTypeInteger[nrrdTypeBlock] is not true, things "
            "could get wacky", me);
    biffAdd(NRRD, err); return 0;
  }

  /* HEY: any other assumptions built into teem? */

  _nrrdSanity = 1;
  return 1;
}

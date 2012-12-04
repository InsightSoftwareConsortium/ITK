/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2012, 2011, 2010, 2009  University of Chicago
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

/* ------------------------------------------------------------ */

void
_nrrdAxisInfoInit(NrrdAxisInfo *axis) {
  int dd;

  if (axis) {
    axis->size = 0;
    axis->spacing = axis->thickness = AIR_NAN;
    axis->min = axis->max = AIR_NAN;
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      axis->spaceDirection[dd] = AIR_NAN;
    }
    axis->center = nrrdCenterUnknown;
    axis->kind = nrrdKindUnknown;
    axis->label = (char *)airFree(axis->label);
    axis->units = (char *)airFree(axis->units);
  }
}

void
_nrrdAxisInfoNewInit(NrrdAxisInfo *axis) {

  if (axis) {
    axis->label = NULL;
    axis->units = NULL;
    _nrrdAxisInfoInit(axis);
  }
}

/* ------------------------------------------------------------ */

/*
******** nrrdKindIsDomain
**
** returns non-zero for kinds (from nrrdKind* enum) that are domain
** axes, or independent variable axes, or resample-able axes, all
** different ways of describing the same thing
*/
int
nrrdKindIsDomain(int kind) {

  return (nrrdKindDomain == kind
          || nrrdKindSpace == kind
          || nrrdKindTime == kind);
}

/*
******** nrrdKindSize
**
** returns suggested size (length) of an axis with the given kind, or,
** 0 if either (1) there is no suggested size because the axis is the
** kind of an independent or domain variable or (2) the kind is invalid
*/
unsigned int
nrrdKindSize(int kind) {
  static const char me[]="nrrdKindSize";
  unsigned int ret;

  if (!( AIR_IN_OP(nrrdKindUnknown, kind, nrrdKindLast) )) {
    /* they gave us invalid or unknown kind */
    return 0;
  }

  switch (kind) {
  case nrrdKindDomain:
  case nrrdKindSpace:
  case nrrdKindTime:
  case nrrdKindList:
  case nrrdKindPoint:
  case nrrdKindVector:
  case nrrdKindCovariantVector:
  case nrrdKindNormal:
    ret = 0;
    break;
  case nrrdKindStub:
  case nrrdKindScalar:
    ret = 1;
    break;
  case nrrdKindComplex:
  case nrrdKind2Vector:
    ret = 2;
    break;
  case nrrdKind3Color:
  case nrrdKindRGBColor:
  case nrrdKindHSVColor:
  case nrrdKindXYZColor:
    ret = 3;
    break;
  case nrrdKind4Color:
  case nrrdKindRGBAColor:
    ret = 4;
    break;
  case nrrdKind3Vector:
  case nrrdKind3Normal:
    ret = 3;
    break;
  case nrrdKind4Vector:
  case nrrdKindQuaternion:
    ret = 4;
    break;
  case nrrdKind2DSymMatrix:
    ret = 3;
    break;
  case nrrdKind2DMaskedSymMatrix:
    ret = 4;
    break;
  case nrrdKind2DMatrix:
    ret = 4;
    break;
  case nrrdKind2DMaskedMatrix:
    ret = 5;
    break;
  case nrrdKind3DSymMatrix:
    ret = 6;
    break;
  case nrrdKind3DMaskedSymMatrix:
    ret = 7;
    break;
  case nrrdKind3DMatrix:
    ret = 9;
    break;
  case nrrdKind3DMaskedMatrix:
    ret = 10;
    break;
  default:
    fprintf(stderr, "%s: PANIC: nrrdKind %d not implemented!\n", me, kind);
    ret = UINT_MAX;
  }

  return ret;
}

/*
** _nrrdKindAltered:
**
** implements logic for how kind should be updated when samples
** along the axis are altered
*/
int
_nrrdKindAltered(int kindIn, int resampling) {
  int kindOut;

  if (nrrdStateKindNoop) {
    kindOut = nrrdKindUnknown;
    /* HEY: setting the kindOut to unknown is arguably not a no-op.
       It is more like pointedly and stubbornly simplistic. So maybe
       nrrdStateKindNoop could be renamed .. */
  } else {
    if (nrrdKindIsDomain(kindIn)
        || (0 == nrrdKindSize(kindIn) && !resampling)) {
      kindOut = kindIn;
    } else {
      kindOut = nrrdKindUnknown;
    }
  }
  return kindOut;
}

/*
** _nrrdAxisInfoCopy
**
** HEY: we have a void return even though this function potentially
** involves calling airStrdup!!
*/
void
_nrrdAxisInfoCopy(NrrdAxisInfo *dest, const NrrdAxisInfo *src, int bitflag) {
  int ii;

  if (!(NRRD_AXIS_INFO_SIZE_BIT & bitflag)) {
    dest->size = src->size;
  }
  if (!(NRRD_AXIS_INFO_SPACING_BIT & bitflag)) {
    dest->spacing = src->spacing;
  }
  if (!(NRRD_AXIS_INFO_THICKNESS_BIT & bitflag)) {
    dest->thickness = src->thickness;
  }
  if (!(NRRD_AXIS_INFO_MIN_BIT & bitflag)) {
    dest->min = src->min;
  }
  if (!(NRRD_AXIS_INFO_MAX_BIT & bitflag)) {
    dest->max = src->max;
  }
  if (!(NRRD_AXIS_INFO_SPACEDIRECTION_BIT & bitflag)) {
    for (ii=0; ii<NRRD_SPACE_DIM_MAX; ii++) {
      dest->spaceDirection[ii] = src->spaceDirection[ii];
    }
  }
  if (!(NRRD_AXIS_INFO_CENTER_BIT & bitflag)) {
    dest->center = src->center;
  }
  if (!(NRRD_AXIS_INFO_KIND_BIT & bitflag)) {
    dest->kind = src->kind;
  }
  if (!(NRRD_AXIS_INFO_LABEL_BIT & bitflag)) {
    if (dest->label != src->label) {
      dest->label = (char *)airFree(dest->label);
      dest->label = (char *)airStrdup(src->label);
    }
  }
  if (!(NRRD_AXIS_INFO_UNITS_BIT & bitflag)) {
    if (dest->units != src->units) {
      dest->units = (char *)airFree(dest->units);
      dest->units = (char *)airStrdup(src->units);
    }
  }

  return;
}

/*
******** nrrdAxisInfoCopy()
**
** For copying all the per-axis peripheral information.  Takes a
** permutation "map"; map[d] tells from which axis in input should the
** output axis d copy its information.  The length of this permutation
** array is nout->dim.  If map is NULL, the identity permutation is
** assumed.  If map[i]==-1 for any i in [0,dim-1], then nothing is
** copied into axis i of output.  The "bitflag" field controls which
** per-axis fields will NOT be copied; if bitflag==0, then all fields
** are copied.  The value of bitflag should be |'s of NRRD_AXIS_INFO_*
** defines.
**
** Decided to Not use Biff, since many times map will be NULL, in
** which case the only error is getting a NULL nrrd, or an invalid map
** permutation, which will probably be unlikely given the contexts in
** which this is called.  For the paranoid, the integer return value
** indicates error.
**
** Sun Feb 27 21:12:57 EST 2005: decided to allow nout==nin, so now
** use a local array of NrrdAxisInfo as buffer.
*/
int
nrrdAxisInfoCopy(Nrrd *nout, const Nrrd *nin, const int *axmap, int bitflag) {
  NrrdAxisInfo axisBuffer[NRRD_DIM_MAX];
  const NrrdAxisInfo *axis;
  unsigned int from, axi;

  if (!(nout && nin)) {
    return 1;
  }
  if (axmap) {
    for (axi=0; axi<nout->dim; axi++) {
      if (-1 == axmap[axi]) {
        continue;
      }
      if (!AIR_IN_CL(0, axmap[axi], (int)nin->dim-1)) {
        return 3;
      }
    }
  }
  if (nout == nin) {
    /* copy axis info to local buffer */
    for (axi=0; axi<nin->dim; axi++) {
      _nrrdAxisInfoNewInit(axisBuffer + axi);
      _nrrdAxisInfoCopy(axisBuffer + axi, nin->axis + axi, bitflag);
    }
    axis = axisBuffer;
  } else {
    axis = nin->axis;
  }
  for (axi=0; axi<nout->dim; axi++) {
    if (axmap && -1 == axmap[axi]) {
      /* for this axis, we don't touch a thing */
      continue;
    }
    from = axmap ? (unsigned int)axmap[axi] : axi;
    _nrrdAxisInfoCopy(nout->axis + axi, axis + from, bitflag);
  }
  if (nout == nin) {
    /* free dynamically allocated stuff */
    for (axi=0; axi<nin->dim; axi++) {
      _nrrdAxisInfoInit(axisBuffer + axi);
    }
  }
  return 0;
}

/*
******** nrrdAxisInfoSet_nva()
**
** Simple means of setting fields of the axis array in the nrrd.
**
** type to pass for third argument:
**           nrrdAxisInfoSize: size_t*
**        nrrdAxisInfoSpacing: double*
**      nrrdAxisInfoThickness: double*
**            nrrdAxisInfoMin: double*
**            nrrdAxisInfoMax: double*
** nrrdAxisInfoSpaceDirection: double (*var)[NRRD_SPACE_DIM_MAX]
**         nrrdAxisInfoCenter: int*
**           nrrdAxisInfoKind: int*
**          nrrdAxisInfoLabel: char**
**          nrrdAxisInfoUnits: char**
**
** Note that in the case of nrrdAxisInfoSpaceDirection, we only access
** spaceDim elements of info.V[ai] (so caller can allocate it for less
** than NRRD_SPACE_DIM_MAX if they know what they're doing)
*/
void
nrrdAxisInfoSet_nva(Nrrd *nrrd, int axInfo, const void *_info) {
  _nrrdAxisInfoSetPtrs info;
  int exists;
  unsigned int ai, si, minsi;

  if (!( nrrd
         && AIR_IN_CL(1, nrrd->dim, NRRD_DIM_MAX)
         && AIR_IN_OP(nrrdAxisInfoUnknown, axInfo, nrrdAxisInfoLast)
         && _info )) {
    return;
  }
  info.P = _info;

  for (ai=0; ai<nrrd->dim; ai++) {
    switch (axInfo) {
    case nrrdAxisInfoSize:
      nrrd->axis[ai].size = info.ST[ai];
      break;
    case nrrdAxisInfoSpacing:
      nrrd->axis[ai].spacing = info.D[ai];
      break;
    case nrrdAxisInfoThickness:
      nrrd->axis[ai].thickness = info.D[ai];
      break;
    case nrrdAxisInfoMin:
      nrrd->axis[ai].min = info.D[ai];
      break;
    case nrrdAxisInfoMax:
      nrrd->axis[ai].max = info.D[ai];
      break;
    case nrrdAxisInfoSpaceDirection:
      /* we won't allow setting an invalid direction */
      exists = AIR_EXISTS(info.V[ai][0]);
      minsi = nrrd->spaceDim;
      for (si=0; si<nrrd->spaceDim; si++) {
        nrrd->axis[ai].spaceDirection[si] = info.V[ai][si];
        if (exists ^ AIR_EXISTS(info.V[ai][si])) {
          minsi = 0;
          break;
        }
      }
      for (si=minsi; si<NRRD_SPACE_DIM_MAX; si++) {
        nrrd->axis[ai].spaceDirection[si] = AIR_NAN;
      }
      break;
    case nrrdAxisInfoCenter:
      nrrd->axis[ai].center = info.I[ai];
      break;
    case nrrdAxisInfoKind:
      nrrd->axis[ai].kind = info.I[ai];
      break;
    case nrrdAxisInfoLabel:
      nrrd->axis[ai].label = (char *)airFree(nrrd->axis[ai].label);
      nrrd->axis[ai].label = (char *)airStrdup(info.CP[ai]);
      break;
    case nrrdAxisInfoUnits:
      nrrd->axis[ai].units = (char *)airFree(nrrd->axis[ai].units);
      nrrd->axis[ai].units = (char *)airStrdup(info.CP[ai]);
      break;
    }
  }
  if (nrrdAxisInfoSpaceDirection == axInfo) {
    for (ai=nrrd->dim; ai<NRRD_DIM_MAX; ai++) {
      for (si=0; si<NRRD_SPACE_DIM_MAX; si++) {
        nrrd->axis[ai].spaceDirection[si] = AIR_NAN;
      }
    }
  }
  return;
}

/*
******** nrrdAxisInfoSet_va()
**
** var args front-end for nrrdAxisInfoSet_nva
**
** types to pass, one for each dimension:
**           nrrdAxisInfoSize: size_t
**        nrrdAxisInfoSpacing: double
**      nrrdAxisInfoThickness: double
**            nrrdAxisInfoMin: double
**            nrrdAxisInfoMax: double
** nrrdAxisInfoSpaceDirection: double*
**         nrrdAxisInfoCenter: int
**           nrrdAxisInfoKind: int
**          nrrdAxisInfoLabel: char*
**          nrrdAxisInfoUnits: char*
*/
void
nrrdAxisInfoSet_va(Nrrd *nrrd, int axInfo, ...) {
  NRRD_TYPE_BIGGEST *buffer[NRRD_DIM_MAX];
  _nrrdAxisInfoSetPtrs info;
  unsigned int ai, si;
  va_list ap;
  double *dp, svec[NRRD_DIM_MAX][NRRD_SPACE_DIM_MAX];

  if (!( nrrd
         && AIR_IN_CL(1, nrrd->dim, NRRD_DIM_MAX)
         && AIR_IN_OP(nrrdAxisInfoUnknown, axInfo, nrrdAxisInfoLast) )) {
    return;
  }

  info.P = buffer;
  va_start(ap, axInfo);
  for (ai=0; ai<nrrd->dim; ai++) {
    switch (axInfo) {
    case nrrdAxisInfoSize:
      info.ST[ai] = va_arg(ap, size_t);
      /*
      printf("!%s: got int[%d] = %d\n", "nrrdAxisInfoSet", d, info.I[ai]);
      */
      break;
    case nrrdAxisInfoSpaceDirection:
      dp = va_arg(ap, double*);  /* punting on using info enum */
      /*
      printf("!%s: got dp = %lu\n", "nrrdAxisInfoSet",
             (unsigned long)(dp));
      */
      for (si=0; si<nrrd->spaceDim; si++) {
        /* nrrd->axis[ai].spaceDirection[si] = dp[si]; */
        svec[ai][si] = dp[si];
      }
      for (si=nrrd->spaceDim; si<NRRD_SPACE_DIM_MAX; si++) {
        /* nrrd->axis[ai].spaceDirection[si] = AIR_NAN; */
        svec[ai][si] = dp[si];
      }
      break;
    case nrrdAxisInfoCenter:
    case nrrdAxisInfoKind:
      info.I[ai] = va_arg(ap, int);
      /*
      printf("!%s: got int[%d] = %d\n",
             "nrrdAxisInfoSet", d, info.I[ai]);
      */
      break;
    case nrrdAxisInfoSpacing:
    case nrrdAxisInfoThickness:
    case nrrdAxisInfoMin:
    case nrrdAxisInfoMax:
      info.D[ai] = va_arg(ap, double);
      /*
      printf("!%s: got double[%d] = %g\n",
             "nrrdAxisInfoSet", d, info.D[ai]);
      */
      break;
    case nrrdAxisInfoLabel:
      /* we DO NOT do the airStrdup() here because this pointer value is
         just going to be handed to nrrdAxisInfoSet_nva(), which WILL do the
         airStrdup(); we're not violating the rules for axis labels */
      info.CP[ai] = va_arg(ap, char *);
      /*
      printf("!%s: got char*[%d] = |%s|\n",
             "nrrdAxisInfoSet", d, info.CP[ai]);
      */
      break;
    case nrrdAxisInfoUnits:
      /* see not above */
      info.CP[ai] = va_arg(ap, char *);
      break;
    }
  }
  va_end(ap);

  if (nrrdAxisInfoSpaceDirection != axInfo) {
    /* now set the quantities which we've gotten from the var args */
    nrrdAxisInfoSet_nva(nrrd, axInfo, info.P);
  } else {
    nrrdAxisInfoSet_nva(nrrd, axInfo, svec);
  }

  return;
}

/*
******** nrrdAxisInfoGet_nva()
**
** get any of the axis fields into an array
**
** Note that getting axes labels involves implicitly allocating space
** for them, due to the action of airStrdup().  The user is
** responsible for free()ing these strings when done with them.
**
** type to pass for third argument:
**           nrrdAxisInfoSize: size_t*
**        nrrdAxisInfoSpacing: double*
**      nrrdAxisInfoThickness: double*
**            nrrdAxisInfoMin: double*
**            nrrdAxisInfoMax: double*
** nrrdAxisInfoSpaceDirection: double (*var)[NRRD_SPACE_DIM_MAX]
**         nrrdAxisInfoCenter: int*
**           nrrdAxisInfoKind: int*
**          nrrdAxisInfoLabel: char**
**          nrrdAxisInfoUnits: char**
*/
void
nrrdAxisInfoGet_nva(const Nrrd *nrrd, int axInfo, void *_info) {
  _nrrdAxisInfoGetPtrs info;
  unsigned int ai, si;

  if (!( nrrd
         && AIR_IN_CL(1, nrrd->dim, NRRD_DIM_MAX)
         && AIR_IN_OP(nrrdAxisInfoUnknown, axInfo, nrrdAxisInfoLast) )) {
    return;
  }

  info.P = _info;
  for (ai=0; ai<nrrd->dim; ai++) {
    switch (axInfo) {
    case nrrdAxisInfoSize:
      info.ST[ai] = nrrd->axis[ai].size;
      break;
    case nrrdAxisInfoSpacing:
      info.D[ai] = nrrd->axis[ai].spacing;
      break;
    case nrrdAxisInfoThickness:
      info.D[ai] = nrrd->axis[ai].thickness;
      break;
    case nrrdAxisInfoMin:
      info.D[ai] = nrrd->axis[ai].min;
      break;
    case nrrdAxisInfoMax:
      info.D[ai] = nrrd->axis[ai].max;
      break;
    case nrrdAxisInfoSpaceDirection:
      for (si=0; si<nrrd->spaceDim; si++) {
        info.V[ai][si] = nrrd->axis[ai].spaceDirection[si];
      }
      for (si=nrrd->spaceDim; si<NRRD_SPACE_DIM_MAX; si++) {
        info.V[ai][si] = AIR_NAN;
      }
      break;
    case nrrdAxisInfoCenter:
      info.I[ai] = nrrd->axis[ai].center;
      break;
    case nrrdAxisInfoKind:
      info.I[ai] = nrrd->axis[ai].kind;
      break;
    case nrrdAxisInfoLabel:
      /* note airStrdup()! */
      info.CP[ai] = airStrdup(nrrd->axis[ai].label);
      break;
    case nrrdAxisInfoUnits:
      /* note airStrdup()! */
      info.CP[ai] = airStrdup(nrrd->axis[ai].units);
      break;
    }
  }
  if (nrrdAxisInfoSpaceDirection == axInfo) {
    for (ai=nrrd->dim; ai<NRRD_DIM_MAX; ai++) {
      for (si=0; si<NRRD_SPACE_DIM_MAX; si++) {
        info.V[ai][si] = AIR_NAN;
      }
    }
  }
  return;
}

/*
** types to pass, one for each dimension:
**           nrrdAxisInfoSize: size_t*
**        nrrdAxisInfoSpacing: double*
**      nrrdAxisInfoThickness: double*
**            nrrdAxisInfoMin: double*
**            nrrdAxisInfoMax: double*
** nrrdAxisInfoSpaceDirection: double*
**         nrrdAxisInfoCenter: int*
**           nrrdAxisInfoKind: int*
**          nrrdAxisInfoLabel: char**
**          nrrdAxisInfoUnits: char**
*/
void
nrrdAxisInfoGet_va(const Nrrd *nrrd, int axInfo, ...) {
  void *buffer[NRRD_DIM_MAX], *ptr;
  _nrrdAxisInfoGetPtrs info;
  unsigned int ai, si;
  va_list ap;
  double svec[NRRD_DIM_MAX][NRRD_SPACE_DIM_MAX];

  if (!( nrrd
         && AIR_IN_CL(1, nrrd->dim, NRRD_DIM_MAX)
         && AIR_IN_OP(nrrdAxisInfoUnknown, axInfo, nrrdAxisInfoLast) )) {
    return;
  }

  if (nrrdAxisInfoSpaceDirection != axInfo) {
    info.P = buffer;
    nrrdAxisInfoGet_nva(nrrd, axInfo, info.P);
  } else {
    nrrdAxisInfoGet_nva(nrrd, axInfo, svec);
  }

  va_start(ap, axInfo);
  for (ai=0; ai<nrrd->dim; ai++) {
    ptr = va_arg(ap, void*);
    /*
    printf("!%s(%d): ptr = %lu\n",
           "nrrdAxisInfoGet", d, (unsigned long)ptr);
    */
    switch (axInfo) {
    case nrrdAxisInfoSize:
      *((size_t*)ptr) = info.ST[ai];
      break;
    case nrrdAxisInfoSpacing:
    case nrrdAxisInfoThickness:
    case nrrdAxisInfoMin:
    case nrrdAxisInfoMax:
      *((double*)ptr) = info.D[ai];
      /* printf("!%s: got double[%d] = %lg\n", "nrrdAxisInfoGet", d,
       *((double*)ptr)); */
      break;
    case nrrdAxisInfoSpaceDirection:
      for (si=0; si<nrrd->spaceDim; si++) {
        ((double*)ptr)[si] = svec[ai][si];
      }
      for (si=nrrd->spaceDim; si<NRRD_SPACE_DIM_MAX; si++) {
        ((double*)ptr)[si] = AIR_NAN;
      }
      break;
    case nrrdAxisInfoCenter:
    case nrrdAxisInfoKind:
      *((int*)ptr) = info.I[ai];
      /* printf("!%s: got int[%d] = %d\n",
         "nrrdAxisInfoGet", d, *((int*)ptr)); */
      break;
    case nrrdAxisInfoLabel:
    case nrrdAxisInfoUnits:
      /* we DO NOT do the airStrdup() here because this pointer value just
         came from nrrdAxisInfoGet_nva(), which already did the airStrdup() */
      *((char**)ptr) = info.CP[ai];
      /* printf("!%s: got char*[%d] = |%s|\n", "nrrdAxisInfoSet", d,
       *((char**)ptr)); */
      break;
    }
  }
  va_end(ap);

  return;
}

/*
** _nrrdCenter()
**
** for nrrdCenterCell and nrrdCenterNode, return will be the same
** as input.  Converts nrrdCenterUnknown into nrrdDefaultCenter,
** and then clamps to (nrrdCenterUnknown+1, nrrdCenterLast-1).
**
** Thus, this ALWAYS returns nrrdCenterNode or nrrdCenterCell
** (as long as those are the only two centering schemes).
*/
int
_nrrdCenter(int center) {

  center =  (nrrdCenterUnknown == center
             ? nrrdDefaultCenter
             : center);
  center = AIR_CLAMP(nrrdCenterUnknown+1, center, nrrdCenterLast-1);
  return center;
}

int
_nrrdCenter2(int center, int defCenter) {

  center =  (nrrdCenterUnknown == center
             ? defCenter
             : center);
  center = AIR_CLAMP(nrrdCenterUnknown+1, center, nrrdCenterLast-1);
  return center;
}


/*
******** nrrdAxisInfoPos()
**
** given a nrrd, an axis, and a (floating point) index space position,
** return the position implied the axis's min, max, and center
** Does the opposite of nrrdAxisIdx().
**
** does not use biff
*/
double
nrrdAxisInfoPos(const Nrrd *nrrd, unsigned int ax, double idx) {
  int center;
  size_t size;
  double min, max;

  if (!( nrrd && ax <= nrrd->dim-1 )) {
    return AIR_NAN;
  }
  center = _nrrdCenter(nrrd->axis[ax].center);
  min = nrrd->axis[ax].min;
  max = nrrd->axis[ax].max;
  size = nrrd->axis[ax].size;

  return NRRD_POS(center, min, max, size, idx);
}

/*
******** nrrdAxisInfoIdx()
**
** given a nrrd, an axis, and a (floating point) world space position,
** return the index implied the axis's min, max, and center.
** Does the opposite of nrrdAxisPos().
**
** does not use biff
*/
double
nrrdAxisInfoIdx(const Nrrd *nrrd, unsigned int ax, double pos) {
  int center;
  size_t size;
  double min, max;

  if (!( nrrd && ax <= nrrd->dim-1 )) {
    return AIR_NAN;
  }
  center = _nrrdCenter(nrrd->axis[ax].center);
  min = nrrd->axis[ax].min;
  max = nrrd->axis[ax].max;
  size = nrrd->axis[ax].size;

  return NRRD_IDX(center, min, max, size, pos);
}

/*
******** nrrdAxisInfoPosRange()
**
** given a nrrd, an axis, and two (floating point) index space positions,
** return the range of positions implied the axis's min, max, and center
** The opposite of nrrdAxisIdxRange()
*/
void
nrrdAxisInfoPosRange(double *loP, double *hiP,
                     const Nrrd *nrrd, unsigned int ax,
                     double loIdx, double hiIdx) {
  int center, flip = 0;
  size_t size;
  double min, max, tmp;

  if (!( loP && hiP && nrrd && ax <= nrrd->dim-1 )) {
    *loP = *hiP = AIR_NAN;
    return;
  }
  center = _nrrdCenter(nrrd->axis[ax].center);
  min = nrrd->axis[ax].min;
  max = nrrd->axis[ax].max;
  size = nrrd->axis[ax].size;

  if (loIdx > hiIdx) {
    flip = 1;
    tmp = loIdx; loIdx = hiIdx; hiIdx = tmp;
  }
  if (nrrdCenterCell == center) {
    *loP = AIR_AFFINE(0, loIdx, size, min, max);
    *hiP = AIR_AFFINE(0, hiIdx+1, size, min, max);
  } else {
    *loP = AIR_AFFINE(0, loIdx, size-1, min, max);
    *hiP = AIR_AFFINE(0, hiIdx, size-1, min, max);
  }
  if (flip) {
    tmp = *loP; *loP = *hiP; *hiP = tmp;
  }

  return;
}

/*
******** nrrdAxisInfoIdxRange()
**
** given a nrrd, an axis, and two (floating point) world space positions,
** return the range of index space implied the axis's min, max, and center
** The opposite of nrrdAxisPosRange().
**
** Actually- there are situations where sending an interval through
** nrrdAxisIdxRange -> nrrdAxisPosRange -> nrrdAxisIdxRange
** such as in cell centering, when the range of positions given does
** not even span one sample.  Such as:
** axis->size = 4, axis->min = -4, axis->max = 4, loPos = 0, hiPos = 1
** --> nrrdAxisIdxRange == (2, 1.5) --> nrrdAxisPosRange == (2, -1)
** The basic problem is that because of the 0.5 offset inherent in
** cell centering, there are situations where (in terms of the arguments
** to nrrdAxisIdxRange()) loPos < hiPos, but *loP > *hiP.
*/
void
nrrdAxisInfoIdxRange(double *loP, double *hiP,
                     const Nrrd *nrrd, unsigned int ax,
                     double loPos, double hiPos) {
  int center, flip = 0;
  size_t size;
  double min, max, tmp;

  if (!( loP && hiP && nrrd && ax <= nrrd->dim-1 )) {
    *loP = *hiP = AIR_NAN;
    return;
  }
  center = _nrrdCenter(nrrd->axis[ax].center);
  min = nrrd->axis[ax].min;
  max = nrrd->axis[ax].max;
  size = nrrd->axis[ax].size;

  if (loPos > hiPos) {
    flip = 1;
    tmp = loPos; loPos = hiPos; hiPos = tmp;
  }
  if (nrrdCenterCell == center) {
    if (min < max) {
      *loP = AIR_AFFINE(min, loPos, max, 0, size);
      *hiP = AIR_AFFINE(min, hiPos, max, -1, size-1);
    } else {
      *loP = AIR_AFFINE(min, loPos, max, -1, size-1);
      *hiP = AIR_AFFINE(min, hiPos, max, 0, size);
    }
  } else {
    *loP = AIR_AFFINE(min, loPos, max, 0, size-1);
    *hiP = AIR_AFFINE(min, hiPos, max, 0, size-1);
  }
  if (flip) {
    tmp = *loP; *loP = *hiP; *hiP = tmp;
  }

  return;
}

void
nrrdAxisInfoSpacingSet(Nrrd *nrrd, unsigned int ax) {
  int sign;
  double min, max, tmp;

  if (!( nrrd && ax <= nrrd->dim-1 )) {
    return;
  }

  min = nrrd->axis[ax].min;
  max = nrrd->axis[ax].max;
  if (!( AIR_EXISTS(min) && AIR_EXISTS(max) )) {
    /* there's no actual basis on which to set the spacing information,
       but we have to set it something, so here goes .. */
    nrrd->axis[ax].spacing = nrrdDefaultSpacing;
    return;
  }

  if (min > max) {
    tmp = min; min = max; max = tmp;
    sign = -1;
  } else {
    sign = 1;
  }

  /* the skinny */
  nrrd->axis[ax].spacing = NRRD_SPACING(_nrrdCenter(nrrd->axis[ax].center),
                                        min, max, nrrd->axis[ax].size);
  nrrd->axis[ax].spacing *= sign;

  return;
}

void
nrrdAxisInfoMinMaxSet(Nrrd *nrrd, unsigned int ax, int defCenter) {
  int center;
  double spacing;

  if (!( nrrd && ax <= nrrd->dim-1 )) {
    return;
  }

  center = _nrrdCenter2(nrrd->axis[ax].center, defCenter);
  spacing = nrrd->axis[ax].spacing;
  if (!AIR_EXISTS(spacing))
    spacing = nrrdDefaultSpacing;
  if (nrrdCenterCell == center) {
    nrrd->axis[ax].min = 0;
    nrrd->axis[ax].max = spacing*AIR_CAST(double, nrrd->axis[ax].size);
  } else {
    nrrd->axis[ax].min = 0;
    nrrd->axis[ax].max = spacing*AIR_CAST(double, nrrd->axis[ax].size - 1);
  }

  return;
}

/*
******** nrrdDomainAxesGet
**
** Based on the per-axis "kind" field, learns which are the domain
** (resample-able) axes of an image, in other words, the axes which
** correspond to independent variables.  The return value is the
** number of domain axes, and that many values are set in the given
** axisIdx[] array
**
** NOTE: this takes a wild guess that an unset (nrrdKindUnknown) kind
** is a domain axis.
*/
unsigned int
nrrdDomainAxesGet(const Nrrd *nrrd, unsigned int axisIdx[NRRD_DIM_MAX]) {
  unsigned int domAxi, axi;

  if (!( nrrd && axisIdx )) {
    return 0;
  }
  domAxi = 0;
  for (axi=0; axi<nrrd->dim; axi++) {
    if (nrrdKindUnknown == nrrd->axis[axi].kind
        || nrrdKindIsDomain(nrrd->axis[axi].kind)) {
      axisIdx[domAxi++] = axi;
    }
  }
  return domAxi;
}

int
_nrrdSpaceVecExists(const Nrrd *nrrd, unsigned int axi) {
  unsigned int sai;
  int ret;

  if (!( nrrd && axi < nrrd->dim && nrrd->spaceDim )) {
    ret = AIR_FALSE;
  } else {
    ret = AIR_TRUE;
    for (sai=0; sai<nrrd->spaceDim; sai++) {
      ret &= AIR_EXISTS(nrrd->axis[axi].spaceDirection[sai]);
    }
  }
  return ret;
}

unsigned int
nrrdSpatialAxesGet(const Nrrd *nrrd, unsigned int axisIdx[NRRD_DIM_MAX]) {
  unsigned int spcAxi, axi;

  if (!( nrrd && axisIdx && nrrd->spaceDim)) {
    return 0;
  }
  spcAxi = 0;
  for (axi=0; axi<nrrd->dim; axi++) {
    if (_nrrdSpaceVecExists(nrrd, axi)) {
      axisIdx[spcAxi++] = axi;
    }
  }
  return spcAxi;
}

/*
******** nrrdRangeAxesGet
**
** Based on the per-axis "kind" field, learns which are the range
** (non-resample-able) axes of an image, in other words, the axes
** which correspond to dependent variables.  The return value is the
** number of range axes; that number of values are set in the given
** axisIdx[] array
**
** Note: this really is as simple as returning the complement of the
** axis selected by nrrdDomainAxesGet()
*/
unsigned int
nrrdRangeAxesGet(const Nrrd *nrrd, unsigned int axisIdx[NRRD_DIM_MAX]) {
  unsigned int domNum, domIdx[NRRD_DIM_MAX], rngAxi, axi, ii, isDom;

  if (!( nrrd && axisIdx )) {
    return 0;
  }
  domNum = nrrdDomainAxesGet(nrrd, domIdx);
  rngAxi = 0;
  for (axi=0; axi<nrrd->dim; axi++) {
    isDom = AIR_FALSE;
    for (ii=0; ii<domNum; ii++) {   /* yes, inefficient */
      isDom |= axi == domIdx[ii];
    }
    if (!isDom) {
      axisIdx[rngAxi++] = axi;
    }
  }
  return rngAxi;
}

unsigned int
nrrdNonSpatialAxesGet(const Nrrd *nrrd, unsigned int axisIdx[NRRD_DIM_MAX]) {
  unsigned int spcNum, spcIdx[NRRD_DIM_MAX], nspAxi, axi, ii, isSpc;

  if (!( nrrd && axisIdx )) {
    return 0;
  }
  /* HEY: copy and paste, should refactor with above */
  spcNum = nrrdSpatialAxesGet(nrrd, spcIdx);
  nspAxi = 0;
  for (axi=0; axi<nrrd->dim; axi++) {
    isSpc = AIR_FALSE;
    for (ii=0; ii<spcNum; ii++) {   /* yes, inefficient */
      isSpc |= axi == spcIdx[ii];
    }
    if (!isSpc) {
      axisIdx[nspAxi++] = axi;
    }
  }
  return nspAxi;
}


/*
******** nrrdSpacingCalculate
**
** Determine nrrdSpacingStatus, and whatever can be calculated about
** spacing for a given axis.  Takes a nrrd, an axis, a double pointer
** (for returning a scalar), a space vector, and an int pointer for
** returning the known length of the space vector.
**
** The behavior of what has been set by the function is determined by
** the return value, which takes values from the nrrdSpacingStatus*
** enum, as follows:
**
** returned status value:            what it means, and what it set
** ---------------------------------------------------------------------------
** nrrdSpacingStatusUnknown          Something about the given arguments is
**                                   invalid.
**                                   *spacing = NaN,
**                                   vector = all NaNs
**
** nrrdSpacingStatusNone             There is no spacing info at all:
**                                   *spacing = NaN,
**                                   vector = all NaNs
**
** nrrdSpacingStatusScalarNoSpace    There is no surrounding space, but the
**                                   axis's spacing was known.
**                                   *spacing = axis->spacing,
**                                   vector = all NaNs
**
** nrrdSpacingStatusScalarWithSpace  There *is* a surrounding space, but the
**                                   given axis does not live in that space,
**                                   because it has no space direction.  Caller
**                                   may want to think about what's going on.
**                                   *spacing = axis->spacing,
**                                   vector = all NaNs
**
** nrrdSpacingStatusDirection        There is a surrounding space, in which
**                                   this axis has a direction V:
**                                   *spacing = |V| (length of direction),
**                                   vector = V/|V| (normalized direction)
**                                   NOTE: it is still possible for both
**                                   *spacing and vector to be all NaNs!!
*/
int
nrrdSpacingCalculate(const Nrrd *nrrd, unsigned int ax,
                     double *spacing, double vector[NRRD_SPACE_DIM_MAX]) {
  int ret;

  if (!( nrrd && spacing && vector
         && ax <= nrrd->dim-1
         && !_nrrdCheck(nrrd, AIR_FALSE, AIR_FALSE) )) {
    /* there's a problem with the arguments.  Note: the _nrrdCheck()
       call does not check on non-NULL-ity of nrrd->data */
    ret = nrrdSpacingStatusUnknown;
    if (spacing) {
      *spacing = AIR_NAN;
    }
    if (vector) {
      nrrdSpaceVecSetNaN(vector);
    }
  } else {
    if (AIR_EXISTS(nrrd->axis[ax].spacing)) {
      if (nrrd->spaceDim > 0) {
        ret = nrrdSpacingStatusScalarWithSpace;
      } else {
        ret = nrrdSpacingStatusScalarNoSpace;
      }
      *spacing = nrrd->axis[ax].spacing;
      nrrdSpaceVecSetNaN(vector);
    } else {
      if (nrrd->spaceDim > 0 && _nrrdSpaceVecExists(nrrd, ax)) {
        ret = nrrdSpacingStatusDirection;
        *spacing = nrrdSpaceVecNorm(nrrd->spaceDim,
                                    nrrd->axis[ax].spaceDirection);
        nrrdSpaceVecScale(vector, 1.0/(*spacing),
                          nrrd->axis[ax].spaceDirection);
      } else {
        ret = nrrdSpacingStatusNone;
        *spacing = AIR_NAN;
        nrrdSpaceVecSetNaN(vector);
      }
    }
  }
  return ret;
}

int
nrrdOrientationReduce(Nrrd *nout, const Nrrd *nin,
                      int setMinsFromOrigin) {
  static const char me[]="nrrdOrientationReduce";
  unsigned int spatialAxisNum, spatialAxisIdx[NRRD_DIM_MAX], saxii;
  NrrdAxisInfo *axis;

  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL spacing", me);
    return 1;
  }

  if (nout != nin) {
    if (nrrdCopy(nout, nin)) {
      biffAddf(NRRD, "%s: trouble doing initial copying", me);
      return 1;
    }
  }
  if (!nout->spaceDim) {
    /* we're done! */
    return 0;
  }
  spatialAxisNum = nrrdSpatialAxesGet(nout, spatialAxisIdx);
  for (saxii=0; saxii<spatialAxisNum; saxii++) {
    axis = nout->axis + spatialAxisIdx[saxii];
    axis->spacing = nrrdSpaceVecNorm(nout->spaceDim,
                                     axis->spaceDirection);
    if (setMinsFromOrigin) {
      axis->min = (saxii < nout->spaceDim
                   ? nout->spaceOrigin[saxii]
                   : AIR_NAN);
    }
  }
  nrrdSpaceSet(nout, nrrdSpaceUnknown);

  return 0;
}


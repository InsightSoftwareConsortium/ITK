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

/*
******** nrrdSlice()
**
** slices a nrrd along a given axis, at a given position.
**
** This is a newer version of the procedure, which is simpler, faster,
** and requires less memory overhead than the first one.  It is based
** on the observation that any slice is a periodic square-wave pattern
** in the original data (viewed as a one- dimensional array).  The
** characteristics of that periodic pattern are how far from the
** beginning it starts (offset), the length of the "on" part (length),
** the period (period), and the number of periods (numper).
*/
int
nrrdSlice(Nrrd *nout, const Nrrd *cnin, unsigned int saxi, size_t pos) {
  static const char me[]="nrrdSlice", func[]="slice";
  size_t
    I,
    rowLen,                  /* length of segment */
    colStep,                 /* distance between start of each segment */
    colLen,                  /* number of periods */
    szOut[NRRD_DIM_MAX];
  unsigned int ai, outdim;
  int map[NRRD_DIM_MAX];
  const char *src;
  char *dest, stmp[2][AIR_STRLEN_SMALL];
  airArray *mop;
  Nrrd *nin;

  if (!(cnin && nout)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (nout == cnin) {
    biffAddf(NRRD, "%s: nout==nin disallowed", me);
    return 1;
  }
  if (1 == cnin->dim) {
    if (0 != saxi) {
      biffAddf(NRRD, "%s: slice axis must be 0, not %u, for 1-D array",
               me, saxi);
      return 1;
    }
  } else {
    if (!( saxi < cnin->dim )) {
      biffAddf(NRRD, "%s: slice axis %d out of bounds (0 to %d)",
               me, saxi, cnin->dim-1);
      return 1;
    }
  }
  if (!( pos < cnin->axis[saxi].size )) {
    biffAddf(NRRD, "%s: position %s out of bounds (0 to %s)", me,
             airSprintSize_t(stmp[0], pos),
             airSprintSize_t(stmp[1], cnin->axis[saxi].size-1));
    return 1;
  }
  /* this shouldn't actually be necessary .. */
  if (!nrrdElementSize(cnin)) {
    biffAddf(NRRD, "%s: nrrd reports zero element size!", me);
    return 1;
  }

  /* HEY: copy and paste from measure.c/nrrdProject */
  mop = airMopNew();
  if (1 == cnin->dim) {
    /* There are more efficient ways of dealing with this case; this way is
       easy to implement because it leaves most of the established code below
       only superficially changed; uniformly replacing nin with (nin ? nin :
       cnin), even if pointlessly so; this expression that can't be assigned
       to a new variable because of the difference in const. */
    nin = nrrdNew();
    airMopAdd(mop, nin, (airMopper)nrrdNuke, airMopAlways);
    if (nrrdAxesInsert(nin, cnin, 1)) {
      biffAddf(NRRD, "%s: trouble inserting axis on 1-D array", me);
      airMopError(mop); return 1;
    }
  } else {
    nin = NULL;
  }

  /* set up control variables */
  rowLen = colLen = 1;
  for (ai=0; ai<(nin ? nin : cnin)->dim; ai++) {
    if (ai < saxi) {
      rowLen *= (nin ? nin : cnin)->axis[ai].size;
    } else if (ai > saxi) {
      colLen *= (nin ? nin : cnin)->axis[ai].size;
    }
  }
  rowLen *= nrrdElementSize(nin ? nin : cnin);
  colStep = rowLen*(nin ? nin : cnin)->axis[saxi].size;

  outdim = (nin ? nin : cnin)->dim-1;
  for (ai=0; ai<outdim; ai++) {
    map[ai] = AIR_INT(ai) + (ai >= saxi);
    szOut[ai] = (nin ? nin : cnin)->axis[map[ai]].size;
  }
  nout->blockSize = (nin ? nin : cnin)->blockSize;
  if (nrrdMaybeAlloc_nva(nout, (nin ? nin : cnin)->type, outdim, szOut)) {
    biffAddf(NRRD, "%s: failed to create slice", me);
    airMopError(mop); return 1;
  }

  /* the skinny */
  src = AIR_CAST(const char *, (nin ? nin : cnin)->data);
  dest = AIR_CAST(char *, nout->data);
  src += rowLen*pos;
  for (I=0; I<colLen; I++) {
    /* HEY: replace with AIR_MEMCPY() or similar, when applicable */
    memcpy(dest, src, rowLen);
    src += colStep;
    dest += rowLen;
  }

  /* copy the peripheral information */
  if (nrrdAxisInfoCopy(nout, (nin ? nin : cnin), map, NRRD_AXIS_INFO_NONE)) {
    biffAddf(NRRD, "%s:", me);
    airMopError(mop); return 1;
  }
  if (nrrdContentSet_va(nout, func, cnin /* hide possible axinsert*/,
                        "%d,%d", saxi, pos)) {
    biffAddf(NRRD, "%s:", me);
    airMopError(mop); return 1;
  }
  if (nrrdBasicInfoCopy(nout, (nin ? nin : cnin),
                        NRRD_BASIC_INFO_DATA_BIT
                        | NRRD_BASIC_INFO_TYPE_BIT
                        | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                        | NRRD_BASIC_INFO_DIMENSION_BIT
                        | NRRD_BASIC_INFO_SPACEORIGIN_BIT
                        | NRRD_BASIC_INFO_CONTENT_BIT
                        | NRRD_BASIC_INFO_COMMENTS_BIT
                        | (nrrdStateKeyValuePairsPropagate
                           ? 0
                           : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT))) {
    biffAddf(NRRD, "%s:", me);
    airMopError(mop); return 1;
  }
  /* translate origin if this was a spatial axis, otherwise copy */
  /* note that if there is no spatial info at all, this is all harmless */
  if (AIR_EXISTS((nin ? nin : cnin)->axis[saxi].spaceDirection[0])) {
    nrrdSpaceVecScaleAdd2(nout->spaceOrigin,
                          1.0, (nin ? nin : cnin)->spaceOrigin,
                          AIR_CAST(double, pos),
                          (nin ? nin : cnin)->axis[saxi].spaceDirection);
  } else {
    nrrdSpaceVecCopy(nout->spaceOrigin, (nin ? nin : cnin)->spaceOrigin);
  }
  airMopOkay(mop);
  return 0;
}

/*
******** nrrdCrop()
**
** select some sub-volume inside a given nrrd, producing an output
** nrrd with the same dimensions, but with equal or smaller sizes
** along each axis.
*/
int
nrrdCrop(Nrrd *nout, const Nrrd *nin, size_t *min, size_t *max) {
  static const char me[]="nrrdCrop", func[] = "crop";
  char buff1[NRRD_DIM_MAX*30], buff2[AIR_STRLEN_SMALL];
  unsigned int ai;
  size_t I,
    lineSize,                /* #bytes in one scanline to be copied */
    typeSize,                /* size of data type */
    cIn[NRRD_DIM_MAX],       /* coords for line start, in input */
    cOut[NRRD_DIM_MAX],      /* coords for line start, in output */
    szIn[NRRD_DIM_MAX],
    szOut[NRRD_DIM_MAX],
    idxIn, idxOut,           /* linear indices for input and output */
    numLines;                /* number of scanlines in output nrrd */
  char *dataIn, *dataOut, stmp[3][AIR_STRLEN_SMALL];

  /* errors */
  if (!(nout && nin && min && max)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (nout == nin) {
    biffAddf(NRRD, "%s: nout==nin disallowed", me);
    return 1;
  }
  for (ai=0; ai<nin->dim; ai++) {
    if (!(min[ai] <= max[ai])) {
      biffAddf(NRRD, "%s: axis %d min (%s) not <= max (%s)", me, ai,
               airSprintSize_t(stmp[0], min[ai]),
               airSprintSize_t(stmp[1], max[ai]));
      return 1;
    }
    if (!( min[ai] < nin->axis[ai].size && max[ai] < nin->axis[ai].size )) {
      biffAddf(NRRD, "%s: axis %d min (%s) or max (%s) out of bounds [0,%s]",
               me, ai, airSprintSize_t(stmp[0], min[ai]),
               airSprintSize_t(stmp[1], max[ai]),
               airSprintSize_t(stmp[2], nin->axis[ai].size-1));
      return 1;
    }
  }
  /* this shouldn't actually be necessary .. */
  if (!nrrdElementSize(nin)) {
    biffAddf(NRRD, "%s: nrrd reports zero element size!", me);
    return 1;
  }

  /* allocate */
  nrrdAxisInfoGet_nva(nin, nrrdAxisInfoSize, szIn);
  numLines = 1;
  for (ai=0; ai<nin->dim; ai++) {
    szOut[ai] = max[ai] - min[ai] + 1;
    if (ai) {
      numLines *= szOut[ai];
    }
  }
  nout->blockSize = nin->blockSize;
  if (nrrdMaybeAlloc_nva(nout, nin->type, nin->dim, szOut)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  lineSize = szOut[0]*nrrdElementSize(nin);

  /* the skinny */
  typeSize = nrrdElementSize(nin);
  dataIn = (char *)nin->data;
  dataOut = (char *)nout->data;
  memset(cOut, 0, NRRD_DIM_MAX*sizeof(*cOut));
  /*
  printf("!%s: nin->dim = %d\n", me, nin->dim);
  printf("!%s: min  = %d %d %d\n", me, min[0], min[1], min[2]);
  printf("!%s: szIn = %d %d %d\n", me, szIn[0], szIn[1], szIn[2]);
  printf("!%s: szOut = %d %d %d\n", me, szOut[0], szOut[1], szOut[2]);
  printf("!%s: lineSize = %d\n", me, lineSize);
  printf("!%s: typeSize = %d\n", me, typeSize);
  printf("!%s: numLines = %d\n", me, (int)numLines);
  */
  for (I=0; I<numLines; I++) {
    for (ai=0; ai<nin->dim; ai++) {
      cIn[ai] = cOut[ai] + min[ai];
    }
    NRRD_INDEX_GEN(idxOut, cOut, szOut, nin->dim);
    NRRD_INDEX_GEN(idxIn, cIn, szIn, nin->dim);
    /*
    printf("!%s: %5d: cOut=(%3d,%3d,%3d) --> idxOut = %5d\n",
           me, (int)I, cOut[0], cOut[1], cOut[2], (int)idxOut);
    printf("!%s: %5d:  cIn=(%3d,%3d,%3d) -->  idxIn = %5d\n",
           me, (int)I, cIn[0], cIn[1], cIn[2], (int)idxIn);
    */
    memcpy(dataOut + idxOut*typeSize, dataIn + idxIn*typeSize, lineSize);
    /* the lowest coordinate in cOut[] will stay zero, since we are
       copying one (1-D) scanline at a time */
    NRRD_COORD_INCR(cOut, szOut, nin->dim, 1);
  }
  if (nrrdAxisInfoCopy(nout, nin, NULL, (NRRD_AXIS_INFO_SIZE_BIT |
                                         NRRD_AXIS_INFO_MIN_BIT |
                                         NRRD_AXIS_INFO_MAX_BIT ))) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  for (ai=0; ai<nin->dim; ai++) {
    nrrdAxisInfoPosRange(&(nout->axis[ai].min), &(nout->axis[ai].max),
                         nin, ai, AIR_CAST(double, min[ai]),
                         AIR_CAST(double, max[ai]));
    /* do the safe thing first */
    nout->axis[ai].kind = _nrrdKindAltered(nin->axis[ai].kind, AIR_FALSE);
    /* try cleverness */
    if (!nrrdStateKindNoop) {
      if (nout->axis[ai].size == nin->axis[ai].size) {
        /* we can safely copy kind; the samples didn't change */
        nout->axis[ai].kind = nin->axis[ai].kind;
      } else if (nrrdKind4Color == nin->axis[ai].kind
                 && 3 == szOut[ai]) {
        nout->axis[ai].kind = nrrdKind3Color;
      } else if (nrrdKind4Vector == nin->axis[ai].kind
                 && 3 == szOut[ai]) {
        nout->axis[ai].kind = nrrdKind3Vector;
      } else if ((nrrdKind4Vector == nin->axis[ai].kind
                  || nrrdKind3Vector == nin->axis[ai].kind)
                 && 2 == szOut[ai]) {
        nout->axis[ai].kind = nrrdKind2Vector;
      } else if (nrrdKindRGBAColor == nin->axis[ai].kind
                 && 0 == min[ai]
                 && 2 == max[ai]) {
        nout->axis[ai].kind = nrrdKindRGBColor;
      } else if (nrrdKind2DMaskedSymMatrix == nin->axis[ai].kind
                 && 1 == min[ai]
                 && max[ai] == szIn[ai]-1) {
        nout->axis[ai].kind = nrrdKind2DSymMatrix;
      } else if (nrrdKind2DMaskedMatrix == nin->axis[ai].kind
                 && 1 == min[ai]
                 && max[ai] == szIn[ai]-1) {
        nout->axis[ai].kind = nrrdKind2DMatrix;
      } else if (nrrdKind3DMaskedSymMatrix == nin->axis[ai].kind
                 && 1 == min[ai]
                 && max[ai] == szIn[ai]-1) {
        nout->axis[ai].kind = nrrdKind3DSymMatrix;
      } else if (nrrdKind3DMaskedMatrix == nin->axis[ai].kind
                 && 1 == min[ai]
                 && max[ai] == szIn[ai]-1) {
        nout->axis[ai].kind = nrrdKind3DMatrix;
      }
    }
  }
  strcpy(buff1, "");
  for (ai=0; ai<nin->dim; ai++) {
    sprintf(buff2, "%s[%s,%s]", (ai ? "x" : ""),
            airSprintSize_t(stmp[0], min[ai]),
            airSprintSize_t(stmp[1], max[ai]));
    strcat(buff1, buff2);
  }
  if (nrrdContentSet_va(nout, func, nin, "%s", buff1)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  if (nrrdBasicInfoCopy(nout, nin,
                        NRRD_BASIC_INFO_DATA_BIT
                        | NRRD_BASIC_INFO_TYPE_BIT
                        | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                        | NRRD_BASIC_INFO_DIMENSION_BIT
                        | NRRD_BASIC_INFO_SPACEORIGIN_BIT
                        | NRRD_BASIC_INFO_CONTENT_BIT
                        | NRRD_BASIC_INFO_COMMENTS_BIT
                        | (nrrdStateKeyValuePairsPropagate
                           ? 0
                           : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT))) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  /* copy origin, then shift it along the spatial axes */
  nrrdSpaceVecCopy(nout->spaceOrigin, nin->spaceOrigin);
  for (ai=0; ai<nin->dim; ai++) {
    if (AIR_EXISTS(nin->axis[ai].spaceDirection[0])) {
      nrrdSpaceVecScaleAdd2(nout->spaceOrigin,
                            1.0, nout->spaceOrigin,
                            AIR_CAST(double, min[ai]),
                            nin->axis[ai].spaceDirection);
    }
  }


  return 0;
}


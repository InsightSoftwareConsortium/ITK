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
nrrdSlice(Nrrd *nout, const Nrrd *nin, unsigned int saxi, size_t pos) {
  char me[]="nrrdSlice", func[]="slice", err[AIR_STRLEN_MED];
  size_t 
    I, 
    rowLen,                  /* length of segment */
    colStep,                 /* distance between start of each segment */
    colLen,                  /* number of periods */
    szOut[NRRD_DIM_MAX];
  unsigned int ai, outdim;
  int map[NRRD_DIM_MAX];
  char *src, *dest;

  if (!(nin && nout)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nout == nin) {
    sprintf(err, "%s: nout==nin disallowed", me);
    biffAdd(NRRD, err); return 1;
  }
  if (1 == nin->dim) {
    sprintf(err, "%s: can't slice a 1-D nrrd; use nrrd{I,F,D}Lookup[]", me);
    biffAdd(NRRD, err); return 1;
  }
  if (!( saxi < nin->dim )) {
    sprintf(err, "%s: slice axis %d out of bounds (0 to %d)", 
            me, saxi, nin->dim-1);
    biffAdd(NRRD, err); return 1;
  }
  if (!( pos < nin->axis[saxi].size )) {
    sprintf(err, "%s: position " _AIR_SIZE_T_CNV 
            " out of bounds (0 to " _AIR_SIZE_T_CNV  ")", 
            me, pos, nin->axis[saxi].size-1);
    biffAdd(NRRD, err); return 1;
  }
  /* this shouldn't actually be necessary ... */
  if (!nrrdElementSize(nin)) {
    sprintf(err, "%s: nrrd reports zero element size!", me);
    biffAdd(NRRD, err); return 1;
  }

  /* set up control variables */
  rowLen = colLen = 1;
  for (ai=0; ai<nin->dim; ai++) {
    if (ai < saxi) {
      rowLen *= nin->axis[ai].size;
    } else if (ai > saxi) {
      colLen *= nin->axis[ai].size;
    }
  }
  rowLen *= nrrdElementSize(nin);
  colStep = rowLen*nin->axis[saxi].size;

  outdim = nin->dim-1;
  for (ai=0; ai<outdim; ai++) {
    map[ai] = ai + (ai >= saxi);
    szOut[ai] = nin->axis[map[ai]].size;
  }
  nout->blockSize = nin->blockSize;
  if (nrrdMaybeAlloc_nva(nout, nin->type, outdim, szOut)) {
    sprintf(err, "%s: failed to create slice", me);
    biffAdd(NRRD, err); return 1;
  }

  /* the skinny */
  src = (char *)nin->data;
  dest = (char *)nout->data;
  src += rowLen*pos;
  for (I=0; I<colLen; I++) {
    /* HEY: replace with AIR_MEMCPY() or similar, when applicable */
    memcpy(dest, src, rowLen);
    src += colStep;
    dest += rowLen;
  }

  /* copy the peripheral information */
  if (nrrdAxisInfoCopy(nout, nin, map, NRRD_AXIS_INFO_NONE)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nrrdContentSet(nout, func, nin, "%d,%d", saxi, pos)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nrrdBasicInfoCopy(nout, nin,
                        NRRD_BASIC_INFO_DATA_BIT
                        | NRRD_BASIC_INFO_TYPE_BIT
                        | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                        | NRRD_BASIC_INFO_DIMENSION_BIT
                        | NRRD_BASIC_INFO_CONTENT_BIT
                        | NRRD_BASIC_INFO_COMMENTS_BIT
                        | NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  /* but we can set the origin more accurately */
  if (AIR_EXISTS(nout->spaceOrigin[0])) {
    _nrrdSpaceVecScaleAdd2(nout->spaceOrigin,
                           1.0, nin->spaceOrigin,
                           pos, nin->axis[saxi].spaceDirection);
  }

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
  char me[]="nrrdCrop", func[] = "crop", err[AIR_STRLEN_MED],
    buff1[NRRD_DIM_MAX*30], buff2[AIR_STRLEN_SMALL];
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
  char *dataIn, *dataOut;

  /* errors */
  if (!(nout && nin && min && max)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nout == nin) {
    sprintf(err, "%s: nout==nin disallowed", me);
    biffAdd(NRRD, err); return 1;
  }
  for (ai=0; ai<nin->dim; ai++) {
    if (!(min[ai] <= max[ai])) {
      sprintf(err, "%s: axis %d min (" _AIR_SIZE_T_CNV 
              ") not <= max (" _AIR_SIZE_T_CNV ")", 
              me, ai, min[ai], max[ai]);
      biffAdd(NRRD, err); return 1;
    }
    if (!( min[ai] < nin->axis[ai].size && max[ai] < nin->axis[ai].size )) {
      sprintf(err, "%s: axis %d min (" _AIR_SIZE_T_CNV  
              ") or max (" _AIR_SIZE_T_CNV  ") out of bounds [0," 
              _AIR_SIZE_T_CNV  "]",
              me, ai, min[ai], max[ai], nin->axis[ai].size-1);
      biffAdd(NRRD, err); return 1;
    }
  }
  /* this shouldn't actually be necessary ... */
  if (!nrrdElementSize(nin)) {
    sprintf(err, "%s: nrrd reports zero element size!", me);
    biffAdd(NRRD, err); return 1;
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
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  lineSize = szOut[0]*nrrdElementSize(nin);
  
  /* the skinny */
  typeSize = nrrdElementSize(nin);
  dataIn = (char *)nin->data;
  dataOut = (char *)nout->data;
  memset(cOut, 0, NRRD_DIM_MAX*sizeof(unsigned int));
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
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  for (ai=0; ai<nin->dim; ai++) {
    nrrdAxisInfoPosRange(&(nout->axis[ai].min), &(nout->axis[ai].max),
                         nin, ai, min[ai], max[ai]);
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
    sprintf(buff2, "%s[" _AIR_SIZE_T_CNV  "," _AIR_SIZE_T_CNV  "]",
            (ai ? "x" : ""), min[ai], max[ai]);
    strcat(buff1, buff2);
  }
  if (nrrdContentSet(nout, func, nin, "%s", buff1)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nrrdBasicInfoCopy(nout, nin,
                        NRRD_BASIC_INFO_DATA_BIT
                        | NRRD_BASIC_INFO_TYPE_BIT
                        | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                        | NRRD_BASIC_INFO_DIMENSION_BIT
                        | NRRD_BASIC_INFO_CONTENT_BIT
                        | NRRD_BASIC_INFO_COMMENTS_BIT
                        | NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  /* but we can set the origin more accurately */
  if (AIR_EXISTS(nout->spaceOrigin[0])) {
    for (ai=0; ai<nin->dim; ai++) {
      _nrrdSpaceVecScaleAdd2(nout->spaceOrigin,
                             1.0, nout->spaceOrigin,
                             min[ai], nin->axis[ai].spaceDirection);
    }
  }
                         

  return 0;
}


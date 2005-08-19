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

/*
******** nrrdInvertPerm()
**
** given an array (p) which represents a permutation of n elements,
** compute the inverse permutation ip.  The value of this function
** is not its core functionality, but all the error checking it
** provides.
*/
int
nrrdInvertPerm(unsigned int *invp, const unsigned int *pp, unsigned int nn) {
  char me[]="nrrdInvertPerm", err[AIR_STRLEN_MED];
  int problem;
  unsigned int ii;

  if (!(invp && pp && nn > 0)) {
    sprintf(err, "%s: got NULL pointer or non-positive nn (%d)", me, nn);
    biffAdd(NRRD, err); return 1;
  }
  
  /* use the given array "invp" as a temp buffer for validity checking */
  memset(invp, 0, nn*sizeof(int));
  for (ii=0; ii<nn; ii++) {
    if (!( pp[ii] <= nn-1)) {
      sprintf(err, "%s: permutation element #%d == %d out of bounds [0,%d]",
              me, ii, pp[ii], nn-1);
      biffAdd(NRRD, err); return 1;
    }
    invp[pp[ii]]++;
  }
  problem = AIR_FALSE;
  for (ii=0; ii<nn; ii++) {
    if (1 != invp[ii]) {
      sprintf(err, "%s: element #%d mapped to %d times (should be once)",
              me, ii, invp[ii]);
      biffAdd(NRRD, err); problem = AIR_TRUE;
    }
  }
  if (problem) {
    return 1;
  }

  /* the skinny */
  for (ii=0; ii<nn; ii++) {
    invp[pp[ii]] = ii;
  }

  return 0;
}

/*
******** nrrdAxesInsert
**
** like reshape, but preserves axis information on old axes, and
** this is only for adding a "stub" axis with length 1.  All other
** axis attributes are initialized as usual.
*/
int
nrrdAxesInsert(Nrrd *nout, const Nrrd *nin, unsigned int axis) {
  char me[]="nrrdAxesInsert", func[]="axinsert", err[AIR_STRLEN_MED];
  unsigned int ai;
  
  if (!(nout && nin)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (!( axis <= nin->dim )) {
    sprintf(err, "%s: given axis (%d) outside valid range [0, %d]",
            me, axis, nin->dim);
    biffAdd(NRRD, err); return 1;
  }
  if (NRRD_DIM_MAX == nin->dim) {
    sprintf(err, "%s: given nrrd already at NRRD_DIM_MAX (%d)",
            me, NRRD_DIM_MAX);
    biffAdd(NRRD, err); return 1;
  }
  if (nout != nin) {
    if (_nrrdCopy(nout, nin, (NRRD_BASIC_INFO_COMMENTS_BIT
                              | NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT))) {
      sprintf(err, "%s:", me);
      biffAdd(NRRD, err); return 1;
    }
  }
  nout->dim = 1 + nin->dim;
  for (ai=nin->dim; ai>axis; ai--) {
    _nrrdAxisInfoCopy(&(nout->axis[ai]), &(nin->axis[ai-1]),
                      NRRD_AXIS_INFO_NONE);
  }
  /* the ONLY thing we can say about the new axis is its size */
  _nrrdAxisInfoInit(&(nout->axis[axis]));
  if (!nrrdStateKindNoop) {
    /* except maybe the kind */
    nout->axis[axis].kind = nrrdKindStub;
  }
  nout->axis[axis].size = 1;
  if (nrrdContentSet(nout, func, nin, "%d", axis)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  /* all basic info has already been copied by nrrdCopy() above */
  return 0;
}

/*
******** nrrdAxesPermute
**
** changes the scanline ordering of the data in a nrrd
** 
** The basic means by which data is moved around is with memcpy().
** The goal is to call memcpy() as few times as possible, on memory 
** segments as large as possible.  Currently, this is done by 
** detecting how many of the low-index axes are left untouched by 
** the permutation- this constitutes a "scanline" which can be
** copied around as a unit.  For permuting the y and z axes of a
** matrix-x-y-z order matrix volume, this optimization produced a
** factor of 5 speed up (exhaustive multi-platform tests, of course).
**
** The axes[] array determines the permutation of the axes.
** axis[i] = j means: axis i in the output will be the input's axis j
** (axis[i] answers: "what do I put here", from the standpoint of the output,
** not "where do I put this", from the standpoint of the input)
*/
int
nrrdAxesPermute(Nrrd *nout, const Nrrd *nin, const unsigned int *axes) {
  char me[]="nrrdAxesPermute", func[]="permute", err[AIR_STRLEN_MED],
    buff1[NRRD_DIM_MAX*30], buff2[AIR_STRLEN_SMALL];
  size_t idxOut, idxIn,      /* indices for input and output scanlines */
    lineSize,                /* size of block of memory which can be
                                moved contiguously from input to output,
                                thought of as a "scanline" */
    numLines,                /* how many "scanlines" there are to permute */
    szIn[NRRD_DIM_MAX], *lszIn,
    szOut[NRRD_DIM_MAX], *lszOut;
  char *dataIn, *dataOut;
  int axmap[NRRD_DIM_MAX];
  unsigned int
    cIn[NRRD_DIM_MAX],
    cOut[NRRD_DIM_MAX];
  unsigned int
    ai,                      /* running index along dimensions */
    lowPax,                  /* lowest axis which is "p"ermutated */
    ldim,                    /* nin->dim - lowPax */
    ip[NRRD_DIM_MAX+1],      /* inverse of permutation in "axes" */
    laxes[NRRD_DIM_MAX+1];   /* copy of axes[], but shifted down by lowPax
                                elements, to remove i such that i == axes[i] */
  airArray *mop;

  mop = airMopNew();
  if (!(nin && nout && axes)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); airMopError(mop); return 1;
  }
  /* we don't actually need ip[], computing it is for error checking */
  if (nrrdInvertPerm(ip, axes, nin->dim)) {
    sprintf(err, "%s: couldn't compute axis permutation inverse", me);
    biffAdd(NRRD, err); airMopError(mop); return 1;
  }
  /* this shouldn't actually be necessary ... */
  if (!nrrdElementSize(nin)) {
    sprintf(err, "%s: nrrd reports zero element size!", me);
    biffAdd(NRRD, err); airMopError(mop); return 1;
  }
  
  for (ai=0; ai<nin->dim && axes[ai] == ai; ai++)
    ;
  lowPax = ai;

  /* allocate output by initial copy */
  if (nout != nin) {
    if (nrrdCopy(nout, nin)) {
      sprintf(err, "%s: trouble copying input", me);
      biffAdd(NRRD, err); airMopError(mop); return 1;      
    }
    dataIn = (char*)nin->data;
  } else {
    dataIn = (char*)calloc(nrrdElementNumber(nin), nrrdElementSize(nin));
    if (!dataIn) {
      sprintf(err, "%s: couldn't create local copy of data", me);
      biffAdd(NRRD, err); airMopError(mop); return 1;
    }
    airMopAdd(mop, dataIn, airFree, airMopAlways);
    memcpy(dataIn, nin->data, nrrdElementNumber(nin)*nrrdElementSize(nin));
  }
  if (lowPax < nin->dim) {
    /* if lowPax == nin->dim, then we were given the identity permutation, so
       there's nothing to do other than the copy already done.  Otherwise,
       here we are (actually, lowPax < nin->dim-1) */
    for (ai=0; ai<nin->dim; ai++) {
      axmap[ai] = axes[ai];
    }
    nrrdAxisInfoGet_nva(nin, nrrdAxisInfoSize, szIn);
    if (nrrdAxisInfoCopy(nout, nin, axmap, NRRD_AXIS_INFO_NONE)) {
      sprintf(err, "%s:", me);
      biffAdd(NRRD, err); airMopError(mop); return 1;
    }
    nrrdAxisInfoGet_nva(nout, nrrdAxisInfoSize, szOut);
    /* the skinny */
    lineSize = 1;
    for (ai=0; ai<lowPax; ai++) {
      lineSize *= szIn[ai];
    }
    numLines = nrrdElementNumber(nin)/lineSize;
    lineSize *= nrrdElementSize(nin);
    lszIn = szIn + lowPax;
    lszOut = szOut + lowPax;
    ldim = nin->dim - lowPax;
    memset(laxes, 0, NRRD_DIM_MAX*sizeof(int));
    for (ai=0; ai<ldim; ai++) {
      laxes[ai] = axes[ai+lowPax]-lowPax;
    }
    dataOut = (char *)nout->data;
    memset(cIn, 0, NRRD_DIM_MAX*sizeof(int));
    memset(cOut, 0, NRRD_DIM_MAX*sizeof(int));
    for (idxOut=0; idxOut<numLines; idxOut++) {
      /* in our representation of the coordinates of the start of the
         scanlines that we're copying, we are not even storing all the
         zeros in the coordinates prior to lowPax, and when we go to
         a linear index for the memcpy(), we multiply by lineSize */
      for (ai=0; ai<ldim; ai++) {
        cIn[laxes[ai]] = cOut[ai];
      }
      NRRD_INDEX_GEN(idxIn, cIn, lszIn, ldim);
      memcpy(dataOut + idxOut*lineSize, dataIn + idxIn*lineSize, lineSize);
      NRRD_COORD_INCR(cOut, lszOut, ldim, 0);
    }
    /* set content */
    strcpy(buff1, "");
    for (ai=0; ai<nin->dim; ai++) {
      sprintf(buff2, "%s%d", (ai ? "," : ""), axes[ai]);
      strcat(buff1, buff2);
    }
    if (nrrdContentSet(nout, func, nin, "%s", buff1)) {
      sprintf(err, "%s:", me);
      biffAdd(NRRD, err); airMopError(mop); return 1;
    }
    if (nout != nin) {
      if (nrrdBasicInfoCopy(nout, nin,
                            NRRD_BASIC_INFO_DATA_BIT
                            | NRRD_BASIC_INFO_TYPE_BIT
                            | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                            | NRRD_BASIC_INFO_DIMENSION_BIT
                            | NRRD_BASIC_INFO_CONTENT_BIT
                            | NRRD_BASIC_INFO_COMMENTS_BIT
                            | NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT)) {
        sprintf(err, "%s:", me);
        biffAdd(NRRD, err); airMopError(mop); return 1;
      }
    }
  }
  airMopOkay(mop); 
  return 0;
}

/*
******** nrrdShuffle
**
** rearranges hyperslices of a nrrd along a given axis according to
** given permutation.  This could be used to on a 4D array,
** representing a 3D volume of vectors, to re-order the vector 
** components.
**
** the given permutation array must allocated for at least as long as
** the input nrrd along the chosen axis.  perm[j] = i means that the
** value at position j in the _new_ array should come from position i
** in the _old_array.  The standpoint is from the new, looking at
** where to find the values amid the old array (perm answers "what do
** I put here", not "where do I put this").  This allows multiple
** positions in the new array to copy from the same old position, and
** insures that there is an source for all positions along the new
** array.
*/
int
nrrdShuffle(Nrrd *nout, const Nrrd *nin, unsigned int axis,
            const size_t *perm) {
  char me[]="nrrdShuffle", func[]="shuffle", err[AIR_STRLEN_MED],
    buff1[NRRD_DIM_MAX*30], buff2[AIR_STRLEN_SMALL];
  unsigned int 
    ai, ldim, len,
    cIn[NRRD_DIM_MAX+1],
    cOut[NRRD_DIM_MAX+1];
  size_t idxIn, idxOut, lineSize, numLines, size[NRRD_DIM_MAX], *lsize;
  char *dataIn, *dataOut;

  if (!(nin && nout && perm)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nout == nin) {
    sprintf(err, "%s: nout==nin disallowed", me);
    biffAdd(NRRD, err); return 1;
  }
  if (!( axis < nin->dim )) {
    sprintf(err, "%s: axis %d outside valid range [0,%d]", 
            me, axis, nin->dim-1);
    biffAdd(NRRD, err); return 1;
  }
  len = nin->axis[axis].size;
  for (ai=0; ai<len; ai++) {
    if (!( perm[ai] < len )) {
      sprintf(err, "%s: perm[%d] (" _AIR_SIZE_T_CNV
              ") outside valid range [0,%d]", me, ai, perm[ai], len-1);
      biffAdd(NRRD, err); return 1;
    }
  }
  /* this shouldn't actually be necessary ... */
  if (!nrrdElementSize(nin)) {
    sprintf(err, "%s: nrrd reports zero element size!", me);
    biffAdd(NRRD, err); return 1;
  }
  /* set information in new volume */
  nout->blockSize = nin->blockSize;
  nrrdAxisInfoGet_nva(nin, nrrdAxisInfoSize, size);
  if (nrrdMaybeAlloc_nva(nout, nin->type, nin->dim, size)) {
    sprintf(err, "%s: failed to allocate output", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nrrdAxisInfoCopy(nout, nin, NULL, NRRD_AXIS_INFO_NONE)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  /* the min and max along the shuffled axis are now meaningless */
  nout->axis[axis].min = nout->axis[axis].max = AIR_NAN;
  /* do the safe thing first */
  nout->axis[ai].kind = _nrrdKindAltered(nin->axis[ai].kind, AIR_FALSE);
  /* try cleverness */
  if (!nrrdStateKindNoop) {
    if (0 == nrrdKindSize(nin->axis[axis].kind)
        || nrrdKindStub == nin->axis[axis].kind
        || nrrdKindScalar == nin->axis[axis].kind
        || nrrdKind2Vector == nin->axis[axis].kind
        || nrrdKind3Color == nin->axis[axis].kind
        || nrrdKind4Color == nin->axis[axis].kind
        || nrrdKind3Vector == nin->axis[axis].kind
        || nrrdKind3Gradient == nin->axis[axis].kind
        || nrrdKind3Normal == nin->axis[axis].kind
        || nrrdKind4Vector == nin->axis[axis].kind) {
      /* these kinds have no intrinsic ordering */
      nout->axis[axis].kind = nin->axis[axis].kind;
    }
  }
  /* the skinny */
  lineSize = 1;
  for (ai=0; ai<axis; ai++) {
    lineSize *= nin->axis[ai].size;
  }
  numLines = nrrdElementNumber(nin)/lineSize;
  lineSize *= nrrdElementSize(nin);
  lsize = size + axis;
  ldim = nin->dim - axis;
  dataIn = (char *)nin->data;
  dataOut = (char *)nout->data;
  memset(cIn, 0, (NRRD_DIM_MAX+1)*sizeof(int));
  memset(cOut, 0, (NRRD_DIM_MAX+1)*sizeof(int));
  for (idxOut=0; idxOut<numLines; idxOut++) {
    memcpy(cIn, cOut, ldim*sizeof(int));
    cIn[0] = perm[cOut[0]];
    NRRD_INDEX_GEN(idxIn, cIn, lsize, ldim);
    NRRD_INDEX_GEN(idxOut, cOut, lsize, ldim);
    memcpy(dataOut + idxOut*lineSize, dataIn + idxIn*lineSize, lineSize);
    NRRD_COORD_INCR(cOut, lsize, ldim, 0);
  }
  /* content */
  strcpy(buff1, "");
  for (ai=0; ai<nin->dim; ai++) {
    sprintf(buff2, "%s" _AIR_SIZE_T_CNV, (ai ? "," : ""), perm[ai]);
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

  return 0;
}


/*
  Teem: Tools to process and visualize scientific data and images              
  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  (LGPL) as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  The terms of redistributing and/or modifying this software also
  include exceptions to the LGPL that facilitate static linking.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this library; if not, write to Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#if defined(__GNUC__) && (__GNUC__ == 4 && __GNUC_MINOR__ >= 3)
#pragma GCC diagnostic ignored "-Warray-bounds"
#endif

#include "NrrdIO.h"
#include "privateNrrd.h"

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
  static const char me[]="nrrdInvertPerm";
  int problem;
  unsigned int ii;

  if (!(invp && pp && nn > 0)) {
    biffAddf(NRRD, "%s: got NULL pointer or non-positive nn (%d)", me, nn);
    return 1;
  }
  
  /* use the given array "invp" as a temp buffer for validity checking */
  memset(invp, 0, nn*sizeof(int));
  for (ii=0; ii<nn; ii++) {
    if (!( pp[ii] <= nn-1)) {
      biffAddf(NRRD,
               "%s: permutation element #%d == %d out of bounds [0,%d]",
               me, ii, pp[ii], nn-1);
      return 1;
    }
    invp[pp[ii]]++;
  }
  problem = AIR_FALSE;
  for (ii=0; ii<nn; ii++) {
    if (1 != invp[ii]) {
      biffAddf(NRRD, "%s: element #%d mapped to %d times (should be once)",
               me, ii, invp[ii]);
      problem = AIR_TRUE;
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
  static const char me[]="nrrdAxesInsert", func[]="axinsert";
  unsigned int ai;
  
  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!( axis <= nin->dim )) {
    biffAddf(NRRD, "%s: given axis (%d) outside valid range [0, %d]",
             me, axis, nin->dim);
    return 1;
  }
  if (NRRD_DIM_MAX == nin->dim) {
    biffAddf(NRRD, "%s: given nrrd already at NRRD_DIM_MAX (%d)",
             me, NRRD_DIM_MAX);
    return 1;
  }
  if (nout != nin) {
    if (_nrrdCopy(nout, nin, (NRRD_BASIC_INFO_COMMENTS_BIT
                              | (nrrdStateKeyValuePairsPropagate
                                 ? 0
                                 : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT)))) {
      biffAddf(NRRD, "%s:", me);
      return 1;
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
  if (nrrdContentSet_va(nout, func, nin, "%d", axis)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
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
  static const char me[]="nrrdAxesPermute", func[]="permute";
  char buff1[NRRD_DIM_MAX*30], buff2[AIR_STRLEN_SMALL];
  size_t idxOut, idxIn,      /* indices for input and output scanlines */
    lineSize,                /* size of block of memory which can be
                                moved contiguously from input to output,
                                thought of as a "scanline" */
    numLines,                /* how many "scanlines" there are to permute */
    szIn[NRRD_DIM_MAX], *lszIn,
    szOut[NRRD_DIM_MAX], *lszOut,
    cIn[NRRD_DIM_MAX],
    cOut[NRRD_DIM_MAX];
  char *dataIn, *dataOut;
  int axmap[NRRD_DIM_MAX];
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
    biffAddf(NRRD, "%s: got NULL pointer", me);
    airMopError(mop); return 1;
  }
  /* we don't actually need ip[], computing it is for error checking */
  if (nrrdInvertPerm(ip, axes, nin->dim)) {
    biffAddf(NRRD, "%s: couldn't compute axis permutation inverse", me);
    airMopError(mop); return 1;
  }
  /* this shouldn't actually be necessary .. */
  if (!nrrdElementSize(nin)) {
    biffAddf(NRRD, "%s: nrrd reports zero element size!", me);
    airMopError(mop); return 1;
  }
  
  for (ai=0; ai<nin->dim && axes[ai] == ai; ai++)
    ;
  lowPax = ai;

  /* allocate output by initial copy */
  if (nout != nin) {
    if (nrrdCopy(nout, nin)) {
      biffAddf(NRRD, "%s: trouble copying input", me);
      airMopError(mop); return 1;      
    }
    dataIn = (char*)nin->data;
  } else {
    dataIn = (char*)calloc(nrrdElementNumber(nin), nrrdElementSize(nin));
    if (!dataIn) {
      biffAddf(NRRD, "%s: couldn't create local copy of data", me);
      airMopError(mop); return 1;
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
      biffAddf(NRRD, "%s:", me);
      airMopError(mop); return 1;
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
    memset(laxes, 0, NRRD_DIM_MAX*sizeof(unsigned int));
    for (ai=0; ai<ldim; ai++) {
      laxes[ai] = axes[ai+lowPax]-lowPax;
    }
    dataOut = (char *)nout->data;
    memset(cIn, 0, NRRD_DIM_MAX*sizeof(size_t));
    memset(cOut, 0, NRRD_DIM_MAX*sizeof(size_t));
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
    if (nrrdContentSet_va(nout, func, nin, "%s", buff1)) {
      biffAddf(NRRD, "%s:", me);
      airMopError(mop); return 1;
    }
    if (nout != nin) {
      if (nrrdBasicInfoCopy(nout, nin,
                            NRRD_BASIC_INFO_DATA_BIT
                            | NRRD_BASIC_INFO_TYPE_BIT
                            | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                            | NRRD_BASIC_INFO_DIMENSION_BIT
                            | NRRD_BASIC_INFO_CONTENT_BIT
                            | NRRD_BASIC_INFO_COMMENTS_BIT
                            | (nrrdStateKeyValuePairsPropagate
                               ? 0
                               : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT))) {
        biffAddf(NRRD, "%s:", me);
        airMopError(mop); return 1;
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
  static const char me[]="nrrdShuffle", func[]="shuffle";
  char buff2[AIR_STRLEN_SMALL];
  /* Sun Feb 8 13:13:58 CST 2009: There was a memory bug here caused
     by using the same buff1[NRRD_DIM_MAX*30] declaration that had
     worked fine for nrrdAxesPermute and nrrdReshape, but does NOT
     work here because now samples along an axes are re-ordered, not
     axes, so its often not allocated for long enough to hold the
     string that's printed to it. Ideally there'd be another argument
     that says whether to document the shuffle in the content string,
     which would mean an API change.  Or, we can use a secret
     heuristic (or maybe later a nrrdState variable) for determining
     when an axis is short enough to make documenting the shuffle
     interesting.  This is useful since functions like nrrdFlip()
     probably do *not* need the shuffle (the sample reversal) to be
     documented for long axes */
#define LONGEST_INTERESTING_AXIS 42
  char buff1[LONGEST_INTERESTING_AXIS*30];
  unsigned int 
    ai, ldim, len,
    cIn[NRRD_DIM_MAX+1],
    cOut[NRRD_DIM_MAX+1];
  size_t idxIn, idxOut, lineSize, numLines, size[NRRD_DIM_MAX], *lsize;
  char *dataIn, *dataOut;

  if (!(nin && nout && perm)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (nout == nin) {
    biffAddf(NRRD, "%s: nout==nin disallowed", me);
    return 1;
  }
  if (!( axis < nin->dim )) {
    biffAddf(NRRD, "%s: axis %d outside valid range [0,%d]", 
             me, axis, nin->dim-1);
    return 1;
  }
  len = AIR_CAST(unsigned int, nin->axis[axis].size);
  for (ai=0; ai<len; ai++) {
    if (!( perm[ai] < len )) {
      biffAddf(NRRD, "%s: perm[%d] (" _AIR_SIZE_T_CNV
               ") outside valid range [0,%d]", me, ai, perm[ai], len-1);
      return 1;
    }
  }
  /* this shouldn't actually be necessary .. */
  if (!nrrdElementSize(nin)) {
    biffAddf(NRRD, "%s: nrrd reports zero element size!", me);
    return 1;
  }
  /* set information in new volume */
  nout->blockSize = nin->blockSize;
  nrrdAxisInfoGet_nva(nin, nrrdAxisInfoSize, size);
  if (nrrdMaybeAlloc_nva(nout, nin->type, nin->dim, size)) {
    biffAddf(NRRD, "%s: failed to allocate output", me);
    return 1;
  }
  if (nrrdAxisInfoCopy(nout, nin, NULL, NRRD_AXIS_INFO_NONE)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  /* the min and max along the shuffled axis are now meaningless */
  nout->axis[axis].min = nout->axis[axis].max = AIR_NAN;
  /* do the safe thing first */
  nout->axis[axis].kind = _nrrdKindAltered(nin->axis[axis].kind, AIR_FALSE);
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
    cIn[0] = AIR_CAST(unsigned int, perm[cOut[0]]);
    NRRD_INDEX_GEN(idxIn, cIn, lsize, ldim);
    NRRD_INDEX_GEN(idxOut, cOut, lsize, ldim);
    memcpy(dataOut + idxOut*lineSize, dataIn + idxIn*lineSize, lineSize);
    NRRD_COORD_INCR(cOut, lsize, ldim, 0);
  }
  /* Set content. The LONGEST_INTERESTING_AXIS hack avoids the
     previous array out-of-bounds bug */
  if (len <= LONGEST_INTERESTING_AXIS) {
    strcpy(buff1, "");
    for (ai=0; ai<len; ai++) {
      sprintf(buff2, "%s" _AIR_SIZE_T_CNV, (ai ? "," : ""), perm[ai]);
      strcat(buff1, buff2);
    }
    if (nrrdContentSet_va(nout, func, nin, "%s", buff1)) {
      biffAddf(NRRD, "%s:", me);
      return 1;
    }
  } else {
    if (nrrdContentSet_va(nout, func, nin, "")) {
      biffAddf(NRRD, "%s:", me);
      return 1;
    }
  }
  if (nrrdBasicInfoCopy(nout, nin,
                        NRRD_BASIC_INFO_DATA_BIT
                        | NRRD_BASIC_INFO_TYPE_BIT
                        | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                        | NRRD_BASIC_INFO_DIMENSION_BIT
                        | NRRD_BASIC_INFO_CONTENT_BIT
                        | NRRD_BASIC_INFO_COMMENTS_BIT
                        | (nrrdStateKeyValuePairsPropagate
                           ? 0
                           : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT))) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  
  return 0;
#undef LONGEST_INTERESTING_AXIS
}

/* ---- BEGIN non-NrrdIO */


/*
******** nrrdAxesSwap()
**
** for when you just want to switch the order of two axes, without
** going through the trouble of creating the permutation array 
** needed to call nrrdAxesPermute()
*/
int
nrrdAxesSwap(Nrrd *nout, const Nrrd *nin, unsigned int ax1, unsigned int ax2) {
  static const char me[]="nrrdAxesSwap", func[]="swap";
  unsigned int ai, axmap[NRRD_DIM_MAX];

  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!( ax1 < nin->dim && ax2 < nin->dim )) {
    biffAddf(NRRD, "%s: ax1 (%d) or ax2 (%d) out of bounds [0,%d]", 
             me, ax1, ax2, nin->dim-1);
    return 1;
  }

  for (ai=0; ai<nin->dim; ai++) {
    axmap[ai] = ai;
  }
  axmap[ax2] = ax1;
  axmap[ax1] = ax2;
  if (nrrdAxesPermute(nout, nin, axmap)
      || nrrdContentSet_va(nout, func, nin, "%d,%d", ax1, ax2)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  /* basic info already copied by nrrdAxesPermute */
  return 0;
}

/*
******** nrrdFlip()
**
** reverse the order of slices along the given axis.
** Actually, just a wrapper around nrrdShuffle() (with some
** extra setting of axis info)
*/
int
nrrdFlip(Nrrd *nout, const Nrrd *nin, unsigned int axis) {
  static const char me[]="nrrdFlip", func[]="flip";
  size_t *perm, si;
  airArray *mop;
  unsigned int axisIdx;

  mop = airMopNew();
  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    airMopError(mop); return 1;
  }
  if (!(  axis < nin->dim )) {
    biffAddf(NRRD, "%s: given axis (%d) is outside valid range ([0,%d])", 
             me, axis, nin->dim-1);
    airMopError(mop); return 1;
  }
  if (!(perm = (size_t*)calloc(nin->axis[axis].size, sizeof(size_t)))) {
    biffAddf(NRRD, "%s: couldn't alloc permutation array", me);
    airMopError(mop); return 1;
  }
  airMopAdd(mop, perm, airFree, airMopAlways);
  for (si=0; si<nin->axis[axis].size; si++) {
    perm[si] = nin->axis[axis].size-1-si;
  }
  /* nrrdBasicInfoCopy called by nrrdShuffle() */
  if (nrrdShuffle(nout, nin, axis, perm)
      || nrrdContentSet_va(nout, func, nin, "%d", axis)) {
    biffAddf(NRRD, "%s:", me);
    airMopError(mop); return 1;
  }
  _nrrdAxisInfoCopy(&(nout->axis[axis]), &(nin->axis[axis]),
                    NRRD_AXIS_INFO_SIZE_BIT
                    | NRRD_AXIS_INFO_KIND_BIT);
  /* HEY: (Tue Jan 18 00:28:26 EST 2005) there's a basic question to
     be answered here: do we want to keep the "location" of the
     samples fixed, while changing their ordering, or do want to flip
     the location of the samples?  In the former, the position
     information has to be flipped to cancel the flipping of the the
     sample order, so that samples maintain location.  In the latter,
     the position information is copied verbatim from the original.  */
  /* (Tue Sep 13 09:59:12 EDT 2005) answer: we keep the "location" of
     the samples fixed, while changing their ordering.  This is the 
     low-level thing to do, so for a nrrd function, its the right thing
     to do.  You don't need a nrrd function to simply manipulate 
     per-axis meta-information */
  nout->axis[axis].min = nin->axis[axis].max;
  nout->axis[axis].max = nin->axis[axis].min;
  /* HEY: Fri Jan 14 02:53:30 EST 2005: isn't spacing supposed to be
     the step from one sample to the next?  So its a signed quantity.
     If min and max can be flipped (so min > max), then spacing can
     be negative, right?  */
  nout->axis[axis].spacing = -nin->axis[axis].spacing;
  /* HEY: Fri Jan 14 02:53:30 EST 2005: but not thickness */
  nout->axis[axis].thickness = nin->axis[axis].thickness;
  /* need to set general orientation info too */
  for (axisIdx=0; axisIdx<NRRD_SPACE_DIM_MAX; axisIdx++) {
    nout->axis[axis].spaceDirection[axisIdx] = 
      -nin->axis[axis].spaceDirection[axisIdx];
  }
  /* modify origin only if we flipped a spatial axis */
  if (AIR_EXISTS(nin->axis[axis].spaceDirection[0])) {
    nrrdSpaceVecScaleAdd2(nout->spaceOrigin,
                          1.0,
                          nin->spaceOrigin,
                          AIR_CAST(double, nin->axis[axis].size-1),
                          nin->axis[axis].spaceDirection);
  } else {
    nrrdSpaceVecCopy(nout->spaceOrigin, nin->spaceOrigin);
  }
  airMopOkay(mop); 
  return 0;
}

/*
**
** NOTE: this seems to destroy all space/orientation info.  What
** should be done? 
*/
int
nrrdJoin(Nrrd *nout, const Nrrd *const *nin, unsigned int numNin,
         unsigned int axis, int incrDim) {
  static const char me[]="nrrdJoin";
  unsigned int ni, ai, mindim, maxdim, outdim,
    permute[NRRD_DIM_MAX], ipermute[NRRD_DIM_MAX];
  int diffdim, axmap[NRRD_DIM_MAX];
  size_t outlen, outnum, chunksize, size[NRRD_DIM_MAX];
  char *dataPerm;
  Nrrd *ntmpperm,    /* axis-permuted version of output */
    **ninperm;
  airArray *mop;

  /* error checking */
  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!(numNin >= 1)) {
    biffAddf(NRRD, "%s: numNin (%d) must be >= 1", me, numNin);
    return 1;
  }
  for (ni=0; ni<numNin; ni++) {
    if (!(nin[ni])) {
      biffAddf(NRRD, "%s: input nrrd #%d NULL", me, ni);
      return 1;
    }
    if (nout==nin[ni]) {
      biffAddf(NRRD, "%s: nout==nin[%d] disallowed", me, ni);
      return 1;
    }
  }

  mop = airMopNew();
  ninperm = (Nrrd **)calloc(numNin, sizeof(Nrrd *));
  if (!(ninperm)) {
    biffAddf(NRRD, "%s: couldn't calloc() temp nrrd array", me);
    airMopError(mop); return 1;
  }
  airMopAdd(mop, ninperm, airFree, airMopAlways);

  maxdim = mindim = nin[0]->dim;
  for (ni=0; ni<numNin; ni++) {
    mindim = AIR_MIN(mindim, nin[ni]->dim);
    maxdim = AIR_MAX(maxdim, nin[ni]->dim);
  }
  diffdim = maxdim - mindim;
  if (diffdim > 1) {
    biffAddf(NRRD, "%s: will only reshape up one dimension (not %d)",
             me, diffdim);
    airMopError(mop); return 1;
  }
  if (axis > maxdim) {
    biffAddf(NRRD, "%s: can't join along axis %d with highest input dim = %d",
             me, axis, maxdim);
    airMopError(mop); return 1;
  }

  /* figure out dimension of output (outdim) */
  if (diffdim) {
    /* case A: (example) 2D slices and 3D slabs are being joined
       together to make a bigger 3D volume */
    outdim = maxdim;
  } else {
    /* diffdim == 0 */
    if (axis == maxdim) {
      /* case B: this is like the old "stitch": a bunch of equal-sized
         slices of dimension N are being stacked together to make an
         N+1 dimensional volume, which is essentially just the result of
         concatenating the memory of individual inputs */
      outdim = maxdim + 1;
    } else {
      /* case C: axis < maxdim; maxdim == mindim */
      /* case C1 (!incrDim): a bunch of N-D slabs are being joined
         together to make a bigger N-D volume.  The axis along which
         they are being joined could be any of existing axes (from 0
         to maxdim-1) */
      /* case C2 (incrDim): this is also a "stitch", but the new axis
         created by the stitching is inserted into the existing
         axes. (ex: stitch 3 PGMs (R, G, B) together into a PPM (with
         color on axis zero) */
      outdim = maxdim + !!incrDim;
    }
  }
  if (outdim > NRRD_DIM_MAX) {
    biffAddf(NRRD, "%s: output dimension (%d) exceeds NRRD_DIM_MAX (%d)",
             me, outdim, NRRD_DIM_MAX);
    airMopError(mop); return 1;    
  }
  
  /* do tacit reshaping, and possibly permuting, as needed */
  for (ai=0; ai<outdim; ai++) {
    permute[ai] = (ai < axis
                   ? ai 
                   : (ai < outdim-1
                      ? ai + 1
                      : axis));
    /* fprintf(stderr, "!%s: 1st permute[%d] = %d\n", me, ai, permute[ai]); */
  }
  for (ni=0; ni<numNin; ni++) {
    ninperm[ni] = nrrdNew();
    diffdim = outdim - nin[ni]->dim;
    /* fprintf(stderr, "!%s: ni = %d ---> diffdim = %d\n", me, ni, diffdim); */
    if (diffdim) {
      /* we do a tacit reshaping, which actually includes
         a tacit permuting, so we don't have to call permute
         on the parts that don't actually need it */
      /* NB: we register nrrdNix, not nrrdNuke */
      /* fprintf(stderr, "!%s: %d: tacit reshape/permute\n", me, ni); */
      airMopAdd(mop, ninperm[ni], (airMopper)nrrdNix, airMopAlways);
      nrrdAxisInfoGet_nva(nin[ni], nrrdAxisInfoSize, size);
      for (ai=nin[ni]->dim-1; ai>=mindim+1; ai--) {
        size[ai] = size[ai-1];
      }
      size[mindim] = 1;
      /* this may be done needlessly often */
      for (ai=0; ai<=nin[ni]->dim; ai++) {
        if (ai < mindim) {
          axmap[ai] = ai;
        } else if (ai > mindim) {
          axmap[ai] = ai-1;
        } else {
          axmap[ai] = -1;
        }
      }
      /* we don't have to actually call nrrdReshape(): we just nrrdWrap()
         the input data with the reshaped size array */
      if (nrrdWrap_nva(ninperm[ni], nin[ni]->data, nin[ni]->type,
                       nin[ni]->dim+1, size)) {
        biffAddf(NRRD, "%s: trouble creating interm. version of nrrd %d",
                 me, ni);
        airMopError(mop); return 1;    
      }
      nrrdAxisInfoCopy(ninperm[ni], nin[ni], axmap, 
                       (NRRD_AXIS_INFO_SIZE_BIT
                        /* HEY: this is being nixed because I can't think
                           of a sane way of keeping it consistent */
                        | NRRD_AXIS_INFO_SPACEDIRECTION_BIT));
    } else {
      /* on this part, we permute (no need for a reshape) */
      airMopAdd(mop, ninperm[ni], (airMopper)nrrdNuke, airMopAlways);
      if (nrrdAxesPermute(ninperm[ni], nin[ni], permute)) {
        biffAddf(NRRD, "%s: trouble permuting input part %d", me, ni);
        airMopError(mop); return 1;
      }
    }
  }

  /* make sure all parts are compatible in type and shape,
     determine length of final output along axis (outlen) */
  outlen = 0;
  for (ni=0; ni<numNin; ni++) {
    if (ninperm[ni]->type != ninperm[0]->type) {
      biffAddf(NRRD, "%s: type (%s) of part %d unlike first's (%s)",
               me, airEnumStr(nrrdType, ninperm[ni]->type),
               ni, airEnumStr(nrrdType, ninperm[0]->type));
      airMopError(mop); return 1;
    }
    if (nrrdTypeBlock == ninperm[0]->type) {
      if (ninperm[ni]->blockSize != ninperm[0]->blockSize) {
        biffAddf(NRRD, "%s: blockSize (" _AIR_SIZE_T_CNV 
                 ") of part %d unlike first's (" _AIR_SIZE_T_CNV ")",
                 me, ninperm[ni]->blockSize, ni, ninperm[0]->blockSize);
        airMopError(mop); return 1;
      }
    }
    if (!nrrdElementSize(ninperm[ni])) {
      biffAddf(NRRD, "%s: got wacky elements size (" _AIR_SIZE_T_CNV 
               ") for part %d", me, nrrdElementSize(ninperm[ni]), ni);
      airMopError(mop); return 1;
    }
    
    /* fprintf(stderr, "!%s: part %03d shape: ", me, ni); */
    for (ai=0; ai<outdim-1; ai++) {
      /* fprintf(stderr, "%03u ", (unsigned int)ninperm[ni]->axis[ai].size);*/
      if (ninperm[ni]->axis[ai].size != ninperm[0]->axis[ai].size) {
        biffAddf(NRRD, "%s: axis %d size (" _AIR_SIZE_T_CNV 
                 ") of part %d unlike first's (" _AIR_SIZE_T_CNV ")",
                 me, ai, ninperm[ni]->axis[ai].size,
                 ni, ninperm[0]->axis[ai].size);
        airMopError(mop); return 1;
      }
    }
    /*
    fprintf(stderr, "%03u\n", (unsigned int)ninperm[ni]->axis[outdim-1].size);
    */
    outlen += ninperm[ni]->axis[outdim-1].size;
  }
  /* fprintf(stderr, "!%s: outlen = %u\n", me, (unsigned int)outlen); */

  /* allocate temporary nrrd and concat input into it */
  outnum = 1;
  if (outdim > 1) {
    for (ai=0; ai<outdim-1; ai++) {
      size[ai] = ninperm[0]->axis[ai].size;
      outnum *= size[ai];
    }
  }
  size[outdim-1] = outlen;
  outnum *= size[outdim-1];
  if (nrrdMaybeAlloc_nva(ntmpperm = nrrdNew(), ninperm[0]->type,
                         outdim, size)) {
    biffAddf(NRRD, "%s: trouble allocating permutation nrrd", me);
    airMopError(mop); return 1;
  }
  airMopAdd(mop, ntmpperm, (airMopper)nrrdNuke, airMopAlways);
  dataPerm = (char *)ntmpperm->data;
  for (ni=0; ni<numNin; ni++) {
    /* here is where the actual joining happens */
    chunksize = nrrdElementNumber(ninperm[ni])*nrrdElementSize(ninperm[ni]);
    memcpy(dataPerm, ninperm[ni]->data, chunksize);
    dataPerm += chunksize;
  }
  
  /* copy other axis-specific fields from nin[0] to ntmpperm */
  for (ai=0; ai<outdim-1; ai++) {
    axmap[ai] = ai;
  }
  axmap[outdim-1] = -1;
  nrrdAxisInfoCopy(ntmpperm, ninperm[0], axmap, 
                   (NRRD_AXIS_INFO_NONE
                    /* HEY: this is being nixed because I can't think
                       of a sane way of keeping it consistent */
                    | NRRD_AXIS_INFO_SPACEDIRECTION_BIT));
  ntmpperm->axis[outdim-1].size = outlen;

  /* do the permutation required to get output in right order */
  if (nrrdInvertPerm(ipermute, permute, outdim)
      || nrrdAxesPermute(nout, ntmpperm, ipermute)) {
    biffAddf(NRRD, "%s: error permuting temporary nrrd", me);
    airMopError(mop); return 1;
  }
  /* basic info is either already set or invalidated by joining */

  /* HEY: set content on output! */

  airMopOkay(mop); 
  return 0;
}

/*
******** nrrdAxesSplit
**
** like reshape, but only for splitting one axis into a fast and slow part.
*/
int
nrrdAxesSplit(Nrrd *nout, const Nrrd *nin,
              unsigned int saxi, size_t sizeFast, size_t sizeSlow) {
  static const char me[]="nrrdAxesSplit", func[]="axsplit";
  unsigned int ai;
  
  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!( saxi <= nin->dim-1 )) {
    biffAddf(NRRD, "%s: given axis (%d) outside valid range [0, %d]",
             me, saxi, nin->dim-1);
    return 1;
  }
  if (NRRD_DIM_MAX == nin->dim) {
    biffAddf(NRRD, "%s: given nrrd already at NRRD_DIM_MAX (%d)",
             me, NRRD_DIM_MAX);
    return 1;
  }
  if (!(sizeFast*sizeSlow == nin->axis[saxi].size)) {
    biffAddf(NRRD, "%s: # samples along axis %d (" _AIR_SIZE_T_CNV
             ") != product of fast and slow sizes (" _AIR_SIZE_T_CNV 
             " * " _AIR_SIZE_T_CNV " = " _AIR_SIZE_T_CNV ")",
             me, saxi, nin->axis[saxi].size,
             sizeFast, sizeSlow, sizeFast*sizeSlow);
    return 1;
  }
  if (nout != nin) {
    if (_nrrdCopy(nout, nin, (NRRD_BASIC_INFO_COMMENTS_BIT
                              | (nrrdStateKeyValuePairsPropagate
                                 ? 0
                                 : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT)))) {
      biffAddf(NRRD, "%s:", me);
      return 1;
    }
  }
  nout->dim = 1 + nin->dim;
  for (ai=nin->dim-1; ai>=saxi+1; ai--) {
    _nrrdAxisInfoCopy(&(nout->axis[ai+1]), &(nin->axis[ai]),
                      NRRD_AXIS_INFO_NONE);
  }
  /* the ONLY thing we can say about the new axes are their sizes */
  _nrrdAxisInfoInit(&(nout->axis[saxi]));
  _nrrdAxisInfoInit(&(nout->axis[saxi+1]));
  nout->axis[saxi].size = sizeFast;
  nout->axis[saxi+1].size = sizeSlow;
  if (nrrdContentSet_va(nout, func, nin, "%d,%d,%d",
                        saxi, sizeFast, sizeSlow)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  /* all basic information already copied by nrrdCopy */
  return 0;
}

/*
******** nrrdAxesDelete
**
** like reshape, but preserves axis information on old axes, and
** this is only for removing a "stub" axis with length 1.
*/
int
nrrdAxesDelete(Nrrd *nout, const Nrrd *nin, unsigned int daxi) {
  static const char me[]="nrrdAxesDelete", func[]="axdelete";
  unsigned int ai;
  
  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!( daxi < nin->dim )) {
    biffAddf(NRRD, "%s: given axis (%d) outside valid range [0, %d]",
             me, daxi, nin->dim-1);
    return 1;
  }
  if (1 == nin->dim) {
    biffAddf(NRRD, "%s: given nrrd already at lowest dimension (1)", me);
    return 1;
  }
  if (1 != nin->axis[daxi].size) {
    biffAddf(NRRD, "%s: size along axis %d is " _AIR_SIZE_T_CNV ", not 1",
             me, daxi, nin->axis[daxi].size);
    return 1;
  }
  if (nout != nin) {
    if (_nrrdCopy(nout, nin, (NRRD_BASIC_INFO_COMMENTS_BIT
                              | (nrrdStateKeyValuePairsPropagate
                                 ? 0
                                 : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT)))) {
      biffAddf(NRRD, "%s:", me);
      return 1;
    }
  }
  for (ai=daxi; ai<nin->dim-1; ai++) {
    _nrrdAxisInfoCopy(&(nout->axis[ai]), &(nin->axis[ai+1]),
                      NRRD_AXIS_INFO_NONE);
  }
  nout->dim = nin->dim - 1;
  if (nrrdContentSet_va(nout, func, nin, "%d", daxi)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  /* all basic information already copied by nrrdCopy */
  return 0;
}

/*
******** nrrdAxesMerge
**
** like reshape, but preserves axis information on old axes
** merges axis ax and ax+1 into one 
*/
int
nrrdAxesMerge(Nrrd *nout, const Nrrd *nin, unsigned int maxi) {
  static const char me[]="nrrdAxesMerge", func[]="axmerge";
  unsigned int ai;
  size_t sizeFast, sizeSlow;
  
  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!( maxi < nin->dim-1 )) {
    biffAddf(NRRD, "%s: given axis (%d) outside valid range [0, %d]",
             me, maxi, nin->dim-2);
    return 1;
  }
  if (1 == nin->dim) {
    biffAddf(NRRD, "%s: given nrrd already at lowest dimension (1)", me);
    return 1;
  }
  if (nout != nin) {
    if (_nrrdCopy(nout, nin, (NRRD_BASIC_INFO_COMMENTS_BIT
                              | (nrrdStateKeyValuePairsPropagate
                                 ? 0
                                 : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT)))) {
      biffAddf(NRRD, "%s:", me);
      return 1;
    }
  }
  sizeFast = nin->axis[maxi].size;
  sizeSlow = nin->axis[maxi+1].size;
  nout->dim = nin->dim - 1;
  for (ai=maxi+1; ai<nout->dim; ai++) {
    _nrrdAxisInfoCopy(&(nout->axis[ai]), &(nin->axis[ai+1]),
                      NRRD_AXIS_INFO_NONE);
  }
  /* the ONLY thing we can say about the new axis is its size */
  _nrrdAxisInfoInit(&(nout->axis[maxi]));
  nout->axis[maxi].size = sizeFast*sizeSlow;
  if (nrrdContentSet_va(nout, func, nin, "%d", maxi)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  /* all basic information already copied by nrrdCopy */
  return 0;
}


/*
******** nrrdReshape_nva()
**
*/
int
nrrdReshape_nva(Nrrd *nout, const Nrrd *nin,
                unsigned int dim, const size_t *size) {
  static const char me[]="nrrdReshape_nva", func[]="reshape";
  char buff1[NRRD_DIM_MAX*30], buff2[AIR_STRLEN_SMALL];
  size_t numOut;
  unsigned int ai;
  
  if (!(nout && nin && size)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!(AIR_IN_CL(1, dim, NRRD_DIM_MAX))) {
    biffAddf(NRRD, "%s: given dimension (%d) outside valid range [1,%d]",
             me, dim, NRRD_DIM_MAX);
    return 1;
  }
  if (_nrrdSizeCheck(size, dim, AIR_TRUE)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  numOut = 1;
  for (ai=0; ai<dim; ai++) {
    numOut *= size[ai];
  }
  if (numOut != nrrdElementNumber(nin)) {
    biffAddf(NRRD, "%s: new sizes product (" _AIR_SIZE_T_CNV ") "
             "!= # elements (" _AIR_SIZE_T_CNV ")",
             me, numOut, nrrdElementNumber(nin));
    return 1;
  }

  if (nout != nin) {
    if (_nrrdCopy(nout, nin, (NRRD_BASIC_INFO_COMMENTS_BIT
                              | (nrrdStateKeyValuePairsPropagate
                                 ? 0
                                 : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT)))) {
      biffAddf(NRRD, "%s:", me);
      return 1;
    }
  }
  nout->dim = dim;
  for (ai=0; ai<dim; ai++) {
    /* the ONLY thing we can say about the axes is the size */
    _nrrdAxisInfoInit(&(nout->axis[ai]));
    nout->axis[ai].size = size[ai];
  }

  strcpy(buff1, "");
  for (ai=0; ai<dim; ai++) {
    sprintf(buff2, "%s" _AIR_SIZE_T_CNV, (ai ? "x" : ""), size[ai]);
    strcat(buff1, buff2);
  }
  /* basic info copied by _nrrdCopy, but probably more than we 
     want- perhaps space dimension and origin should be nixed? */
  if (nrrdContentSet_va(nout, func, nin, "%s", buff1)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  return 0;
}

/*
******** nrrdReshape_va()
**
** var-args version of nrrdReshape_nva()
*/
int
nrrdReshape_va(Nrrd *nout, const Nrrd *nin, unsigned int dim, ...) {
  static const char me[]="nrrdReshape_va";
  unsigned int ai;
  size_t size[NRRD_DIM_MAX];
  va_list ap;

  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (!(AIR_IN_CL(1, dim, NRRD_DIM_MAX))) {
    biffAddf(NRRD, "%s: given dimension (%d) outside valid range [1,%d]",
             me, dim, NRRD_DIM_MAX);
    return 1;
  }
  va_start(ap, dim);
  for (ai=0; ai<dim; ai++) {
    size[ai] = va_arg(ap, size_t);
  }
  va_end(ap);
  /* basic info copied (indirectly) by nrrdReshape_nva() */
  if (nrrdReshape_nva(nout, nin, dim, size)) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  
  return 0;
}

/*
******** nrrdBlock()
**
** collapse the first axis (axis 0) of the nrrd into a block, making
** an output nrrd of type nrrdTypeBlock.  The input type can be block.
** All information for other axes is shifted down one axis.
*/
int
nrrdBlock(Nrrd *nout, const Nrrd *nin) {
  static const char me[]="nrrdBlock", func[]="block";
  unsigned int ai;
  size_t numEl, size[NRRD_DIM_MAX];
  int map[NRRD_DIM_MAX];

  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (nout == nin) {
    biffAddf(NRRD, "%s: due to laziness, nout==nin disallowed", me);
    return 1;
  }
  if (1 == nin->dim) {
    biffAddf(NRRD, "%s: can't blockify 1-D nrrd", me);
    return 1;
  }
  /* this shouldn't actually be necessary .. */
  if (!nrrdElementSize(nin)) {
    biffAddf(NRRD, "%s: nrrd reports zero element size!", me);
    return 1;
  }
  
  numEl = nin->axis[0].size;;
  nout->blockSize = numEl*nrrdElementSize(nin);
  /*
  fprintf(stderr, "!%s: nout->blockSize = %d * %d = %d\n", me,
          numEl, nrrdElementSize(nin), nout->blockSize);
  */
  for (ai=0; ai<nin->dim-1; ai++) {
    map[ai] = ai+1;
    size[ai] = nin->axis[map[ai]].size;
  }

  /* nout->blockSize set above */
  if (nrrdMaybeAlloc_nva(nout, nrrdTypeBlock, nin->dim-1, size)) {
    biffAddf(NRRD, "%s: failed to allocate output", me);
    return 1;
  }
  memcpy(nout->data, nin->data, nrrdElementNumber(nin)*nrrdElementSize(nin));
  if (nrrdAxisInfoCopy(nout, nin, map, NRRD_AXIS_INFO_NONE)) {
    biffAddf(NRRD, "%s: failed to copy axes", me);
    return 1;
  }
  if (nrrdContentSet_va(nout, func, nin, "")) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  if (nrrdBasicInfoCopy(nout, nin,
                        NRRD_BASIC_INFO_DATA_BIT
                        | NRRD_BASIC_INFO_TYPE_BIT
                        | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                        | NRRD_BASIC_INFO_DIMENSION_BIT
                        | NRRD_BASIC_INFO_CONTENT_BIT
                        | NRRD_BASIC_INFO_COMMENTS_BIT
                        | (nrrdStateKeyValuePairsPropagate
                           ? 0
                           : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT))) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  return 0;
}

/*
******** nrrdUnblock()
**
** takes a nrrdTypeBlock nrrd and breaks the blocks into elements of 
** type "type", and shifts other axis information up by one axis
*/
int
nrrdUnblock(Nrrd *nout, const Nrrd *nin, int type) {
  static const char me[]="nrrdUnblock", func[]="unblock";
  unsigned int dim;
  size_t size[NRRD_DIM_MAX], outElSz;
  int map[NRRD_DIM_MAX];

  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (nout == nin) {
    biffAddf(NRRD, "%s: due to laziness, nout==nin disallowed", me);
    return 1;
  }
  if (nrrdTypeBlock != nin->type) {
    biffAddf(NRRD, "%s: need input nrrd type %s", me,
             airEnumStr(nrrdType, nrrdTypeBlock));
    return 1;
  }
  if (NRRD_DIM_MAX == nin->dim) {
    biffAddf(NRRD, "%s: input nrrd already at dimension limit (%d)",
             me, NRRD_DIM_MAX);
    return 1;
  }
  if (airEnumValCheck(nrrdType, type)) {
    biffAddf(NRRD, "%s: invalid requested type %d", me, type);
    return 1;
  }
  if (nrrdTypeBlock == type && (!(0 < nout->blockSize))) {
    biffAddf(NRRD, "%s: for %s type, need nout->blockSize set", me,
             airEnumStr(nrrdType, nrrdTypeBlock));
    return 1;
  }
  /* this shouldn't actually be necessary .. */
  if (!(nrrdElementSize(nin))) {
    biffAddf(NRRD, "%s: nin or nout reports zero element size!", me);
    return 1;
  }
  
  nout->type = type;
  outElSz = nrrdElementSize(nout);
  if (nin->blockSize % outElSz) {
    biffAddf(NRRD, "%s: input blockSize (" _AIR_SIZE_T_CNV 
             ") not multiple of output element size (" _AIR_SIZE_T_CNV  ")",
             me, nin->blockSize, outElSz);
    return 1;
  }
  for (dim=0; dim<=nin->dim; dim++) {
    map[dim] = !dim ?  -1 : (int)dim-1;
    size[dim] = !dim ? nin->blockSize / outElSz : nin->axis[map[dim]].size;
  }
  /* if nout->blockSize is needed, we've checked that its set */
  if (nrrdMaybeAlloc_nva(nout, type, nin->dim+1, size)) {
    biffAddf(NRRD, "%s: failed to allocate output", me);
    return 1;
  }
  memcpy(nout->data, nin->data, nrrdElementNumber(nin)*nrrdElementSize(nin));
  if (nrrdAxisInfoCopy(nout, nin, map, NRRD_AXIS_INFO_NONE)) {
    biffAddf(NRRD, "%s: failed to copy axes", me);
    return 1;
  }
  if (nrrdContentSet_va(nout, func, nin, "")) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  if (nrrdBasicInfoCopy(nout, nin,
                        NRRD_BASIC_INFO_DATA_BIT
                        | NRRD_BASIC_INFO_TYPE_BIT
                        | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                        | NRRD_BASIC_INFO_DIMENSION_BIT
                        | NRRD_BASIC_INFO_CONTENT_BIT
                        | NRRD_BASIC_INFO_COMMENTS_BIT
                        | (nrrdStateKeyValuePairsPropagate
                           ? 0
                           : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT))) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  return 0;
}

/* for nrrdTile ..

will require that # slices be <= number of images: won't crop for you,
but will happy pad with black.  This will be handled in another
function.  Probably unu tile.

*/

/*
******** nrrdTile2D()
**
** Splits axis axSplit into two pieces of size sizeFast and sizeSlow.
** The data from the fast partition is juxtaposed following ax0, the
** slow after ax1.  nrrdAxesMerge is then called to join ax0 and ax1
** with their respective newly permuted data.  There should be one
** fewer dimensions in the output nrrd than in the input nrrd.
*/
int
nrrdTile2D(Nrrd *nout, const Nrrd *nin, unsigned int ax0, unsigned int ax1,
           unsigned int axSplit, size_t sizeFast, size_t sizeSlow) {
  static const char me[]="nrrdTile2D";
  int E,                     /* error flag */
    insAxis[2*NRRD_DIM_MAX], /* array for inserting the two axes resulting
                                from the initial split amongst the other
                                axes: inserted axes go in odd slots, 
                                other axes go in even slots */
    mapIdx,                  /* index for filling map[] */
    merge[2],                /* two axes to be merged post-permute */
    mergeIdx;                /* index for filling merge[] */
  unsigned int ii,
    map[NRRD_DIM_MAX];       /* axis map for axis permute */

  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  
  /* at least for now, axSplit, ax0, and ax1 need to be distinct */
  if (!( axSplit != ax0 
         && axSplit != ax1 
         && ax0 != ax1 )) {
    biffAddf(NRRD, "%s: axSplit, ax0, ax1 (%d,%d,%d) must be distinct",
             me, axSplit, ax0, ax1);
    return 1;
  }
  if (!( ax0 < nin->dim
         && ax1 < nin->dim
         && axSplit < nin->dim )) {
    biffAddf(NRRD, "%s: axSplit, ax0, ax1 (%d,%d,%d) must be in range [0,%d]",
             me, axSplit, ax0, ax1, nin->dim-1);
    return 1;
  }
  
  if (nout != nin) {
    if (_nrrdCopy(nout, nin, (NRRD_BASIC_INFO_COMMENTS_BIT
                              | (nrrdStateKeyValuePairsPropagate
                                 ? 0
                                 : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT)))) {
      biffAddf(NRRD, "%s:", me);
      return 1;
    }
  }

  /* increment ax0 and ax1 if they're above axSplit, since the
     initial axis split will bump up the corresponding axes */
  ax0 += (axSplit < ax0);
  ax1 += (axSplit < ax1);
  /* initialize insAxis to all invalid (blank) values */
  for (ii=0; ii<2*(nout->dim+1); ii++) {
    insAxis[ii] = -1;
  }
  /* run through post-split axes, inserting axSplit and axSplit+1
     into the slots after ax0 and ax1 respectively, otherwise
     set the identity map */
  for (ii=0; ii<(nout->dim+1); ii++) {
    if (axSplit == ii) {
      insAxis[2*ax0 + 1] = axSplit;
    } else if (axSplit+1 == ii) {
      insAxis[2*ax1 + 1] = axSplit+1;
    } else {
      insAxis[2*ii + 0] = ii;
    }
  }
  /* settle the values from insAxis[] into map[] by removing the -1's */
  mergeIdx = mapIdx = 0;
  for (ii=0; ii<2*(nout->dim+1); ii++) {
    if (insAxis[ii] != -1) {
      if (1 == ii % 2) {
        /* its an odd entry in insAxis[], so the previous axis is to be
           merged.  Using mapIdx-1 is legit because we disallow
           axSplit == ax{0,1} */
        merge[mergeIdx++] = mapIdx-1;
      }
      map[mapIdx++] = insAxis[ii];
    }
  }

  E = AIR_FALSE;
  if (!E) E |= nrrdAxesSplit(nout, nout, axSplit, sizeFast, sizeSlow);
  if (!E) E |= nrrdAxesPermute(nout, nout, map);
  if (!E) E |= nrrdAxesMerge(nout, nout, merge[1]);    
  if (!E) E |= nrrdAxesMerge(nout, nout, merge[0]);
  if (E) {
    biffAddf(NRRD, "%s: trouble", me);
    return 1;
  }
  /* HEY: set content */
  if (nrrdBasicInfoCopy(nout, nin,
                        NRRD_BASIC_INFO_DATA_BIT
                        | NRRD_BASIC_INFO_TYPE_BIT
                        | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                        | NRRD_BASIC_INFO_DIMENSION_BIT
                        | NRRD_BASIC_INFO_CONTENT_BIT
                        | NRRD_BASIC_INFO_COMMENTS_BIT
                        | (nrrdStateKeyValuePairsPropagate
                           ? 0
                           : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT))) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  return 0;
}

/*
******** nrrdUntile2D()
**
** This will split ax0 into nin->axis[ax0].size/sizeFast and sizeFast
** sizes.  ax1 will then be split into nin->axis[ax1].size/sizeSlow
** and sizeSlow sizes.  The axes corresponding to sizeFast and
** sizeSlow will be permuted and merged such that
** nout->axis[axMerge].size == sizeFast*sizeSlow.
**
** The thing to be careful of is that axMerge identifies an axis
** in the array set *after* the two axis splits, not before.  This
** is in contrast to the axSplit (and ax0 and ax1) argument of nrrdTile2D
** which identifies axes in the original nrrd.
*/
int nrrdUntile2D(Nrrd *nout, const Nrrd *nin,
                 unsigned int ax0, unsigned int ax1,
                 unsigned int axMerge, size_t sizeFast, size_t sizeSlow) {
  static const char me[]="nrrdUntile2D";
  int E;
  unsigned int ii, mapIdx, map[NRRD_DIM_MAX];
  
  if (!(nout && nin)) {
    biffAddf(NRRD, "%s: got NULL pointer", me);
    return 1;
  }
  if (ax0 == ax1) {
    biffAddf(NRRD, "%s: ax0 (%d) and ax1 (%d) must be distinct",
             me, ax0, ax1);
    return 1;
  }
  if (!( ax0 < nin->dim && ax1 < nin->dim )) {
    biffAddf(NRRD, "%s: ax0, ax1 (%d,%d) must be in range [0,%d]",
             me, ax0, ax1, nin->dim-1);
    return 1;
  }
  if (!( axMerge <= nin->dim )) {
    biffAddf(NRRD, "%s: axMerge (%d) must be in range [0,%d]",
             me, axMerge, nin->dim);
    return 1;
  }
  if (nin->axis[ax0].size != sizeFast*(nin->axis[ax0].size/sizeFast)) {
    biffAddf(NRRD, "%s: sizeFast (" _AIR_SIZE_T_CNV ") doesn't divide into "
             "axis %d size (" _AIR_SIZE_T_CNV ")",
             me, sizeFast, ax0, nin->axis[ax0].size);
    return 1;
  }
  if (nin->axis[ax1].size != sizeSlow*(nin->axis[ax1].size/sizeSlow)) {
    biffAddf(NRRD, "%s: sizeSlow (" _AIR_SIZE_T_CNV ") doesn't divide into "
             "axis %d size (" _AIR_SIZE_T_CNV ")",
             me, sizeSlow, ax1, nin->axis[ax1].size);
    return 1;
  }

  if (nout != nin) {
    if (_nrrdCopy(nout, nin, (NRRD_BASIC_INFO_COMMENTS_BIT
                              | (nrrdStateKeyValuePairsPropagate
                                 ? 0
                                 : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT)))) {
      biffAddf(NRRD, "%s:", me);
      return 1;
    }
  }

  /* Split the larger (slower) axis first. */
  E = AIR_FALSE;
  if (ax0 < ax1) {
    if (!E) E |= nrrdAxesSplit(nout, nout, ax1,
                               nin->axis[ax1].size/sizeSlow, sizeSlow);
    if (!E) E |= nrrdAxesSplit(nout, nout, ax0, 
                               nin->axis[ax0].size/sizeFast, sizeFast);
    /* Increment the larger value as it will get shifted by the lower
       split. */
    ax1++;
  } else {
    if (!E) E |= nrrdAxesSplit(nout, nout, ax0,
                               nin->axis[ax0].size/sizeFast, sizeFast);
    if (!E) E |= nrrdAxesSplit(nout, nout, ax1, 
                               nin->axis[ax1].size/sizeSlow, sizeSlow);
    ax0++;
  }
  if (E) {
    biffAddf(NRRD, "%s: trouble with initial splitting", me);
    return 1;
  }
  
  /* Determine the axis permutation map */
  mapIdx = 0;
  for (ii=0; ii<nout->dim; ii++) {
    if (mapIdx == axMerge) {
      /* Insert the slow parts of the axes that have been split */
      map[mapIdx++] = ax0+1;
      map[mapIdx++] = ax1+1;
    }
    if (ii == ax0+1 || ii == ax1+1) {
      /* These are handled by the logic above */
    } else {
      /* Otherwise use the identity map */
      map[mapIdx++] = ii;
    }
  }

  /*
  fprintf(stderr, "%s: map =", me);
  for (ii=0; ii<nout->dim; ii++) {
    fprintf(stderr, " %d", map[ii]);
  }
  fprintf(stderr, "; axMerge = %d\n", axMerge);
  */

  E = AIR_FALSE;
  if (!E) E |= nrrdAxesPermute(nout, nout, map);
  if (!E) E |= nrrdAxesMerge(nout, nout, axMerge);    
  if (E) {
    biffAddf(NRRD, "%s: trouble", me);
    return 1;
  }
  
  if (nrrdBasicInfoCopy(nout, nin,
                        NRRD_BASIC_INFO_DATA_BIT
                        | NRRD_BASIC_INFO_TYPE_BIT
                        | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                        | NRRD_BASIC_INFO_DIMENSION_BIT
                        | NRRD_BASIC_INFO_CONTENT_BIT
                        | NRRD_BASIC_INFO_COMMENTS_BIT
                        | (nrrdStateKeyValuePairsPropagate
                           ? 0
                           : NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT))) {
    biffAddf(NRRD, "%s:", me);
    return 1;
  }
  return 0;
}


/* ---- END non-NrrdIO */

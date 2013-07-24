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
  memset(invp, 0, nn*sizeof(unsigned int));
  for (ii=0; ii<nn; ii++) {
    if (!( pp[ii] <= nn-1)) {
      biffAddf(NRRD,
               "%s: permutation element #%d == %d out of bounds [0,%d]",
               me, ii, pp[ii], nn-1);
      return 1;
    }
    invp[pp[ii]]++;
  }
  /* for some reason when this code was written (revision 2700 Sun Jul
     3 04:18:33 2005 UTC) it was decided that all problems with the
     permutation would be reported with a pile of error messages in
     biff; rather than bailing at the first problem.  Not clear if
     this is a good idea. */
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
  size_t idxOut, idxInA=0,   /* indices for input and output scanlines */
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
      axmap[ai] = AIR_INT(axes[ai]);
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
    memset(laxes, 0, sizeof(laxes));
    for (ai=0; ai<ldim; ai++) {
      laxes[ai] = axes[ai+lowPax]-lowPax;
    }
    dataOut = AIR_CAST(char *, nout->data);
    memset(cIn, 0, sizeof(cIn));
    memset(cOut, 0, sizeof(cOut));
    for (idxOut=0; idxOut<numLines; idxOut++) {
      /* in our representation of the coordinates of the start of the
         scanlines that we're copying, we are not even storing all the
         zeros in the coordinates prior to lowPax, and when we go to
         a linear index for the memcpy(), we multiply by lineSize */
      for (ai=0; ai<ldim; ai++) {
        cIn[laxes[ai]] = cOut[ai];
      }
      NRRD_INDEX_GEN(idxInA, cIn, lszIn, ldim);
      memcpy(dataOut + idxOut*lineSize, dataIn + idxInA*lineSize, lineSize);
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
  unsigned int ai, ldim, len;
  size_t idxInB=0, idxOut, lineSize, numLines, size[NRRD_DIM_MAX], *lsize,
    cIn[NRRD_DIM_MAX+1], cOut[NRRD_DIM_MAX+1];
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
      char stmp[AIR_STRLEN_SMALL];
      biffAddf(NRRD, "%s: perm[%d] (%s) outside valid range [0,%d]", me, ai,
               airSprintSize_t(stmp, perm[ai]), len-1);
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
  dataIn = AIR_CAST(char *, nin->data);
  dataOut = AIR_CAST(char *, nout->data);
  memset(cIn, 0, sizeof(cIn));
  memset(cOut, 0, sizeof(cOut));
  for (idxOut=0; idxOut<numLines; idxOut++) {
    memcpy(cIn, cOut, sizeof(cIn));
    cIn[0] = perm[cOut[0]];
    NRRD_INDEX_GEN(idxInB, cIn, lsize, ldim);
    NRRD_INDEX_GEN(idxOut, cOut, lsize, ldim);
    memcpy(dataOut + idxOut*lineSize, dataIn + idxInB*lineSize, lineSize);
    NRRD_COORD_INCR(cOut, lsize, ldim, 0);
  }
  /* Set content. The LONGEST_INTERESTING_AXIS hack avoids the
     previous array out-of-bounds bug */
  if (len <= LONGEST_INTERESTING_AXIS) {
    strcpy(buff1, "");
    for (ai=0; ai<len; ai++) {
      char stmp[AIR_STRLEN_SMALL];
      sprintf(buff2, "%s%s", (ai ? "," : ""), airSprintSize_t(stmp, perm[ai]));
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


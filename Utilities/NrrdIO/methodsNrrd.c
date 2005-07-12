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

void
nrrdPeripheralInit(Nrrd *nrrd) {

  nrrdBasicInfoInit(nrrd,
                    NRRD_BASIC_INFO_DATA_BIT
                    | NRRD_BASIC_INFO_TYPE_BIT
                    | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                    | NRRD_BASIC_INFO_DIMENSION_BIT
                    | NRRD_BASIC_INFO_CONTENT_BIT
                    | NRRD_BASIC_INFO_COMMENTS_BIT
                    | NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT);
  return;
}

int
nrrdPeripheralCopy(Nrrd *nout, const Nrrd *nin) {

  nrrdBasicInfoCopy(nout, nin,
                    NRRD_BASIC_INFO_DATA_BIT
                    | NRRD_BASIC_INFO_TYPE_BIT
                    | NRRD_BASIC_INFO_BLOCKSIZE_BIT
                    | NRRD_BASIC_INFO_DIMENSION_BIT
                    | NRRD_BASIC_INFO_CONTENT_BIT
                    | NRRD_BASIC_INFO_COMMENTS_BIT
                    | NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT);
  return 0;
}
  
/* ------------------------------------------------------------ */

void
nrrdIoStateInit (NrrdIoState *nio) {

  if (nio) {
    nio->path = (char *)airFree(nio->path);
    nio->base = (char *)airFree(nio->base);
    nio->line = (char *)airFree(nio->line);
    nio->dataFNFormat = (char *)airFree(nio->dataFNFormat);
    airArrayLenSet(nio->dataFNArr, 0);
    /* closing this is always someone else's responsibility */
    nio->headerFile = NULL;
    nio->dataFile = NULL;
    nio->dataFileDim = 0;
    nio->dataFNMin = 0;
    nio->dataFNMax = 0;
    nio->dataFNStep = 0;
    nio->dataFNIndex = -1;
    nio->lineLen = 0;
    nio->pos = 0;
    nio->endian = airEndianUnknown;
    nio->lineSkip = 0;
    nio->byteSkip = 0;
    memset(nio->seen, 0, (NRRD_FIELD_MAX+1)*sizeof(int));
    nio->detachedHeader = AIR_FALSE;
    nio->bareText = nrrdDefWriteBareText;
    nio->charsPerLine = nrrdDefWriteCharsPerLine;
    nio->valsPerLine = nrrdDefWriteValsPerLine;
    nio->skipData = AIR_FALSE;
    nio->keepNrrdDataFileOpen = AIR_FALSE;
    nio->zlibLevel = -1;
    nio->zlibStrategy = nrrdZlibStrategyDefault;
    nio->bzip2BlockSize = -1;
    nio->oldData = NULL;
    nio->oldDataSize = 0;
    nio->format = nrrdFormatUnknown;
    nio->encoding = nrrdEncodingUnknown;
  }
  return;
}

NrrdIoState *
nrrdIoStateNew (void) {
  NrrdIoState *nio;
  
  nio = (NrrdIoState *)calloc(1, sizeof(NrrdIoState));
  if (nio) {
    nio->path = NULL;
    nio->base = NULL;
    nio->line = NULL;
    nio->dataFNFormat = NULL;
    nio->dataFN = NULL;
    nio->dataFNArr = airArrayNew((void**)(&(nio->dataFN)), NULL, 
                                 sizeof(char *), NRRD_FILENAME_INCR);
    airArrayPointerCB(nio->dataFNArr, airNull, airFree);
    nio->format = nrrdFormatUnknown;
    nio->encoding = nrrdEncodingUnknown;
    nrrdIoStateInit(nio);
  }
  return nio;
}

NrrdIoState *
nrrdIoStateNix (NrrdIoState *nio) {
  
  nio->path = (char *)airFree(nio->path);
  nio->base = (char *)airFree(nio->base);
  nio->line = (char *)airFree(nio->line);
  nio->dataFNFormat = (char *)airFree(nio->dataFNFormat);
  nio->dataFNArr = airArrayNuke(nio->dataFNArr);
  /* the NrrdIoState never owned nio->oldData; we don't free it */
  airFree(nio);  /* no NULL assignment, else compile warnings */
  return NULL;
}


/* ------------------------------------------------------------ */

/* see axis.c for axis-specific "methods" */

/* ------------------------------------------------------------ */

/*
******** nrrdBasicInfoInit
**
** resets "basic" (per-array) information
** formerly nrrdPeripheralInit
**
** the bitflag communicates which fields should *not* be initialized
*/
void
nrrdBasicInfoInit (Nrrd *nrrd, int bitflag) {
  int dd, ee;

  if (!nrrd) {
    return;
  }

  if (!(NRRD_BASIC_INFO_DATA_BIT & bitflag)) {
    nrrd->data = airFree(nrrd->data);
  }
  if (!(NRRD_BASIC_INFO_TYPE_BIT & bitflag)) {
    nrrd->type = nrrdTypeUnknown;
  }
  if (!(NRRD_BASIC_INFO_BLOCKSIZE_BIT & bitflag)) {
    nrrd->blockSize = 0;
  }
  if (!(NRRD_BASIC_INFO_DIMENSION_BIT & bitflag)) {
    nrrd->dim = 0;
  }
  if (!(NRRD_BASIC_INFO_CONTENT_BIT & bitflag)) {
    nrrd->content = (char *)airFree(nrrd->content); 
  }
  if (!(NRRD_BASIC_INFO_SAMPLEUNITS_BIT & bitflag)) {
    nrrd->sampleUnits = (char *)airFree(nrrd->sampleUnits); 
  }
  if (!(NRRD_BASIC_INFO_SPACE_BIT & bitflag)) {
    nrrd->space = nrrdSpaceUnknown;
    nrrd->spaceDim = 0;
  }
  if (!(NRRD_BASIC_INFO_SPACEDIMENSION_BIT & bitflag)) {
    nrrd->space = nrrdSpaceUnknown;
    nrrd->spaceDim = 0;
  }
  if (!(NRRD_BASIC_INFO_SPACEUNITS_BIT & bitflag)) {
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      nrrd->spaceUnits[dd] = (char *)airFree(nrrd->spaceUnits[dd]);
    }
  }
  if (!(NRRD_BASIC_INFO_SPACEORIGIN_BIT & bitflag)) {
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      nrrd->spaceOrigin[dd] = AIR_NAN;
    }
  }
  if (!(NRRD_BASIC_INFO_MEASUREMENTFRAME_BIT & bitflag)) {
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      for (ee=0; ee<NRRD_SPACE_DIM_MAX; ee++) {
        nrrd->measurementFrame[dd][ee] = AIR_NAN;
      }
    }
  }
  if (!(NRRD_BASIC_INFO_OLDMIN_BIT & bitflag)) {
    nrrd->oldMin = AIR_NAN;
  }
  if (!(NRRD_BASIC_INFO_OLDMAX_BIT & bitflag)) {
    nrrd->oldMax = AIR_NAN;
  }
  if (!(NRRD_BASIC_INFO_COMMENTS_BIT & bitflag)) {
    nrrdCommentClear(nrrd);
  }
  if (!(NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT & bitflag)) {
    nrrdKeyValueClear(nrrd);
  }
  return;
}

/*
******** nrrdBasicInfoCopy
**
** copies "basic" (per-array) information
** formerly known as nrrdPeripheralCopy, which was not used consistently
**
** the bitflag communicates which fields should *not* be copied
*/
int
nrrdBasicInfoCopy (Nrrd *dest, const Nrrd *src, int bitflag) {
  char me[]="nrrdBasicInfoCopy", err[AIR_STRLEN_MED];
  unsigned int dd, ee;

  if (!( dest && src ))
    return 0;
  if (dest == src) {
    /* nothing to do */
    return 0;
  }

  if (!(NRRD_BASIC_INFO_DATA_BIT & bitflag)) {
    dest->data = src->data;
  }
  if (!(NRRD_BASIC_INFO_TYPE_BIT & bitflag)) {
    dest->type = src->type;
  }
  if (!(NRRD_BASIC_INFO_BLOCKSIZE_BIT & bitflag)) {
    dest->blockSize = src->blockSize;
  }
  if (!(NRRD_BASIC_INFO_DIMENSION_BIT & bitflag)) {
    dest->dim = src->dim;
  }
  if (!(NRRD_BASIC_INFO_CONTENT_BIT & bitflag)) {
    dest->content = (char *)airFree(dest->content);
    dest->content = airStrdup(src->content);
    if (src->content && !dest->content) {
      sprintf(err, "%s: couldn't copy content", me);
      biffAdd(NRRD, err); return 1;
    }
  }
  if (!(NRRD_BASIC_INFO_SAMPLEUNITS_BIT & bitflag)) {
    dest->sampleUnits = (char *)airFree(dest->sampleUnits);
    dest->sampleUnits = airStrdup(src->sampleUnits);
    if (src->sampleUnits && !dest->sampleUnits) {
      sprintf(err, "%s: couldn't copy sampleUnits", me);
      biffAdd(NRRD, err); return 1;
    }
  }
  if (!(NRRD_BASIC_INFO_SPACE_BIT & bitflag)) {
    dest->space = src->space;
  }
  if (!(NRRD_BASIC_INFO_SPACEDIMENSION_BIT & bitflag)) {
    dest->spaceDim = src->spaceDim;
  }
  if (!(NRRD_BASIC_INFO_SPACEUNITS_BIT & bitflag)) {
    for (dd=0; dd<src->spaceDim; dd++) {
      dest->spaceUnits[dd] = (char *)airFree(dest->spaceUnits[dd]);
      dest->spaceUnits[dd] = airStrdup(src->spaceUnits[dd]);
      if (src->spaceUnits[dd] && !dest->spaceUnits[dd]) {
        sprintf(err, "%s: couldn't copy spaceUnits[%d]", me, dd);
        biffAdd(NRRD, err); return 1;
      }
    }
    for (dd=src->spaceDim; dd<NRRD_SPACE_DIM_MAX; dd++) {
      dest->spaceUnits[dd] = (char *)airFree(dest->spaceUnits[dd]);
    }
  }
  if (!(NRRD_BASIC_INFO_SPACEORIGIN_BIT & bitflag)) {
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      if (dd <= src->spaceDim-1) {
        dest->spaceOrigin[dd] = src->spaceOrigin[dd];
      } else {
        dest->spaceOrigin[dd] = AIR_NAN;
      }
    }
  }
  if (!(NRRD_BASIC_INFO_MEASUREMENTFRAME_BIT & bitflag)) {
    for (dd=0; dd<NRRD_SPACE_DIM_MAX; dd++) {
      for (ee=0; ee<NRRD_SPACE_DIM_MAX; ee++) {
        if (dd <= src->spaceDim-1 && ee <= src->spaceDim-1) {
          dest->measurementFrame[dd][ee] = src->measurementFrame[dd][ee];
        } else {
          dest->measurementFrame[dd][ee] = AIR_NAN;
        }
      }
    }
    for (dd=src->spaceDim; dd<NRRD_SPACE_DIM_MAX; dd++) {
      dest->spaceOrigin[dd] = AIR_NAN;
    }
  }
  if (!(NRRD_BASIC_INFO_OLDMIN_BIT & bitflag)) {
    dest->oldMin = src->oldMin;
  }
  if (!(NRRD_BASIC_INFO_OLDMAX_BIT & bitflag)) {
    dest->oldMax = src->oldMax;
  }
  if (!(NRRD_BASIC_INFO_COMMENTS_BIT & bitflag)) {
    if (nrrdCommentCopy(dest, src)) {
      sprintf(err, "%s: trouble copying comments", me);
      biffAdd(NRRD, err); return 1;
    }
  }
  if (!(NRRD_BASIC_INFO_KEYVALUEPAIRS_BIT & bitflag)) {
    if (nrrdKeyValueCopy(dest, src)) {
      sprintf(err, "%s: trouble copying key/value pairs", me);
      biffAdd(NRRD, err); return 1;
    }
  }
  return 0;
}

/*
******* nrrdInit
**
** initializes a nrrd to default state.  All nrrd functions in the
** business of initializing a nrrd struct use this function.  Mostly
** just sets values to 0, NaN, "", NULL, or Unknown
*/
void
nrrdInit (Nrrd *nrrd) {
  int ii;

  if (nrrd) {
    nrrdBasicInfoInit(nrrd, NRRD_BASIC_INFO_NONE);
    for (ii=0; ii<NRRD_DIM_MAX; ii++) {
      _nrrdAxisInfoInit(nrrd->axis + ii);
    }
  }
  return;
}

/*
******** nrrdNew()
**
** creates and initializes a Nrrd
**
** this does NOT use biff
*/
Nrrd *
nrrdNew (void) {
  int ii;
  Nrrd *nrrd;
  
  nrrd = (Nrrd*)(calloc(1, sizeof(Nrrd)));
  if (!nrrd)
    return NULL;

  /* explicitly set pointers to NULL, since calloc isn't officially
     guaranteed to do that.  */
  nrrd->data = NULL;
  for (ii=0; ii<NRRD_DIM_MAX; ii++) {
    _nrrdAxisInfoNewInit(nrrd->axis + ii);
  }
  for (ii=0; ii<NRRD_SPACE_DIM_MAX; ii++) {
    nrrd->spaceUnits[ii] = NULL;
  }
  nrrd->content = NULL;
  nrrd->sampleUnits = NULL;

  /* create comment airArray (even though it starts empty) */
  nrrd->cmt = NULL;
  nrrd->cmtArr = airArrayNew((void**)(&(nrrd->cmt)), NULL, 
                             sizeof(char *), NRRD_COMMENT_INCR);
  if (!nrrd->cmtArr) {
    return NULL;
  }
  airArrayPointerCB(nrrd->cmtArr, airNull, airFree);

  /* create key/value airArray (even thought it starts empty) */
  nrrd->kvp = NULL;
  nrrd->kvpArr = airArrayNew((void**)(&(nrrd->kvp)), NULL, 
                             2*sizeof(char *), NRRD_KEYVALUE_INCR);
  if (!nrrd->kvpArr) {
    return NULL;
  }
  /* key/value airArray uses no callbacks for now */
  
  /* finish initializations */
  nrrdInit(nrrd);

  return nrrd;
}

/*
******** nrrdNix()
**
** does nothing with the array data inside, just does whatever is needed
** to free the nrrd itself
**
** returns NULL
**
** this does NOT use biff
*/
Nrrd *
nrrdNix (Nrrd *nrrd) {
  int ii;
  
  if (nrrd) {
    for (ii=0; ii<NRRD_DIM_MAX; ii++) {
      _nrrdAxisInfoInit(&(nrrd->axis[ii]));
    }
    for (ii=0; ii<NRRD_SPACE_DIM_MAX; ii++) {
      nrrd->spaceUnits[ii] = (char *)airFree(nrrd->spaceUnits[ii]);
    }
    nrrd->content = (char *)airFree(nrrd->content);
    nrrd->sampleUnits = (char *)airFree(nrrd->sampleUnits);
    nrrdCommentClear(nrrd);
    nrrd->cmtArr = airArrayNix(nrrd->cmtArr);
    nrrdKeyValueClear(nrrd);
    nrrd->kvpArr = airArrayNix(nrrd->kvpArr);
    airFree(nrrd);
  }
  return NULL;
}

/*
******** nrrdEmpty()
**
** frees data inside nrrd AND resets all its state, so its the
** same as what comes from nrrdNew().  This includes free()ing
** any comments.
*/
Nrrd *
nrrdEmpty (Nrrd *nrrd) {
  
  if (nrrd) {
    nrrd->data = airFree(nrrd->data);
    nrrdInit(nrrd);
  }
  return nrrd;
}

/*
******** nrrdNuke()
**
** blows away the nrrd and everything inside
**
** always returns NULL
*/
Nrrd *
nrrdNuke (Nrrd *nrrd) {
  
  if (nrrd) {
    nrrdEmpty(nrrd);
    nrrdNix(nrrd);
  }
  return NULL;
}

/* ------------------------------------------------------------ */

int
_nrrdSizeCheck (const size_t *size, unsigned int dim, int useBiff) {
  char me[]="_nrrdSizeCheck", err[AIR_STRLEN_MED];
  size_t num, pre;
  unsigned int ai;
  
  pre = num = 1;
  for (ai=0; ai<dim; ai++) {
    num *= size[ai];
    if (num/size[ai] != pre) {
      sprintf(err, "%s: total # of elements too large to be represented in "
              "type size_t, so too large for current architecture", me);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
    pre *= size[ai];
  }
  return 0;
}

/*
******** nrrdWrap_nva()
**
** wraps a given Nrrd around a given array
**
** we don't touch any of the peripheral information (content, comments,
** blocksize, min/max) because it is entirely reasonable to be setting
** this before or after this call.  "type" could be passed as 
** nrrdTypeBlock, in which case it is the user's responsibility to
** set nrrd->blockSize at some other time.
*/
int
nrrdWrap_nva (Nrrd *nrrd, void *data, int type,
              unsigned int dim, const size_t *size) {
  char me[]="nrrdWrap_nva", err[AIR_STRLEN_MED];
  
  if (!(nrrd && size)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  nrrd->data = data;
  nrrd->type = type;
  nrrd->dim = dim;
  if (_nrrdSizeCheck(size, dim, AIR_TRUE)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoSize, size);
  return 0;
}

/*
******** nrrdWrap()
**
** Minimal var args wrapper around nrrdWrap_nva, with the advantage of 
** taking all the axes sizes as the var args.
**
** This is THE BEST WAY to wrap a nrrd around existing raster data,
** assuming that the dimension is known at compile time.
**
** If successful, returns 0, otherwise, 1.
** This does use biff.
*/
int
nrrdWrap (Nrrd *nrrd, void *data, int type, unsigned int dim, ...) {
  char me[] = "nrrdWrap", err[AIR_STRLEN_MED];
  va_list ap;
  size_t size[NRRD_DIM_MAX];
  unsigned int ai;
  
  if (!(nrrd && data)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  va_start(ap, dim);
  for (ai=0; ai<dim; ai++) {
    size[ai] = va_arg(ap, size_t);
  }
  va_end(ap);
  
  return nrrdWrap_nva(nrrd, data, type, dim, size);
}

/*
void
_nrrdTraverse (Nrrd *nrrd) {
  char *test, tval;
  size_t I, N;
  int S;
  
  N = nrrdElementNumber(nrrd);
  S = nrrdElementSize(nrrd);
  tval = 0;
  test = nrrd->data;
  for (I=0; I<N*S; I++) {
    tval += test[I];
  }
}
*/

int
_nrrdCopy (Nrrd *nout, const Nrrd *nin, int bitflag) {
  char me[]="_nrrdCopy", err[AIR_STRLEN_MED];
  size_t size[NRRD_DIM_MAX];

  if (!(nin && nout)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nout == nin) {
    /* its not the case that we have nothing to do- the semantics of
       copying cannot be achieved if the input and output nrrd are
       the same; this is an error */
    sprintf(err, "%s: nout==nin disallowed", me);
    biffAdd(NRRD, err); return 1;
  }
  if (!nrrdElementSize(nin)) {
    sprintf(err, "%s: input nrrd reports zero element size!", me);
    biffAdd(NRRD, err); return 1;
  }
  nrrdAxisInfoGet_nva(nin, nrrdAxisInfoSize, size);
  if (nin->data) {
    if (nrrdMaybeAlloc_nva(nout, nin->type, nin->dim, size)) {
      sprintf(err, "%s: couldn't allocate data", me);
      biffAdd(NRRD, err); return 1;
    }
    memcpy(nout->data, nin->data,
           nrrdElementNumber(nin)*nrrdElementSize(nin));
  } else {
    /* someone is trying to copy structs without data, fine fine fine */
    if (nrrdWrap_nva(nout, NULL, nin->type, nin->dim, size)) {
      sprintf(err, "%s: couldn't allocate data", me);
      biffAdd(NRRD, err); return 1;
    }
  }
  nrrdAxisInfoCopy(nout, nin, NULL, NRRD_AXIS_INFO_SIZE_BIT);
  /* if nin->data non-NULL (second branch above), this will 
     harmlessly unset and set type and dim */
  nrrdBasicInfoInit(nout, NRRD_BASIC_INFO_DATA_BIT | bitflag);
  if (nrrdBasicInfoCopy(nout, nin, NRRD_BASIC_INFO_DATA_BIT | bitflag)) {
    sprintf(err, "%s: trouble copying basic info", me);
    biffAdd(NRRD, err); return 1;
  }

  return 0;
}

/*
******** nrrdCopy
**
** copy method for nrrds.  nout will end up as an "exact" copy of nin.
** New space for data is allocated here, and output nrrd points to it.
** Comments from old are added to comments for new, so these are also
** newly allocated.  nout->ptr is not set, nin->ptr is not read.
*/
int
nrrdCopy(Nrrd *nout, const Nrrd *nin) {
  char me[]="nrrdCopy", err[AIR_STRLEN_MED];

  if (_nrrdCopy(nout, nin, NRRD_BASIC_INFO_NONE)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  return 0;
}


/*
******** nrrdAlloc_nva()
**
** allocates data array and sets information.  If this is a block type
** nrrd, it is necessary to set nrrd->blockSize PRIOR to calling
** this function.
**
** This function will always allocate more memory (via calloc), but
** it will free() nrrd->data if it is non-NULL when passed in.
**
** This function takes the same "don't mess with peripheral information"
** attitude as nrrdWrap().
**
** Note to Gordon: don't get clever and change ANY axis-specific
** information here.  It may be very convenient to set that before
** nrrdAlloc() or nrrdMaybeAlloc()
**
** Note: This function DOES use biff
*/
int 
nrrdAlloc_nva (Nrrd *nrrd, int type, unsigned int dim, const size_t *size) {
  char me[] = "nrrdAlloc_nva", err[AIR_STRLEN_MED];
  size_t num;
  int esize;

  if (!(nrrd && size)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (airEnumValCheck(nrrdType, type)) {
    sprintf(err, "%s: type (%d) is invalid", me, type);
    biffAdd(NRRD, err); return 1;
  }
  if (nrrdTypeBlock == type) {
    if (!(0 < nrrd->blockSize)) {
      sprintf(err, "%s: given nrrd->blockSize " _AIR_SIZE_T_CNV " invalid", 
              me, nrrd->blockSize);
      biffAdd(NRRD, err); return 1;
    }
  }
  if (!AIR_IN_CL(1, dim, NRRD_DIM_MAX)) {
    sprintf(err, "%s: dim (%d) not in valid range [1,%d]",
            me, dim, NRRD_DIM_MAX);
    biffAdd(NRRD, err); return 1;
  }

  nrrd->data = airFree(nrrd->data);
  if (nrrdWrap_nva(nrrd, NULL, type, dim, size)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1 ;
  }
  num = nrrdElementNumber(nrrd);
  esize = nrrdElementSize(nrrd);
  nrrd->data = calloc(num, esize);
  if (!(nrrd->data)) {
    sprintf(err, "%s: calloc(" _AIR_SIZE_T_CNV ",%d) failed",
            me, num, esize);
    biffAdd(NRRD, err); return 1 ;
  }

  return 0;
}

/*
******** nrrdAlloc()
**
** Handy wrapper around nrrdAlloc_nva, which takes, as its vararg list,
** all the axes sizes.
*/
int 
nrrdAlloc (Nrrd *nrrd, int type, unsigned int dim, ...) {
  char me[]="nrrdAlloc", err[AIR_STRLEN_MED];
  size_t size[NRRD_DIM_MAX];
  unsigned int ai;
  va_list ap;
  
  if (!nrrd) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  va_start(ap, dim);
  for (ai=0; ai<dim; ai++) {
    size[ai] = va_arg(ap, size_t);
  }
  va_end(ap);
  if (nrrdAlloc_nva(nrrd, type, dim, size)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  return 0;
}


/*
******** nrrdMaybeAlloc_nva
**
** calls nrrdAlloc_nva if the requested space is different than
** what is currently held
**
** also subscribes to the "don't mess with peripheral information" philosophy
*/
int
nrrdMaybeAlloc_nva (Nrrd *nrrd, int type,
                    unsigned int dim, const size_t *size) {
  char me[]="nrrdMaybeAlloc_nva", err[AIR_STRLEN_MED];
  size_t sizeWant, sizeHave, numWant, elementSizeWant;
  int need;
  unsigned int ai;

  if (!nrrd) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (airEnumValCheck(nrrdType, type)) {
    sprintf(err, "%s: type (%d) is invalid", me, type);
    biffAdd(NRRD, err); return 1;
  }
  if (nrrdTypeBlock == type) {
    if (nrrdTypeBlock == nrrd->type) {
      sprintf(err, "%s: can't change from one block nrrd to another", me);
      biffAdd(NRRD, err); return 1;
    }
    if (!(0 < nrrd->blockSize)) {
      sprintf(err, "%s: given nrrd->blockSize " _AIR_SIZE_T_CNV " invalid", 
              me, nrrd->blockSize);
      biffAdd(NRRD, err); return 1;
    }
    elementSizeWant = nrrd->blockSize;
  } else {
    elementSizeWant = nrrdTypeSize[type];
  }
  if (_nrrdSizeCheck(size, dim, AIR_TRUE)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }

  if (!(nrrd->data)) {
    need = 1;
  } else {
    numWant = 1;
    for (ai=0; ai<dim; ai++) {
      numWant *= size[ai];
    }
    if (!nrrdElementSize(nrrd)) {
      sprintf(err, "%s: nrrd reports zero element size!", me);
      biffAdd(NRRD, err); return 1;
    }
    sizeHave = nrrdElementNumber(nrrd) * nrrdElementSize(nrrd);
    /* fprintf(stderr, "##%s: sizeHave = %d * %d = %d\n", me,
            (int)(nrrdElementNumber(nrrd)),
            (int)(nrrdElementSize(nrrd)), (int)sizeHave); */
    sizeWant = numWant * elementSizeWant;
    /* fprintf(stderr, "##%s: sizeWant = %d * %d = %d\n", me,
            (int)(numWant),
            (int)(elementSizeWant), (int)sizeWant); */
    need = sizeHave != sizeWant;
    /* fprintf(stderr, "##%s: need = %d\n", me, need); */
  }
  if (need) {
    if (nrrdAlloc_nva(nrrd, type, dim, size)) {
      sprintf(err, "%s:", me);
      biffAdd(NRRD, err); return 1;
    }
  } else {
    if (nrrdWrap_nva(nrrd, nrrd->data, type, dim, size)) {
      sprintf(err, "%s:", me);
      biffAdd(NRRD, err); return 1;
    }
    /* but we do have to initialize memory! */
    memset(nrrd->data, 0, nrrdElementNumber(nrrd)*nrrdElementSize(nrrd));
  }

  return 0;
}

/*
******** nrrdMaybeAlloc()
**
** Handy wrapper around nrrdAlloc, which takes, as its vararg list
** all the axes sizes, thereby calculating the total number.
*/
int 
nrrdMaybeAlloc (Nrrd *nrrd, int type, unsigned int dim, ...) {
  char me[]="nrrdMaybeAlloc", err[AIR_STRLEN_MED];
  size_t size[NRRD_DIM_MAX];
  unsigned int ai;
  va_list ap;
  
  if (!nrrd) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  va_start(ap, dim);
  for (ai=0; ai<dim; ai++) {
    size[ai] = va_arg(ap, size_t);
  }
  va_end(ap);
  if (nrrdMaybeAlloc_nva(nrrd, type, dim, size)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  return 0;
}

/*
******** nrrdPPM()
**
** for making a nrrd suitable for holding PPM data
**
** "don't mess with peripheral information"
*/
int
nrrdPPM (Nrrd *ppm, size_t sx, size_t sy) {
  char me[]="nrrdPPM", err[AIR_STRLEN_MED];

  if (nrrdMaybeAlloc(ppm, nrrdTypeUChar, 3, 3, sx, sy)) {
    sprintf(err, "%s: couldn't allocate " _AIR_SIZE_T_CNV
            " x " _AIR_SIZE_T_CNV " 24-bit image", me, sx, sy);
    biffAdd(NRRD, err); return 1;
  }
  return 0;
}

/*
******** nrrdPGM()
**
** for making a nrrd suitable for holding PGM data
**
** "don't mess with peripheral information"
*/
int
nrrdPGM (Nrrd *pgm, size_t sx, size_t sy) {
  char me[]="nrrdNewPGM", err[AIR_STRLEN_MED];

  if (nrrdMaybeAlloc(pgm, nrrdTypeUChar, 2, sx, sy)) {
    sprintf(err, "%s: couldn't allocate " _AIR_SIZE_T_CNV
            " x " _AIR_SIZE_T_CNV " 8-bit image", me, sx, sy);
    biffAdd(NRRD, err);
    return 1;
  }
  return 0;
}


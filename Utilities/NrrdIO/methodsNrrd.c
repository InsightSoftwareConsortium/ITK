/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998 University of Utah
 
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

/* ------------------------------------------------------------ */

void
nrrdIoStateInit (NrrdIoState *nio) {

  if (nio) {
    nio->path = airFree(nio->path);
    nio->base = airFree(nio->base);
    nio->dataFN = airFree(nio->dataFN);
    nio->line = airFree(nio->line);
    nio->lineLen = 0;
    nio->pos = 0;
    /* closing this is always someone else's responsibility */
    nio->dataFile = NULL;
    nio->format = nrrdFormatUnknown;
    nio->encoding = nrrdEncodingUnknown;
    nio->endian = airEndianUnknown;
    nio->lineSkip = 0;
    nio->byteSkip = 0;
    nio->detachedHeader = AIR_FALSE;
    nio->bareText = nrrdDefWriteBareText;
    nio->charsPerLine = nrrdDefWriteCharsPerLine;
    nio->valsPerLine = nrrdDefWriteValsPerLine;
    nio->zlibLevel = -1;
    nio->zlibStrategy = nrrdZlibStrategyDefault;
    nio->bzip2BlockSize = -1;
    nio->skipData = AIR_FALSE;
    nio->keepNrrdDataFileOpen = AIR_FALSE;
    nio->oldData = NULL;
    nio->oldDataSize = 0;
    memset(nio->seen, 0, (NRRD_FIELD_MAX+1)*sizeof(int));
  }
  return;
}

NrrdIoState *
nrrdIoStateNew (void) {
  NrrdIoState *nio;
  
  nio = calloc(1, sizeof(NrrdIoState));
  if (nio) {
    nio->path = NULL;
    nio->base = NULL;
    nio->dataFN = NULL;
    nio->line = NULL;
    nio->dataFile = NULL;
    nio->format = nrrdFormatUnknown;
    nio->encoding = nrrdEncodingUnknown;
    nrrdIoStateInit(nio);
  }
  return nio;
}

NrrdIoState *
nrrdIoStateNix (NrrdIoState *nio) {
  
  nio->path = airFree(nio->path);
  nio->base = airFree(nio->base);
  nio->dataFN = airFree(nio->dataFN);
  nio->line = airFree(nio->line);
  airFree(nio);  /* no NULL assignment, else compile warnings */
  /* the NrrdIoState never owned nio->oldData; we don't free it */
  return NULL;
}


/* ------------------------------------------------------------ */

/* see axis.c for axis-specific "methods" */

/* ------------------------------------------------------------ */

/*
******* nrrdInit
**
** initializes a nrrd to default state.  All nrrd functions in the
** business of initializing a nrrd struct use this function.  Mostly
** just sets values to 0, NaN, "", NULL, or Unknown
*/
void
nrrdInit (Nrrd *nrrd) {
  int i;

  if (nrrd) {
    nrrd->data = airFree(nrrd->data);
    nrrd->type = nrrdTypeUnknown;
    nrrd->dim = 0;
    
    for (i=0; i<NRRD_DIM_MAX; i++) {
      _nrrdAxisInfoInit(&(nrrd->axis[i]));
    }
    
    nrrd->content = airFree(nrrd->content);
    nrrd->blockSize = 0;
    nrrd->oldMin = nrrd->oldMax = AIR_NAN;
    /* nrrd->ptr = NULL; */
    
    /* the comment airArray should be already been allocated, 
       though perhaps empty */
    nrrdCommentClear(nrrd);

    /* likewise for key/value pairs */
    nrrdKeyValueClear(nrrd);
  }
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
  int i;
  Nrrd *nrrd;
  
  nrrd = (Nrrd*)(calloc(1, sizeof(Nrrd)));
  if (!nrrd)
    return NULL;

  /* explicitly set pointers to NULL */
  nrrd->data = NULL;
  nrrd->content = NULL;
  /* HEY: this is a symptom of some stupidity, no? */
  for (i=0; i<NRRD_DIM_MAX; i++) {
    nrrd->axis[i].label = NULL;
  }

  /* create comment airArray (even though it starts empty) */
  nrrd->cmt = NULL;
  nrrd->cmtArr = airArrayNew((void**)(&(nrrd->cmt)), NULL, 
                             sizeof(char *), NRRD_COMMENT_INCR);
  if (!nrrd->cmtArr)
    return NULL;
  airArrayPointerCB(nrrd->cmtArr, airNull, airFree);

  /* create key/value airArray (even thought it starts empty) */
  nrrd->kvp = NULL;
  nrrd->kvpArr = airArrayNew((void**)(&(nrrd->kvp)), NULL, 
                             2*sizeof(char *), NRRD_KEYVALUE_INCR);
  if (!nrrd->kvpArr)
    return NULL;
  /* no airArray callbacks for now */
  

  /* finish initializations */
  nrrdInit(nrrd);

  return nrrd;
}

/*
******** nrrdNix()
**
** does nothing with the array, just does whatever is needed
** to free the nrrd itself
**
** returns NULL
**
** this does NOT use biff
*/
Nrrd *
nrrdNix (Nrrd *nrrd) {
  int i;
  
  if (nrrd) {
    nrrd->content = airFree(nrrd->content);
    /* HEY: this is a symptom of some stupidity, no? */
    for (i=0; i<NRRD_DIM_MAX; i++) {
      nrrd->axis[i].label = airFree(nrrd->axis[i].label);
    }
    nrrdCommentClear(nrrd);
    nrrd->cmtArr = airArrayNix(nrrd->cmtArr);
    nrrdKeyValueClear(nrrd);
    nrrd->kvpArr = airArrayNix(nrrd->kvpArr);
    airFree(nrrd);  /* no NULL assignment, else compile warnings */
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
_nrrdSizeCheck (int dim, const int *size, int useBiff) {
  char me[]="_nrrdSizeCheck", err[AIR_STRLEN_MED];
  size_t num, pre;
  int d;
  
  pre = num = 1;
  for (d=0; d<dim; d++) {
    if (!(size[d] > 0)) {
      sprintf(err, "%s: invalid size (%d) for axis %d (dim = %d)",
              me, size[d], d, dim);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
    num *= size[d];
    if (num/size[d] != pre) {
      sprintf(err, "%s: total # of elements too large to be represented in "
              "type size_t, so too large for current architecture", me);
      biffMaybeAdd(NRRD, err, useBiff); return 1;
    }
    pre *= size[d];
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
nrrdWrap_nva (Nrrd *nrrd, void *data, int type, int dim, const int *size) {
  char me[] = "nrrdWrap_nva", err[AIR_STRLEN_MED];
  int d;
  
  if (!(nrrd && size)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  nrrd->data = data;
  nrrd->type = type;
  nrrd->dim = dim;
  if (_nrrdSizeCheck(dim, size, AIR_TRUE)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  for (d=0; d<dim; d++) {
    nrrd->axis[d].size = size[d];
  }
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
nrrdWrap (Nrrd *nrrd, void *data, int type, int dim, ...) {
  char me[] = "nrrdWrap", err[AIR_STRLEN_MED];
  va_list ap;
  int d, size[NRRD_DIM_MAX];
  
  if (!(nrrd && data)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  va_start(ap, dim);
  for (d=0; d<dim; d++) {
    size[d] = va_arg(ap, int);
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

/*
** _nrrdCopyShallow
**
** Similar to nrrdCopy, but the data itself is not copied.  nout->data
** and nout->data will share a pointer to the data.  This should be
** used with extreem caution, because there is no pointer magic to
** make sure the data is not freed twice.
*/
int
_nrrdCopyShallow (Nrrd *nout, const Nrrd *nin) {
  char me[]="_nrrdCopyShallow", err[AIR_STRLEN_MED];
  Nrrd *ntmp;
  airArray *mop;

  if (!(nin && nout)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }

  /* We're not using nrrdNew() because it allocates new airArray's for
     comments (cmtArr) and key/value pairs (kvpArr), which we do not
     need here, and which we don't want to have to explicitly delete */
  ntmp = (Nrrd*)(calloc(1, sizeof(Nrrd)));
  if (!ntmp) {
    sprintf(err, "%s: error allocating temporary nrrd.", me);
    biffAdd(NRRD, err); return 1;
  }
  /* Since we should never have copied the data, or allocated new meta-data,
     we want to make sure that we don't delete it here. */
  mop = airMopNew();
  airMopAdd(mop, ntmp, airFree, airMopAlways);
  
  /* Shallow copy the contents of the nrrd.  It's OK if this is not a
     deep copy (i.e. all the axis info), because nrrdCopy will do this
     for us.  This is only to facilitate setting the data pointer to
     NULL which will cause nrrdCopy to not copy the data. */
  memcpy(ntmp, nin, sizeof(Nrrd));

  /* Setting this to NULL will cause nrrdCopy to not copy the data */
  ntmp->data = NULL;
  
  if (nrrdCopy(nout, ntmp)) {
    sprintf(err, "%s: couldn't copy to output", me);
    biffAdd(NRRD, err);
    airMopError(mop);
    return 1;
  }

  /* Share the data pointer */
  nout->data = nin->data;

  airMopOkay(mop);
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
nrrdCopy (Nrrd *nout, const Nrrd *nin) {
  char me[]="nrrdCopy", err[AIR_STRLEN_MED];
  int size[NRRD_DIM_MAX];

  if (!(nin && nout)) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  if (nout == nin) {
    /* its not the case that we have nothing to do- the semantics of
       copying can not be achieved if the input and output nrrd are
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
    nout->data = NULL;
    /* We need to make sure to copy important stuff like type, dim,
       and sizes, as this information is not copied elsewhere.  If we
       did have non-NULL data, this information would be set in 
       nrrdAlloc_nva() called by nrrdMaybeAlloc_nva() above. */
    nout->type = nin->type;
    nout->dim = nin->dim;
    nrrdAxisInfoSet_nva(nout, nrrdAxisInfoSize, size);
  }
  nrrdAxisInfoCopy(nout, nin, NULL, NRRD_AXIS_INFO_NONE);

  /* HEY: shouldn't this be handled with nrrdPeripheralCopy() */
  nout->content = airFree(nout->content);
  nout->content = airStrdup(nin->content);
  if (nin->content && !nout->content) {
    sprintf(err, "%s: couldn't copy content", me);
    biffAdd(NRRD, err); return 1;
  }
  nout->blockSize = nin->blockSize;
  nout->oldMin = nin->oldMin;
  nout->oldMax = nin->oldMax;
  /* nout->ptr = nin->ptr; */
    
  if (nrrdCommentCopy(nout, nin)) {
    sprintf(err, "%s: trouble copying comments", me);
    biffAdd(NRRD, err); return 1;
  }

  if (nrrdKeyValueCopy(nout, nin)) {
    sprintf(err, "%s: trouble copying key/value pairs", me);
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
nrrdAlloc_nva (Nrrd *nrrd, int type, int dim, const int *size) {
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
      sprintf(err, "%s: given nrrd->blockSize %d invalid", 
              me, nrrd->blockSize);
      biffAdd(NRRD, err); return 1;
    }
  }
  if (!AIR_IN_CL(1, dim, NRRD_DIM_MAX)) {
    sprintf(err, "%s: dim (%d) not in valid range [1,%d]",
            me, dim, NRRD_DIM_MAX);
    biffAdd(NRRD, err); return 1;
  }

  nrrd->type = type;
  nrrd->data = airFree(nrrd->data);
  nrrd->dim = dim;
  if (_nrrdSizeCheck(dim, size, AIR_TRUE)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoSize, size);
  num = nrrdElementNumber(nrrd);
  esize = nrrdElementSize(nrrd);
  nrrd->data = calloc(num, esize);
  if (!(nrrd->data)) {
    sprintf(err, "%s: calloc(" _AIR_SIZE_T_FMT ",%d) failed", 
            me, num, nrrdElementSize(nrrd));
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
nrrdAlloc (Nrrd *nrrd, int type, int dim, ...) {
  char me[]="nrrdAlloc", err[AIR_STRLEN_MED];
  int size[NRRD_DIM_MAX], d;
  va_list ap;
  
  if (!nrrd) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  va_start(ap, dim);
  for (d=0; d<dim; d++) {
    size[d] = va_arg(ap, int);
  }
  va_end(ap);
  if (_nrrdSizeCheck(dim, size, AIR_TRUE)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
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
nrrdMaybeAlloc_nva (Nrrd *nrrd, int type, int dim, const int *size) {
  char me[]="nrrdMaybeAlloc_nva", err[AIR_STRLEN_MED];
  size_t sizeWant, sizeHave, numWant;
  int d, need, elementSizeWant;

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
      sprintf(err, "%s: given nrrd->blockSize %d invalid", 
              me, nrrd->blockSize);
      biffAdd(NRRD, err); return 1;
    }
    elementSizeWant = nrrd->blockSize;
  } else {
    elementSizeWant = nrrdTypeSize[type];
  }
  if (_nrrdSizeCheck(dim, size, AIR_TRUE)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }

  if (!(nrrd->data)) {
    need = 1;
  } else {
    numWant = 1;
    for (d=0; d<dim; d++) {
      numWant *= size[d];
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
    /* this is essentially a reshape, or maybe not even that */
    nrrd->type = type;
    nrrd->dim = dim;
    nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoSize, size);
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
nrrdMaybeAlloc (Nrrd *nrrd, int type, int dim, ...) {
  char me[]="nrrdMaybeAlloc", err[AIR_STRLEN_MED];
  int d, size[NRRD_DIM_MAX];
  va_list ap;
  
  if (!nrrd) {
    sprintf(err, "%s: got NULL pointer", me);
    biffAdd(NRRD, err); return 1;
  }
  va_start(ap, dim);
  for (d=0; d<dim; d++) {
    size[d] = va_arg(ap, int);
  }
  va_end(ap);
  if (_nrrdSizeCheck(dim, size, AIR_TRUE)) {
    sprintf(err, "%s:", me);
    biffAdd(NRRD, err); return 1;
  }
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
nrrdPPM (Nrrd *ppm, int sx, int sy) {
  char me[]="nrrdPPM", err[AIR_STRLEN_MED];

  if (!(sx > 0 && sy > 0)) {
    sprintf(err, "%s: got invalid sizes (%d,%d)", me, sx, sy);
    biffAdd(NRRD, err); return 1;
  }
  if (nrrdMaybeAlloc(ppm, nrrdTypeUChar, 3, 3, sx, sy)) {
    sprintf(err, "%s: couldn't allocate %d x %d 24-bit image", me, sx, sy);
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
nrrdPGM (Nrrd *pgm, int sx, int sy) {
  char me[]="nrrdNewPGM", err[AIR_STRLEN_MED];

  if (!(sx > 0 && sy > 0)) {
    sprintf(err, "%s: got invalid sizes (%d,%d)", me, sx, sy);
    biffAdd(NRRD, err);
    return 1;
  }
  if (nrrdMaybeAlloc(pgm, nrrdTypeUChar, 2, sx, sy)) {
    sprintf(err, "%s: couldn't allocate %d x %d 8-bit image", me, sx, sy);
    biffAdd(NRRD, err);
    return 1;
  }
  return 0;
}


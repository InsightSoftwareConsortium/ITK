/*
  NrrdIO: stand-alone code for basic nrrd functionality
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

typedef union {
  char **c;
  void **v;
} ptrHack;


int
_nrrdEncodingGzip_available(void) {

#if TEEM_ZLIB
  return AIR_TRUE;
#else
  return AIR_FALSE;
#endif
}

/*
** nio->byteSkip < 0 functionality contributed by Katharina Quintus
*/
int
_nrrdEncodingGzip_read(FILE *file, void *_data, size_t elNum,
                       Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdEncodingGzip_read";
#if TEEM_ZLIB
  size_t sizeData, sizeRed, sizeChunk;
  int error;
  long int bi;
  unsigned int read;
  char *data;
  gzFile gzfin;
  ptrHack hack;

  sizeData = nrrdElementSize(nrrd)*elNum;
  /* Create the gzFile for reading in the gzipped data. */
  if ((gzfin = _nrrdGzOpen(file, "rb")) == Z_NULL) {
    /* there was a problem */
    biffAddf(NRRD, "%s: error opening gzFile", me);
    return 1;
  }
  
  /* keeps track of how many bytes have been successfully read in */
  sizeRed = 0;
  
  /* zlib can only handle data sizes up to UINT_MAX ==> if there's more
     than UINT_MAX bytes to read in, we read in in chunks */
  sizeChunk = AIR_MIN(sizeData, UINT_MAX);
  
  if (nio->byteSkip < 0) { 
    /* We don't know the size of the size to skip before the data, so
       decompress the data first into a temporary memory buffer.  Then
       the byteskipping is then just memcpy-ing the appropriate region
       of memory from "buff" into the given "_data" pointer */
    char *buff;
    airArray *buffArr;
      
    /* setting the airArray increment to twice the chunk size means that for
       headers that are small compared to the data, the airArray never
       actually has to reallocate.  The unit is 1 because we are managing
       the reading in terms of bytes (sizeof(char)==1 by definition) */
    buff = NULL;
    hack.c = &buff;
    buffArr = airArrayNew(hack.v, NULL, 1, 2*sizeChunk);
    airArrayLenSet(buffArr, sizeChunk);
    if (!( buffArr && buffArr->data )) {
      biffAddf(NRRD, "%s: couldn't initialize airArray\n", me);
      return 1;
    }
    
    /* we keep reading in chunks as long as there hasn't been an error,
       and we haven't hit EOF (EOF signified by read == 0).  Unlike the
       code below (for positive byteskip), we are obligated to read until
       the bitter end, and can't update sizeChunk to encompass only the 
       required data.  Cast on third arg ok because of AIR_MIN use above */
    while (!(error = _nrrdGzRead(gzfin, buff + sizeRed,
                                 AIR_CAST(unsigned int, sizeChunk),
                                 &read))
           && read > 0) {
      sizeRed += read;
      if (read >= sizeChunk) {
        /* we were able to read as much data as we requested, maybe there is
           more, so we need to make our temp buffer bigger */
        airArrayLenIncr(buffArr, sizeChunk);
        if (!buffArr->data) {
          biffAddf(NRRD, "%s: couldn't re-allocate data buffer", me);
          return 1;
        }
      }
    }
    if (error) {
      biffAddf(NRRD, "%s: error reading from gzFile", me);
      return 1;
    }
    if (sizeRed < sizeData + (-nio->byteSkip - 1)) {
      biffAddf(NRRD, "%s: expected " _AIR_SIZE_T_CNV
               " bytes and received only "
               _AIR_SIZE_T_CNV " bytes", me,
               AIR_CAST(size_t, sizeData + (-nio->byteSkip - 1)), sizeRed);
      return 1;
    }
    /* also handles nio->byteSkip == -N-1 signifying extra N bytes at end */
    memcpy(_data, buff + sizeRed - sizeData - (-nio->byteSkip - 1), sizeData);
    airArrayNuke(buffArr);
  } else {
    /* no negative byteskip: after byteskipping, we can read directly
       into given data buffer */
    if (nio->byteSkip > 0) {
      for (bi=0; bi<nio->byteSkip; bi++) {
        unsigned char b;
        /* Check to see if a single byte was able to be read. */
        if (_nrrdGzRead(gzfin, &b, 1, &read) != 0 || read != 1) {
          biffAddf(NRRD, "%s: hit an error skipping byte %ld of %ld",
                   me, bi, nio->byteSkip);
          return 1;
        }
      }
    }
    /* Pointer to chunks as we read them. */
    data = AIR_CAST(char *, _data);
    while (!(error = _nrrdGzRead(gzfin, data, sizeChunk, &read))
           && read > 0) {
      /* Increment the data pointer to the next available chunk. */
      data += read; 
      sizeRed += read;
      /* We only want to read as much data as we need, so we need to check
         to make sure that we don't request data that might be there but that
         we don't want.  This will reduce sizeChunk when we get to the last
         block (which may be smaller than sizeChunk). */
      if (sizeData >= sizeRed 
          && sizeData - sizeRed < sizeChunk) {
        sizeChunk = sizeData - sizeRed;
      }
    }
    if (error) {
      biffAddf(NRRD, "%s: error reading from gzFile", me);
      return 1;
    }
    /* Check to see if we got out as much as we thought we should. */
    if (sizeRed != sizeData) {
      biffAddf(NRRD, "%s: expected " _AIR_SIZE_T_CNV " bytes and received "
               _AIR_SIZE_T_CNV " bytes",
               me, sizeData, sizeRed);
      return 1;
    }
  }

  /* Close the gzFile.  Since _nrrdGzClose does not close the FILE* we
     will not encounter problems when dataFile is closed later. */
  if (_nrrdGzClose(gzfin)) {
    biffAddf(NRRD, "%s: error closing gzFile", me);
    return 1;
  }
  
  return 0;
#else
  AIR_UNUSED(file);
  AIR_UNUSED(_data);
  AIR_UNUSED(elNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  biffAddf(NRRD, "%s: sorry, this nrrd not compiled with gzip enabled", me);
  return 1;
#endif
}

int
_nrrdEncodingGzip_write(FILE *file, const void *_data, size_t elNum,
                        const Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdEncodingGzip_write";
#if TEEM_ZLIB
  size_t sizeData, sizeWrit, sizeChunk;
  int fmt_i=0, error;
  char *data, fmt[4];
  gzFile gzfout;
  unsigned int wrote;
  
  sizeData = nrrdElementSize(nrrd)*elNum;

  /* Set format string based on the NrrdIoState parameters. */
  fmt[fmt_i++] = 'w';
  if (0 <= nio->zlibLevel && nio->zlibLevel <= 9)
    fmt[fmt_i++] = '0' + nio->zlibLevel;
  switch (nio->zlibStrategy) {
  case nrrdZlibStrategyHuffman:
    fmt[fmt_i++] = 'h';
    break;
  case nrrdZlibStrategyFiltered:
    fmt[fmt_i++] = 'f';
    break;
  case nrrdZlibStrategyDefault:
  default:
    break;
  }
  fmt[fmt_i] = 0;

  /* Create the gzFile for writing in the gzipped data. */
  if ((gzfout = _nrrdGzOpen(file, fmt)) == Z_NULL) {
    /* there was a problem */
    biffAddf(NRRD, "%s: error opening gzFile", me);
    return 1;
  }
  
  /* zlib can only handle data sizes up to UINT_MAX ==> if there's more
     than UINT_MAX bytes to write out, we write out in chunks */
  sizeChunk = AIR_MIN(sizeData, UINT_MAX);

  /* keeps track of what how much has been successfully written */
  sizeWrit = 0;
  /* Pointer to the chunks as we write them. */
  data = (char *)_data;
  
  /* Ok, now we can begin writing. Cast on third arg ok because of
     AIR_MIN use above */
  while ((error = _nrrdGzWrite(gzfout, data,
                               AIR_CAST(unsigned int, sizeChunk),
                               &wrote)) == 0 
         && wrote > 0) {
    /* Increment the data pointer to the next available spot. */
    data += wrote;
    sizeWrit += wrote;
    /* We only want to write as much data as we need, so we need to check
       to make sure that we don't write more data than is there.  This
       will reduce sizeChunk when we get to the last block (which may
       be smaller than sizeChunk).
    */
    if (sizeData >= sizeWrit
        && (unsigned int)(sizeData - sizeWrit) < sizeChunk)
      sizeChunk = sizeData - sizeWrit;
  }
  
  if (error) {
    biffAddf(NRRD, "%s: error writing to gzFile", me);
    return 1;
  }
  
  /* Check to see if we wrote out as much as we thought we should. */
  if (sizeWrit != sizeData) {
    biffAddf(NRRD, "%s: expected to write " _AIR_SIZE_T_CNV 
             " bytes, but only wrote " _AIR_SIZE_T_CNV,
             me, sizeData, sizeWrit);
    return 1;
  }
  
  /* Close the gzFile.  Since _nrrdGzClose does not close the FILE* we
     will not encounter problems when dataFile is closed later. */
  if (_nrrdGzClose(gzfout)) {
    biffAddf(NRRD, "%s: error closing gzFile", me);
    return 1;
  }
  
  return 0;
#else
  AIR_UNUSED(file);
  AIR_UNUSED(_data);
  AIR_UNUSED(elNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  biffAddf(NRRD, "%s: sorry, this nrrd not compiled with zlib "
           "(needed for gzip) enabled", me);
  return 1;
#endif
}

const NrrdEncoding
_nrrdEncodingGzip = {
  "gzip",      /* name */
  "raw.gz",    /* suffix */
  AIR_TRUE,    /* endianMatters */
  AIR_TRUE,   /* isCompression */
  _nrrdEncodingGzip_available,
  _nrrdEncodingGzip_read,
  _nrrdEncodingGzip_write
};

const NrrdEncoding *const
nrrdEncodingGzip = &_nrrdEncodingGzip;

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

static int
_nrrdEncodingGzip_available(void) {

#if TEEM_ZLIB
  return AIR_TRUE;
#else
  return AIR_FALSE;
#endif
}

/*
** Maximum size that allow zlib to try to read or write at once.
** The real limit is UINT_MAX, but a smaller value here permits
** exercising the multi-chunk capability of the code below.
*/
static unsigned int
_nrrdZlibMaxChunk = UINT_MAX;

/*
** nio->byteSkip < 0 functionality contributed by Katharina Quintus
*/
static int
_nrrdEncodingGzip_read(FILE *file, void *_data, size_t elNum,
                       Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdEncodingGzip_read";
#if TEEM_ZLIB
  size_t sizeData, sizeRed;
  int error;
  long int bi;
  unsigned int didread, sizeChunk, maxChunk;
  char *data;
  gzFile gzfin;
  airPtrPtrUnion appu;

  sizeData = nrrdElementSize(nrrd)*elNum;
  /* Create the gzFile for reading in the gzipped data. */
  if ((gzfin = _nrrdGzOpen(file, "rb")) == Z_NULL) {
    /* there was a problem */
    biffAddf(NRRD, "%s: error opening gzFile", me);
    return 1;
  }

  /* keeps track of how many bytes have been successfully read in */
  sizeRed = 0;

  /* zlib can only handle data sizes up to UINT_MAX ==> if there's more than
     UINT_MAX bytes to read in, we read in in chunks. However, we wrap a value
     _nrrdZlibMaxChunk around UINT_MAX for testing purposes.  Given how
     sizeChunk is used below, we also cap chunk size at _nrrdZlibMaxChunk/2 to
     prevent overflow. */
  maxChunk = _nrrdZlibMaxChunk/2;
  sizeChunk = AIR_CAST(unsigned int, AIR_MIN(sizeData, maxChunk));

  if (nio->byteSkip < 0) {
    /* We don't know the size of the size to skip before the data, so
       decompress the data first into a temporary memory buffer.  Then
       the byteskipping is then just memcpy-ing the appropriate region
       of memory from "buff" into the given "_data" pointer */
    char *buff;
    airArray *buffArr;
    long backwards;

    /* setting the airArray increment to twice the chunk size means that for
       headers that are small compared to the data, the airArray never
       actually has to reallocate.  The unit is 1 because we are managing
       the reading in terms of bytes (sizeof(char)==1 by definition) */
    buff = NULL;
    appu.c = &buff;
    buffArr = airArrayNew(appu.v, NULL, 1, 2*sizeChunk);
    airArrayLenSet(buffArr, sizeChunk);
    if (!( buffArr && buffArr->data )) {
      biffAddf(NRRD, "%s: couldn't initialize airArray\n", me);
      return 1;
    }

    /* we keep reading in chunks as long as there hasn't been an error,
       and we haven't hit EOF (EOF signified by read == 0).  Unlike the
       code below (for positive byteskip), we are obligated to read until
       the bitter end, and can't update sizeChunk to encompass only the
       required data. */
    while (!(error = _nrrdGzRead(gzfin, buff + sizeRed,
                                 sizeChunk, &didread))
           && didread > 0) {
      sizeRed += didread;
      if (didread >= sizeChunk) {
        /* we were able to read as much data as we requested, maybe there is
           more, so we need to make our temp buffer bigger */
        unsigned int newlen = buffArr->len + sizeChunk;
        if (newlen < buffArr->len) {
          biffAddf(NRRD, "%s: array size will exceed uint capacity", me);
          return 1;
        }
        airArrayLenSet(buffArr, newlen);
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
    /* backwards is (positive) number of bytes AFTER data that we ignore */
    backwards = -nio->byteSkip - 1;
    if (sizeRed < sizeData + AIR_CAST(size_t, backwards)) {
      char stmp1[AIR_STRLEN_SMALL], stmp2[AIR_STRLEN_SMALL];
      biffAddf(NRRD, "%s: expected %s bytes but received only %s", me,
               airSprintSize_t(stmp1, sizeData + AIR_CAST(size_t, backwards)),
               airSprintSize_t(stmp2, sizeRed));
      return 1;
    }
    /* also handles nio->byteSkip == -N-1 signifying extra N bytes at end */
    memcpy(_data, buff + sizeRed - sizeData - backwards, sizeData);
    airArrayNuke(buffArr);
  } else {
    /* no negative byteskip: after byteskipping, we can read directly
       into given data buffer */
    if (nio->byteSkip > 0) {
      for (bi=0; bi<nio->byteSkip; bi++) {
        unsigned char b;
        /* Check to see if a single byte was able to be read. */
        if (_nrrdGzRead(gzfin, &b, 1, &didread) != 0 || didread != 1) {
          biffAddf(NRRD, "%s: hit an error skipping byte %ld of %ld",
                   me, bi, nio->byteSkip);
          return 1;
        }
      }
    }
    /* Pointer to chunks as we read them. */
    data = AIR_CAST(char *, _data);
    while (!(error = _nrrdGzRead(gzfin, data, sizeChunk, &didread))
           && didread > 0) {
      /* Increment the data pointer to the next available chunk. */
      data += didread;
      sizeRed += didread;
      /* We only want to read as much data as we need, so we need to check
         to make sure that we don't request data that might be there but that
         we don't want.  This will reduce sizeChunk when we get to the last
         block (which may be smaller than the original sizeChunk). */
      if (sizeData >= sizeRed
          && sizeData - sizeRed < sizeChunk) {
        sizeChunk = AIR_CAST(unsigned int, sizeData - sizeRed);
      }
    }
    if (error) {
      biffAddf(NRRD, "%s: error reading from gzFile", me);
      return 1;
    }
    /* Check to see if we got out as much as we thought we should. */
    if (sizeRed != sizeData) {
      char stmp1[AIR_STRLEN_SMALL], stmp2[AIR_STRLEN_SMALL];
      biffAddf(NRRD, "%s: expected %s bytes but received %s", me,
               airSprintSize_t(stmp1, sizeData),
               airSprintSize_t(stmp2, sizeRed));
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

static int
_nrrdEncodingGzip_write(FILE *file, const void *_data, size_t elNum,
                        const Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdEncodingGzip_write";
#if TEEM_ZLIB
  size_t sizeData, sizeWrit;
  int fmt_i=0, error;
  const char *data;
  char fmt[4];
  gzFile gzfout;
  unsigned int wrote, sizeChunk;

  sizeData = nrrdElementSize(nrrd)*elNum;

  /* Set format string based on the NrrdIoState parameters. */
  fmt[fmt_i++] = 'w';
  if (0 <= nio->zlibLevel && nio->zlibLevel <= 9)
    fmt[fmt_i++] = AIR_CAST(char, '0' + nio->zlibLevel);
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

  /* zlib can only handle data sizes up to UINT_MAX ==> if there's more than
     UINT_MAX bytes to write out, we write out in chunks.  As above, we wrap
     _nrrdZlibMaxChunk around UINT_MAX for testing purposes. */
  sizeChunk = AIR_CAST(unsigned int, AIR_MIN(sizeData, _nrrdZlibMaxChunk));

  /* keeps track of what how much has been successfully written */
  sizeWrit = 0;
  /* Pointer to the chunks as we write them. */
  data = AIR_CAST(const char *, _data);

  /* Ok, now we can begin writing. */
  while ((error = _nrrdGzWrite(gzfout, AIR_CVOIDP(data),
                               sizeChunk, &wrote)) == 0
         && wrote > 0) {
    /* Increment the data pointer to the next available spot. */
    data += wrote;
    sizeWrit += wrote;
    /* We only want to write as much data as we need, so we need to check
       to make sure that we don't write more data than is there.  This
       will reduce sizeChunk when we get to the last block (which may
       be smaller than the original sizeChunk).
    */
    if (sizeData >= sizeWrit
        && sizeData - sizeWrit < sizeChunk)
      sizeChunk = AIR_CAST(unsigned int, sizeData - sizeWrit);
  }

  if (error) {
    biffAddf(NRRD, "%s: error writing to gzFile", me);
    return 1;
  }

  /* Check to see if we wrote out as much as we thought we should. */
  if (sizeWrit != sizeData) {
    char stmp1[AIR_STRLEN_SMALL], stmp2[AIR_STRLEN_SMALL];
    biffAddf(NRRD, "%s: expected to write %s bytes, but only wrote %s", me,
             airSprintSize_t(stmp1, sizeData),
             airSprintSize_t(stmp2, sizeWrit));
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

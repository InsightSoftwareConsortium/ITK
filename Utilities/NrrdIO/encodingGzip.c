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

int
_nrrdEncodingGzip_available(void) {

#if TEEM_ZLIB
  return AIR_TRUE;
#else
  return AIR_FALSE;
#endif
}

int
_nrrdEncodingGzip_read(Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdEncodingGzip_read", err[AIR_STRLEN_MED];
#if TEEM_ZLIB
  size_t num, bsize, size, total_read;
  int block_size, i, error=0;
  unsigned int read;
  char *data;
  gzFile gzfin;
  
  if (nio->skipData) {
    return 0;
  }
  num = nrrdElementNumber(nrrd);
  bsize = num * nrrdElementSize(nrrd);
  size = bsize;
  if (num != bsize/nrrdElementSize(nrrd)) {
    fprintf(stderr,
            "%s: PANIC: \"size_t\" can't represent byte-size of data.\n", me);
    exit(1);
  }

  /* Allocate memory for the incoming data. */
  if (_nrrdCalloc(nrrd, nio)) {
    sprintf(err, "%s: couldn't allocate sufficient memory for all data", me);
    biffAdd(NRRD, err); return 1;
  }

  /* Create the gzFile for reading in the gzipped data. */
  if ((gzfin = _nrrdGzOpen(nio->dataFile, "r")) == Z_NULL) {
    /* there was a problem */
    sprintf(err, "%s: error opening gzFile", me);
    biffAdd(NRRD, err);
    return 1;
  }

  /* Here is where we do the byte skipping. */
  for(i = 0; i < nio->byteSkip; i++) {
    unsigned char b;
    /* Check to see if a single byte was able to be read. */
    if (_nrrdGzRead(gzfin, &b, 1, &read) != 0 || read != 1) {
      sprintf(err, "%s: hit an error skipping byte %d of %d",
              me, i, nio->byteSkip);
      biffAdd(NRRD, err);
      return 1;
    }
  }
  
  /* zlib can handle data sizes up to UINT_MAX, so we can't just 
     pass in the size, because it might be too large for an 
     unsigned int.  Therefore it must be read in chunks 
     if the size is larger than UINT_MAX. */
  if (size <= UINT_MAX) {
    block_size = (unsigned int)size;
  } else {
    block_size = UINT_MAX;
  }

  /* This counter will help us to make sure that we read as much data
     as we think we should. */
  total_read = 0;
  /* Pointer to the blocks as we read them. */
  data = nrrd->data;
  
  /* Ok, now we can begin reading. */
  while ((error = _nrrdGzRead(gzfin, data, block_size, &read)) == 0 && read > 0) {
    /* Increment the data pointer to the next available spot. */
    data += read; 
    total_read += read;
    /* We only want to read as much data as we need, so we need to check
       to make sure that we don't request data that might be there but that
       we don't want.  This will reduce block_size when we get to the last
       block (which may be smaller than block_size).
    */
    if (size - total_read < block_size)
      block_size = (unsigned int)(size - total_read);
  }

  /* Check if we stopped because of an error. */
  if (error != 0)
  {
    sprintf(err, "%s: error reading from gzFile", me);
    biffAdd(NRRD, err);
    return 1;
  }

  /* Close the gzFile.  Since _nrrdGzClose does not close the FILE* we
     will not encounter problems when nio->dataFile is closed later. */
  if (_nrrdGzClose(gzfin) != 0) {
    sprintf(err, "%s: error closing gzFile", me);
    biffAdd(NRRD, err);
    return 1;
  }
  
  /* Check to see if we got out as much as we thought we should. */
  if (total_read != size) {
    sprintf(err, "%s: expected " _AIR_SIZE_T_FMT " bytes and received "
            _AIR_SIZE_T_FMT " bytes",
            me, size, total_read);
    biffAdd(NRRD, err);
    return 1;
  }
  
  return 0;
#else
  sprintf(err, "%s: sorry, this nrrd not compiled with gzip enabled", me);
  biffAdd(NRRD, err); return 1;
#endif
}

int
_nrrdEncodingGzip_write(const Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdEncodingGzip_write", err[AIR_STRLEN_MED];
#if TEEM_ZLIB
  size_t num, bsize, size, total_written;
  int block_size, fmt_i=0, error=0;
  char *data, fmt[4];
  gzFile gzfout;
  unsigned int wrote;
  
  if (nio->skipData) {
    return 0;
  }
  /* this shouldn't actually be necessary ... */
  if (!nrrdElementSize(nrrd)) {
    sprintf(err, "%s: nrrd reports zero element size!", me);
    biffAdd(NRRD, err); return 1;
  }
  num = nrrdElementNumber(nrrd);
  if (!num) {
    sprintf(err, "%s: calculated number of elements to be zero!", me);
    biffAdd(NRRD, err); return 1;
  }
  bsize = num * nrrdElementSize(nrrd);
  size = bsize;
  if (num != bsize/nrrdElementSize(nrrd)) {
    fprintf(stderr,
            "%s: PANIC: \"size_t\" can't represent byte-size of data.\n", me);
    exit(1);
  }

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
  if ((gzfout = _nrrdGzOpen(nio->dataFile, fmt)) == Z_NULL) {
    /* there was a problem */
    sprintf(err, "%s: error opening gzFile", me);
    biffAdd(NRRD, err);
    return 1;
  }

  /* zlib can handle data sizes up to UINT_MAX, so we can't just 
     pass in the size, because it might be too large for an 
     unsigned int.  Therefore it must be read in chunks 
     if the size is larger than UINT_MAX. */
  if (size <= UINT_MAX) {
    block_size = (unsigned int)size;
  } else {
    block_size = UINT_MAX;
  }

  /* This counter will help us to make sure that we write as much data
     as we think we should. */
  total_written = 0;
  /* Pointer to the blocks as we write them. */
  data = nrrd->data;
  
  /* Ok, now we can begin writing. */
  while ((error = _nrrdGzWrite(gzfout, data, block_size, &wrote)) == 0 
         && wrote > 0) {
    /* Increment the data pointer to the next available spot. */
    data += wrote;
    total_written += wrote;
    /* We only want to write as much data as we need, so we need to check
       to make sure that we don't write more data than is there.  This
       will reduce block_size when we get to the last block (which may
       be smaller than block_size).
    */
    if (size - total_written < block_size)
      block_size = (unsigned int)(size - total_written);
  }
  
  /* Check if we stopped because of an error. */
  if (error != 0)
  {
    sprintf(err, "%s: error reading from gzFile", me);
    biffAdd(NRRD, err);
    return 1;
  }

  /* Close the gzFile.  Since _nrrdGzClose does not close the FILE* we
     will not encounter problems when nio->dataFile is closed later. */
  if (_nrrdGzClose(gzfout) != 0) {
    sprintf(err, "%s: error closing gzFile", me);
    biffAdd(NRRD, err);
    return 1;
  }
  
  /* Check to see if we got out as much as we thought we should. */
  if (total_written != size) {
    sprintf(err, "%s: expected to write " _AIR_SIZE_T_FMT " bytes, but only "
            "wrote " _AIR_SIZE_T_FMT,
            me, size, total_written);
    biffAdd(NRRD, err);
    return 1;
  }
  
  return 0;
#else
  sprintf(err, "%s: sorry, this nrrd not compiled with zlib "
          "(needed for gzip) enabled", me);
  biffAdd(NRRD, err); return 1;
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

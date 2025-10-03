/*
  NrrdIO: C library for NRRD file IO (with optional compressions)
  Copyright (C) 2009--2025  University of Chicago
  Copyright (C) 2005--2008  Gordon Kindlmann
  Copyright (C) 1998--2004  University of Utah

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

#if TEEM_BZIP2
#  include <bzlib.h>
#endif

static int
_nrrdEncodingBzip2_available(void) {

#if TEEM_BZIP2
  return AIR_TRUE;
#else
  return AIR_FALSE;
#endif
}

static int /* Biff: 1 */
_nrrdEncodingBzip2_read(FILE *file, void *_data, size_t elNum, Nrrd *nrrd,
                        NrrdIoState *nio) {
  static const char me[] = "_nrrdEncodingBzip2_read";
#if TEEM_BZIP2
  size_t bsize, total_read, block_size;
  int read, bzerror = BZ_OK;
  long int bi;
  char *data;
  BZFILE *bzfin;

  bsize = nrrdElementSize(nrrd) * elNum;

  /* Create the BZFILE* for reading in the gzipped data. */
  bzfin = BZ2_bzReadOpen(&bzerror, file, 0, 0, NULL, 0);
  if (bzerror != BZ_OK) {
    /* there was a problem */
    biffAddf(NRRD, "%s: error opening BZFILE: %s", me, BZ2_bzerror(bzfin, &bzerror));
    BZ2_bzReadClose(&bzerror, bzfin);
    return 1;
  }

  /* Here is where we do the byte skipping. */
  for (bi = 0; bi < nio->byteSkip; bi++) {
    unsigned char b;
    /* Check to see if a single byte was able to be read. */
    read = BZ2_bzRead(&bzerror, bzfin, &b, 1);
    if (read != 1 || bzerror != BZ_OK) {
      biffAddf(NRRD, "%s: hit an error skipping byte %ld of %ld: %s", me, bi,
               nio->byteSkip, BZ2_bzerror(bzfin, &bzerror));
      return 1;
    }
  }

  /* bzip2 can handle data sizes up to INT_MAX, so we can't just
     pass in the bsize, because it might be too large for an int.
     Therefore it must be read in chunks if the size is larger
     than INT_MAX. */
  if (bsize <= INT_MAX) {
    block_size = bsize;
  } else {
    block_size = INT_MAX;
  }

  /* This counter will help us to make sure that we read as much data
     as we think we should. */
  total_read = 0;
  /* Pointer to the blocks as we read them. */
  data = (char *)_data;

  /* Ok, now we can begin reading. */
  bzerror = BZ_OK;
  while ((read = BZ2_bzRead(&bzerror, bzfin, data, block_size))
         && (BZ_OK == bzerror || BZ_STREAM_END == bzerror)) {
    /* Increment the data pointer to the next available spot. */
    data += read;
    total_read += read;
    /* We only want to read as much data as we need, so we need to check
       to make sure that we don't request data that might be there but that
       we don't want.  This will reduce block_size when we get to the last
       block (which may be smaller than block_size).
    */
    if (bsize >= total_read && bsize - total_read < block_size)
      block_size = bsize - total_read;
  }

  if (!(BZ_OK == bzerror || BZ_STREAM_END == bzerror)) {
    biffAddf(NRRD, "%s: error reading from BZFILE: %s", me,
             BZ2_bzerror(bzfin, &bzerror));
    return 1;
  }

  /* Close the BZFILE. */
  BZ2_bzReadClose(&bzerror, bzfin);
  if (BZ_OK != bzerror) {
    biffAddf(NRRD, "%s: error closing BZFILE: %s", me, BZ2_bzerror(bzfin, &bzerror));
    return 1;
  }

  /* Check to see if we got out as much as we thought we should. */
  if (total_read != bsize) {
    char stmp[2][AIR_STRLEN_SMALL + 1];
    biffAddf(NRRD, "%s: expected %s bytes but received %s", me,
             airSprintSize_t(stmp[0], bsize), airSprintSize_t(stmp[1], total_read));
    return 1;
  }

  return 0;
#else
  AIR_UNUSED(file);
  AIR_UNUSED(_data);
  AIR_UNUSED(elNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  biffAddf(NRRD, "%s: sorry, this nrrd not compiled with bzip2 enabled", me);
  return 1;
#endif
}

static int /* Biff: 1 */
_nrrdEncodingBzip2_write(FILE *file, const void *_data, size_t elNum, const Nrrd *nrrd,
                         NrrdIoState *nio) {
  static const char me[] = "_nrrdEncodingBzip2_write";
#if TEEM_BZIP2
  size_t bsize, total_written, block_size;
  int bs, bzerror = BZ_OK;
  char *data;
  BZFILE *bzfout;

  bsize = nrrdElementSize(nrrd) * elNum;

  /* Set compression block size. */
  if (1 <= nio->bzip2BlockSize && nio->bzip2BlockSize <= 9) {
    bs = nio->bzip2BlockSize;
  } else {
    bs = 9;
  }
  /* Open bzfile for writing. Verbosity and work factor are set
     to default values. */
  bzfout = BZ2_bzWriteOpen(&bzerror, file, bs, 0, 0);
  if (BZ_OK != bzerror) {
    biffAddf(NRRD, "%s: error opening BZFILE: %s", me, BZ2_bzerror(bzfout, &bzerror));
    BZ2_bzWriteClose(&bzerror, bzfout, 0, NULL, NULL);
    return 1;
  }

  /* bzip2 can handle data sizes up to INT_MAX, so we can't just
     pass in the bsize, because it might be too large for an int.
     Therefore it must be read in chunks if the bsize is larger
     than INT_MAX. */
  if (bsize <= INT_MAX) {
    block_size = bsize;
  } else {
    block_size = INT_MAX;
  }

  /* This counter will help us to make sure that we write as much data
     as we think we should. */
  total_written = 0;
  /* Pointer to the blocks as we write them. */
  data = (char *)_data;

  /* Ok, now we can begin writing. */
  bzerror = BZ_OK;
  while (bsize - total_written > block_size) {
    BZ2_bzWrite(&bzerror, bzfout, data, block_size);
    if (BZ_OK != bzerror) break;
    /* Increment the data pointer to the next available spot. */
    data += block_size;
    total_written += block_size;
  }
  /* write the last (possibly smaller) block when its humungous data;
     write the whole data when its small */
  if (BZ_OK == bzerror) {
    block_size = bsize >= total_written ? bsize - total_written : 0;
    BZ2_bzWrite(&bzerror, bzfout, data, block_size);
    total_written += block_size;
  }

  if (BZ_OK != bzerror) {
    biffAddf(NRRD, "%s: error writing to BZFILE: %s", me, BZ2_bzerror(bzfout, &bzerror));
    return 1;
  }

  /* Close the BZFILE. */
  BZ2_bzWriteClose(&bzerror, bzfout, 0, NULL, NULL);
  if (BZ_OK != bzerror) {
    biffAddf(NRRD, "%s: error closing BZFILE: %s", me, BZ2_bzerror(bzfout, &bzerror));
    return 1;
  }

  /* Check to see if we got out as much as we thought we should. */
  if (total_written != bsize) {
    char stmp[2][AIR_STRLEN_SMALL + 1];
    biffAddf(NRRD, "%s: expected to write %s bytes, but only wrote %s", me,
             airSprintSize_t(stmp[0], bsize), airSprintSize_t(stmp[1], total_written));
    return 1;
  }

  return 0;
#else
  AIR_UNUSED(file);
  AIR_UNUSED(_data);
  AIR_UNUSED(elNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  biffAddf(NRRD, "%s: sorry, this nrrd not compiled with bzip2 enabled", me);
  return 1;
#endif
}

const NrrdEncoding _nrrdEncodingBzip2 = {"bzip2",   /* name */
                                         "raw.bz2", /* suffix */
                                         AIR_TRUE,  /* endianMatters */
                                         AIR_TRUE,  /* isCompression */
                                         _nrrdEncodingBzip2_available,
                                         _nrrdEncodingBzip2_read,
                                         _nrrdEncodingBzip2_write};

const NrrdEncoding *const nrrdEncodingBzip2 = &_nrrdEncodingBzip2;

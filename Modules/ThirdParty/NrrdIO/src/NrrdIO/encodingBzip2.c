/*
  NrrdIO: C library for NRRD file IO (with optional compressions)
  Copyright (C) 2009--2026  University of Chicago
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

#ifdef TEEM_BZIP2
#  if defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wreserved-identifier"
/*   because bzlib.h's "#define _BZLIB_H" has reserved identifier _BZLIB_H */
#  endif
#  include <bzlib.h>
#  if defined(__clang__)
#    pragma clang diagnostic pop
#  endif
#endif

static int
encodingBzip2_available(void) {

#ifdef TEEM_BZIP2
  return AIR_TRUE;
#else
  return AIR_FALSE;
#endif
}

static int /* Biff: 1 */
encodingBzip2_read(FILE *file, void *_data, size_t elNum, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[] = "encodingBzip2_read";
#ifdef TEEM_BZIP2
  size_t data_size, total_read, block_size;
  int read, bzerror = BZ_OK;
  long int bi;
  char *data;
  BZFILE *bzfin;

  data_size = nrrdElementSize(nrrd) * elNum;

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
     pass in the data_size, because it might be too large for an int.
     Therefore it must be read in chunks if the size is larger
     than INT_MAX. */
  if (data_size <= INT_MAX) {
    block_size = data_size;
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
  while ((read = BZ2_bzRead(&bzerror, bzfin, data, AIR_INT(block_size)))
         && (BZ_OK == bzerror || BZ_STREAM_END == bzerror)) {
    /* BZ2_bzRead really does return a signed int */
    if (read < 0) {
      /* this probably can't happen,
         but adding this test to ensure that cast to size_t is safe */
      biffAddf(NRRD, "%s: confused by negative return %d from BZ2_bzRead", me, read);
      return 1;
    }
    /* Increment the data pointer to the next available spot. */
    data += read;
    total_read += AIR_SIZE_T(read);
    /* We only want to read as much data as we need, so we need to check
       to make sure that we don't request data that might be there but that
       we don't want.  This will reduce block_size when we get to the last
       block (which may be smaller than block_size).
    */
    if (data_size >= total_read && data_size - total_read < block_size)
      block_size = data_size - total_read;
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
  if (total_read != data_size) {
    char stmp[2][AIR_STRLEN_SMALL + 1];
    biffAddf(NRRD, "%s: expected %s bytes but received %s", me,
             airSprintSize_t(stmp[0], data_size), airSprintSize_t(stmp[1], total_read));
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

/* Considering BZ2_bzWrite https://sourceware.org/pub/bzip2/docs/v101/manual_3.html
   The third arg is `void *data`.
   It should be `const void *data`, no?
   Meanwhile, we need a local `const char *` pointer to point to the part of the
   data not yet written.  So we sneakily alias these two kinds of pointers together. */
typedef union {
  void *vvp;       /* volatile void pointer */
  const char *ccp; /* const char * pointer */
} ptrAlias;

static int /* Biff: 1 */
encodingBzip2_write(FILE *file, const void *_data, size_t elNum, const Nrrd *nrrd,
                    NrrdIoState *nio) {
  static const char me[] = "encodingBzip2_write";
#ifdef TEEM_BZIP2
  size_t data_size, total_written, block_size;
  int bs, bzerror = BZ_OK;
  ptrAlias dataAlias;
  BZFILE *bzfout;

  data_size = nrrdElementSize(nrrd) * elNum;

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
     pass in the data_size, because it might be too large for an int.
     Therefore it must be read in chunks if the data_size is larger
     than INT_MAX. */
  if (data_size <= INT_MAX) {
    block_size = data_size;
  } else {
    block_size = INT_MAX;
  }

  /* This counter will help us to make sure that we write as much data
     as we think we should. */
  total_written = 0;
  /* Pointer to the blocks as we write them. */
  dataAlias.ccp = (const char *)_data;

  /* Ok, now we can begin writing. */
  bzerror = BZ_OK;
  while (data_size - total_written > block_size) {
    BZ2_bzWrite(&bzerror, bzfout, dataAlias.vvp, AIR_INT(block_size));
    if (BZ_OK != bzerror) break;
    /* Increment the data pointer to the next available spot. */
    dataAlias.ccp += block_size;
    total_written += block_size;
  }
  /* write the last (possibly smaller) block when its humungous data;
     write the whole data when its small */
  if (BZ_OK == bzerror) {
    block_size = data_size >= total_written ? data_size - total_written : 0;
    BZ2_bzWrite(&bzerror, bzfout, dataAlias.vvp, AIR_INT(block_size));
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
  if (total_written != data_size) {
    char stmp[2][AIR_STRLEN_SMALL + 1];
    biffAddf(NRRD, "%s: expected to write %s bytes, but only wrote %s", me,
             airSprintSize_t(stmp[0], data_size),
             airSprintSize_t(stmp[1], total_written));
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

const NrrdEncoding nrrd__EncodingBzip2 = {"bzip2",   /* name */
                                          "raw.bz2", /* suffix */
                                          AIR_TRUE,  /* endianMatters */
                                          AIR_TRUE,  /* isCompression */
                                          encodingBzip2_available,
                                          encodingBzip2_read,
                                          encodingBzip2_write};

const NrrdEncoding *const nrrdEncodingBzip2 = &nrrd__EncodingBzip2;

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

static int
_nrrdEncodingRaw_available(void) {

  return AIR_TRUE;
}

static int /* Biff: 1 */
_nrrdEncodingRaw_read(FILE *file, void *data, size_t elementNum, Nrrd *nrrd,
                      NrrdIoState *nio) {
  static const char me[] = "_nrrdEncodingRaw_read";
  size_t ret, bsize;
  int car;
  long savePos;
  char *data_c;
  size_t elementSize, maxChunkSize, remainderValue, chunkSize;
  size_t retTmp;
  char stmp[3][AIR_STRLEN_SMALL + 1];

  bsize = nrrdElementSize(nrrd) * elementNum;

  /* HEY: There's a bug in fread/fwrite in gcc 4.2.1 (with SnowLeopard).
          When it reads/writes a >=2GB data array, it pretends to succeed
          (i.e. the return value is the right number) but it hasn't
          actually read/written the data.  The work-around is to loop
          over the data, reading/writing 1GB (or smaller) chunks.         */
  ret = 0;
  data_c = (char *)data;
  elementSize = nrrdElementSize(nrrd);
  maxChunkSize = 1024 * 1024 * 1024 / elementSize;
  while (ret < elementNum) {
    remainderValue = elementNum - ret;
    if (remainderValue < maxChunkSize) {
      chunkSize = remainderValue;
    } else {
      chunkSize = maxChunkSize;
    }
    retTmp = fread(&(data_c[ret * elementSize]), elementSize, chunkSize, file);
    ret += retTmp;
    if (retTmp != chunkSize) {
      biffAddf(NRRD,
               "%s: fread got only %s %s-sized things, not %s "
               "(%g%% of expected)",
               me, airSprintSize_t(stmp[0], ret),
               airSprintSize_t(stmp[1], nrrdElementSize(nrrd)),
               airSprintSize_t(stmp[2], elementNum),
               100.0 * AIR_CAST(double, ret) / AIR_CAST(double, elementNum));
      return 1;
    }
  }

  car = fgetc(file);
  if (EOF != car) {
    if (nio && nio->verbose >= 1) {
      fprintf(stderr,
              "%s: WARNING: finished reading raw data, "
              "but file not at EOF\n",
              me);
    }
    ungetc(car, file);
  }
  if (nio && nio->verbose >= 2 && nio->byteSkip && stdin != file) {
    savePos = ftell(file);
    if (!fseek(file, 0, SEEK_END)) {
      double frac = (AIR_CAST(double, bsize) / AIR_CAST(double, ftell(file) + 1));
      fprintf(stderr, "(%s: used %g%% of file for nrrd data)\n", me, 100.0 * frac);
      fseek(file, savePos, SEEK_SET);
    }
  }

  return 0;
}

static int /* Biff: 1 */
_nrrdEncodingRaw_write(FILE *file, const void *data, size_t elementNum, const Nrrd *nrrd,
                       NrrdIoState *nio) {
  static const char me[] = "_nrrdEncodingRaw_write";
  size_t ret;
  const char *data_c;
  size_t elementSize, maxChunkSize, remainderValue, chunkSize;
  size_t retTmp;
  char stmp[3][AIR_STRLEN_SMALL + 1];

  AIR_UNUSED(nio);

  /* HEY: There's a bug in fread/fwrite in gcc 4.2.1 (with SnowLeopard).
          When it reads/writes a >=2GB data array, it pretends to succeed
          (i.e. the return value is the right number) but it hasn't
          actually read/written the data.  The work-around is to loop
          over the data, reading/writing 1GB (or smaller) chunks.         */
  ret = 0;
  data_c = AIR_CAST(const char *, data);
  elementSize = nrrdElementSize(nrrd);
  maxChunkSize = 1024 * 1024 * 1024 / elementSize;
  while (ret < elementNum) {
    remainderValue = elementNum - ret;
    if (remainderValue < maxChunkSize) {
      chunkSize = remainderValue;
    } else {
      chunkSize = maxChunkSize;
    }
    retTmp = fwrite(&(data_c[ret * elementSize]), elementSize, chunkSize, file);
    ret += retTmp;
    if (retTmp != chunkSize) {
      biffAddf(NRRD,
               "%s: fwrite wrote only %s %s-sized things, not %s "
               "(%g%% of expected)",
               me, airSprintSize_t(stmp[0], ret),
               airSprintSize_t(stmp[1], nrrdElementSize(nrrd)),
               airSprintSize_t(stmp[2], elementNum),
               100.0 * AIR_CAST(double, ret) / AIR_CAST(double, elementNum));
      return 1;
    }
  }

  fflush(file);
  /*
  if (ferror(file)) {
    biffAddf(NRRD, "%s: ferror returned non-zero", me);
    return 1;
  }
  */

  return 0;
}

const NrrdEncoding _nrrdEncodingRaw = {"raw",     /* name */
                                       "raw",     /* suffix */
                                       AIR_TRUE,  /* endianMatters */
                                       AIR_FALSE, /* isCompression */
                                       _nrrdEncodingRaw_available,
                                       _nrrdEncodingRaw_read,
                                       _nrrdEncodingRaw_write};

const NrrdEncoding *const nrrdEncodingRaw = &_nrrdEncodingRaw;

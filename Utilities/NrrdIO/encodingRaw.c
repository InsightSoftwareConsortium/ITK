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
_nrrdEncodingRaw_available(void) {

  return AIR_TRUE;
}

int
_nrrdEncodingRaw_read(Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdEncodingRaw_read", err[AIR_STRLEN_MED];
  size_t num, bsize, size, ret, dio;
  int car;
  long savePos;

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

  if (nio->format->usesDIO) {
    dio = airDioTest(size, nio->dataFile, NULL);
  } else {
    dio = airNoDio_format;
  }
  /* Notice that DIO will NOT be used if we are given pre-allocated memory
     that fits the new nrrd.  The memory could be used if we were to check
     its alignment here, but there currently is a laziness problem */
  if (airNoDio_okay == dio 
      && !(nio->oldData && bsize == nio->oldDataSize) ) {
    if (nio->format->usesDIO) {
      if (3 <= nrrdStateVerboseIO) {
        fprintf(stderr, "with direct I/O ");
      }
      if (2 <= nrrdStateVerboseIO) {
        fprintf(stderr, "... ");
        fflush(stderr);
      }
    }
    /* airDioRead includes the memory allocation */
    ret = airDioRead(nio->dataFile, &(nrrd->data), size);
    if (size != ret) {
      sprintf(err, "%s: airDioRead() failed", me);
      biffAdd(NRRD, err); return 1;
    }
  } else {
    if (airNoDio_okay == dio && nio->oldData && bsize == nio->oldDataSize) {
      if (nrrdStateVerboseIO) {
        fprintf(stderr,
                "%s: sorry, too lazy to use existing memory for DIO\n", me); 
      }
    }
    if (_nrrdCalloc(nrrd, nio)) {
      sprintf(err, "%s: couldn't allocate sufficient memory for all data", me);
      biffAdd(NRRD, err); return 1;
    }
    if (AIR_DIO && nio->format->usesDIO) {
      if (3 <= nrrdStateVerboseIO) {
        fprintf(stderr, "with fread()");
        if (4 <= nrrdStateVerboseIO) {
          fprintf(stderr, " (why no DIO: %s)", airNoDioErr(dio));
        }
      }
      if (2 <= nrrdStateVerboseIO) {
        fprintf(stderr, " ... ");
        fflush(stderr);
      }
    }
    ret = fread(nrrd->data, nrrdElementSize(nrrd), num, nio->dataFile);
    if (ret != num) {
      sprintf(err, "%s: fread() got only " _AIR_SIZE_T_FMT " %d-byte things, "
              "not " _AIR_SIZE_T_FMT ,
              me, ret, nrrdElementSize(nrrd), num);
      biffAdd(NRRD, err); return 1;
    }
  }
  car = fgetc(nio->dataFile);
  if (EOF != car) {
    fprintf(stderr, "%s: WARNING: finished reading raw data, "
            "but file not at EOF\n", me);
    ungetc(car, nio->dataFile);
  }
  if (nrrdStateVerboseIO && nio->byteSkip && stdin != nio->dataFile) {
    savePos = ftell(nio->dataFile);
    if (!fseek(nio->dataFile, 0, SEEK_END)) {
      fprintf(stderr, "(%s: used %g%% of file for nrrd data)\n",
              me, (double)100*bsize/(ftell(nio->dataFile) + 1));
      fseek(nio->dataFile, savePos, SEEK_SET);
    }
  }

  return 0;
}

int
_nrrdEncodingRaw_write(const Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdEncodingRaw_write", err[AIR_STRLEN_MED];
  size_t size, ret, dio;
  
  if (nio->skipData) {
    return 0;
  }
  /* this shouldn't actually be necessary ... */
  if (!nrrdElementSize(nrrd)) {
    sprintf(err, "%s: nrrd reports zero element size!", me);
    biffAdd(NRRD, err); return 1;
  }
  size = nrrdElementNumber(nrrd) * nrrdElementSize(nrrd);
  if (nrrdElementNumber(nrrd) != size/nrrdElementSize(nrrd)) {
    sprintf(err, "%s: \"size_t\" can't represent byte-size of data.", me);
    biffAdd(NRRD, err); return 1;
  }

  if (nio->format->usesDIO) {
    dio = airDioTest(size, nio->dataFile, nrrd->data);
  } else {
    dio = airNoDio_format;
  }
  if (airNoDio_okay == dio) {
    if (nio->format->usesDIO) {
      if (3 <= nrrdStateVerboseIO) {
        fprintf(stderr, "with direct I/O ");
      }
      if (2 <= nrrdStateVerboseIO) {
        fprintf(stderr, "... ");
        fflush(stderr);
      }
    }
    ret = airDioWrite(nio->dataFile, nrrd->data, size);
    if (size != ret) {
      sprintf(err, "%s: airDioWrite failed", me);
      biffAdd(NRRD, err); return 1;
    }
  } else {
    if (AIR_DIO && nio->format->usesDIO) {
      if (3 <= nrrdStateVerboseIO) {
        fprintf(stderr, "with fwrite()");
        if (4 <= nrrdStateVerboseIO) {
          fprintf(stderr, " (why no DIO: %s)", airNoDioErr(dio));
        }
      }
      if (2 <= nrrdStateVerboseIO) {
        fprintf(stderr, " ... ");
        fflush(stderr);
      }
    }
    ret = fwrite(nrrd->data, nrrdElementSize(nrrd),
                 nrrdElementNumber(nrrd), nio->dataFile);
    if (ret != nrrdElementNumber(nrrd)) {
      sprintf(err, "%s: fwrite() wrote only " _AIR_SIZE_T_FMT 
              " %d-byte things, not " _AIR_SIZE_T_FMT ,
              me, ret, nrrdElementSize(nrrd), nrrdElementNumber(nrrd));
      biffAdd(NRRD, err); return 1;
    }
    fflush(nio->dataFile);
    /*
    if (ferror(nio->dataFile)) {
      sprintf(err, "%s: ferror returned non-zero", me);
      biffAdd(NRRD, err); return 1;
    }
    */
  }
  return 0;
}

const NrrdEncoding
_nrrdEncodingRaw = {
  "raw",      /* name */
  "raw",      /* suffix */
  AIR_TRUE,   /* endianMatters */
  AIR_FALSE,  /* isCompression */
  _nrrdEncodingRaw_available,
  _nrrdEncodingRaw_read,
  _nrrdEncodingRaw_write
};

const NrrdEncoding *const
nrrdEncodingRaw = &_nrrdEncodingRaw;

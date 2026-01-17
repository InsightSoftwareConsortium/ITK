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

static const int _nrrdWriteHexTable[16] = {'0', '1', '2', '3', '4', '5', '6', '7',
                                           '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

/*
** -2: not allowed, error
** -1: whitespace
** [0,15]: values
*/
/* clang-format off */
static const int
_nrrdReadHexTable[128] = {
/* 0   1   2   3   4   5   6   7   8   9 */
  -2, -2, -2, -2, -2, -2, -2, -2, -2, -1,  /*   0 */
  -1, -1, -1, -1, -2, -2, -2, -2, -2, -2,  /*  10 */
  -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,  /*  20 */
  -2, -2, -1, -2, -2, -2, -2, -2, -2, -2,  /*  30 */
  -2, -2, -2, -2, -2, -2, -2, -2,  0,  1,  /*  40 */
   2,  3,  4,  5,  6,  7,  8,  9, -2, -2,  /*  50 */
  -2, -2, -2, -2, -2, 10, 11, 12, 13, 14,  /*  60 */
  15, -2, -2, -2, -2, -2, -2, -2, -2, -2,  /*  70 */
  -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,  /*  80 */
  -2, -2, -2, -2, -2, -2, -2, 10, 11, 12,  /*  90 */
  13, 14, 15, -2, -2, -2, -2, -2, -2, -2,  /* 100 */
  -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,  /* 110 */
  -2, -2, -2, -2, -2, -2, -2, -2           /* 120 */
};
/* clang-format on */

static int
_nrrdEncodingHex_available(void) {

  return AIR_TRUE;
}

static int /* Biff: 1 */
_nrrdEncodingHex_read(FILE *file, void *_data, size_t elNum, Nrrd *nrrd,
                      NrrdIoState *nio) {
  static const char me[] = "_nrrdEncodingHex_read";
  size_t nibIdx, nibNum;
  unsigned char *data;
  int car = 0, nib;

  AIR_UNUSED(nio);
  data = AIR_CAST(unsigned char *, _data);
  nibIdx = 0;
  nibNum = 2 * elNum * nrrdElementSize(nrrd);
  if (nibNum / elNum != 2 * nrrdElementSize(nrrd)) {
    biffAddf(NRRD, "%s: size_t can't hold 2*(#bytes in array)\n", me);
    return 1;
  }
  while (nibIdx < nibNum) {
    unsigned char nibshift;
    car = fgetc(file);
    if (EOF == car) break;
    nib = _nrrdReadHexTable[car & 127];
    if (-2 == nib) {
      /* not a valid hex character */
      break;
    }
    if (-1 == nib) {
      /* its white space */
      continue;
    }
    /* else it is a valid character, representing a value from 0 to 15 */
    nibshift = AIR_UCHAR(nib << (4 * (1 - (nibIdx & 1))));
    /* HEY not sure why the cast is needed with gcc v4.8 -Wconversion */
    *data = AIR_UCHAR(*data + nibshift);
    data += nibIdx & 1;
    nibIdx++;
  }
  if (nibIdx != nibNum) {
    char stmp[2][AIR_STRLEN_SMALL + 1];
    if (EOF == car) {
      biffAddf(NRRD, "%s: hit EOF getting byte %s of %s", me,
               airSprintSize_t(stmp[0], nibIdx / 2),
               airSprintSize_t(stmp[1], nibNum / 2));
    } else {
      biffAddf(NRRD,
               "%s: hit invalid character ('%c') getting "
               "byte %s of %s",
               me, car, airSprintSize_t(stmp[0], nibIdx / 2),
               airSprintSize_t(stmp[1], nibNum / 2));
    }
    return 1;
  }
  return 0;
}

static int /* Biff: 1 */
_nrrdEncodingHex_write(FILE *file, const void *_data, size_t elNum, const Nrrd *nrrd,
                       NrrdIoState *nio) {
  static const char me[] = "_nrrdEncodingHex_write";
  const unsigned char *data;
  size_t byteIdx, byteNum;
  unsigned int bytesPerLine;

  if (!(file && _data && nrrd && nio)) {
    biffAddf(NRRD, "%s: got NULL pointer (%p,%p,%p,%p)", me, (void *)file,
             (const void *)_data, (const void *)nrrd, (void *)nio);
    return 1;
  }
  bytesPerLine = AIR_MAX(1, nio->charsPerLine / 2);
  data = AIR_CAST(const unsigned char *, _data);
  byteNum = elNum * nrrdElementSize(nrrd);
  for (byteIdx = 0; byteIdx < byteNum; byteIdx++) {
    fprintf(file, "%c%c", _nrrdWriteHexTable[(*data) >> 4],
            _nrrdWriteHexTable[(*data) & 15]);
    if (bytesPerLine - 1 == byteIdx % bytesPerLine) {
      fprintf(file, "\n");
    }
    data++;
  }
  /* just to be sure, we always end with a carraige return */
  fprintf(file, "\n");
  return 0;
}

const NrrdEncoding _nrrdEncodingHex = {"hex",     /* name */
                                       "hex",     /* suffix */
                                       AIR_TRUE,  /* endianMatters */
                                       AIR_FALSE, /* isCompression */
                                       _nrrdEncodingHex_available,
                                       _nrrdEncodingHex_read,
                                       _nrrdEncodingHex_write};

const NrrdEncoding *const nrrdEncodingHex = &_nrrdEncodingHex;

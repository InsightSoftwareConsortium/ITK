/*
  Teem: Tools to process and visualize scientific data and images              
  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  (LGPL) as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  The terms of redistributing and/or modifying this software also
  include exceptions to the LGPL that facilitate static linking.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this library; if not, write to Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "NrrdIO.h"
#include "privateNrrd.h"

const int
_nrrdWriteHexTable[16] = {
  '0', '1', '2', '3', '4', '5', '6', '7',
  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
};

/*
** -2: not allowed, error
** -1: whitespace
** [0,15]: values
*/
const int
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


int
_nrrdEncodingHex_available(void) {

  return AIR_TRUE;
}

int
_nrrdEncodingHex_read(FILE *file, void *_data, size_t elNum,
                      Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdEncodingHex_read";
  size_t nibIdx, nibNum;
  unsigned char *data;
  int car=0, nib;

  AIR_UNUSED(nio);
  data = (unsigned char *)_data;
  nibIdx = 0;
  nibNum = 2*elNum*nrrdElementSize(nrrd);
  if (nibNum/elNum != 2*nrrdElementSize(nrrd)) {
    biffAddf(NRRD, "%s: size_t can't hold 2*(#bytes in array)\n", me);
    return 1;
  }
  while (nibIdx < nibNum) {
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
    *data += AIR_CAST(unsigned int, nib << (4*(1-(nibIdx & 1))));
    data += nibIdx & 1;
    nibIdx++;
  }
  if (nibIdx != nibNum) {
    if (EOF == car) {
      biffAddf(NRRD, "%s: hit EOF getting "
               "byte " _AIR_SIZE_T_CNV " of " _AIR_SIZE_T_CNV,
               me, nibIdx/2, nibNum/2);
    } else {
      biffAddf(NRRD, "%s: hit invalid character ('%c') getting "
               "byte " _AIR_SIZE_T_CNV " of " _AIR_SIZE_T_CNV,
               me, car, nibIdx/2, nibNum/2);
    }
    return 1;
  }
  return 0;
}

int
_nrrdEncodingHex_write(FILE *file, const void *_data, size_t elNum,
                       const Nrrd *nrrd, NrrdIoState *nio) {
  /* static const char me[]="_nrrdEncodingHex_write"; */
  unsigned char *data;
  size_t byteIdx, byteNum;
  unsigned int bytesPerLine;

  bytesPerLine = AIR_MAX(1, nio->charsPerLine/2);
  data = (unsigned char*)_data;
  byteNum = elNum*nrrdElementSize(nrrd);
  for (byteIdx=0; byteIdx<byteNum; byteIdx++) {
    fprintf(file, "%c%c",
            _nrrdWriteHexTable[(*data)>>4],
            _nrrdWriteHexTable[(*data)&15]);
    if (bytesPerLine-1 == byteIdx % bytesPerLine) {
      fprintf(file, "\n");
    }
    data++;
  }
  /* just to be sure, we always end with a carraige return */
  fprintf(file, "\n");
  return 0;
}

const NrrdEncoding
_nrrdEncodingHex = {
  "hex",      /* name */
  "hex",      /* suffix */
  AIR_TRUE,   /* endianMatters */
  AIR_FALSE,   /* isCompression */
  _nrrdEncodingHex_available,
  _nrrdEncodingHex_read,
  _nrrdEncodingHex_write
};

const NrrdEncoding *const
nrrdEncodingHex = &_nrrdEncodingHex;

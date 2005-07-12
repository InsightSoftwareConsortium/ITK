/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2005  Gordon Kindlmann
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
  char me[]="_nrrdEncodingHex_read", err[AIR_STRLEN_MED];
  size_t nibIdx, nibNum;
  unsigned char *data;
  int car=0, nib;

  AIR_UNUSED(nio);
  data = (unsigned char *)_data;
  nibIdx = 0;
  nibNum = 2*elNum*nrrdElementSize(nrrd);
  if (nibNum/elNum != 2*nrrdElementSize(nrrd)) {
    sprintf(err, "%s: size_t can't hold 2*(#bytes in array)\n", me);
    biffAdd(NRRD, err); return 1;
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
    *data += nib << (4*(1-(nibIdx & 1)));
    data += nibIdx & 1;
    nibIdx++;
  }
  if (nibIdx != nibNum) {
    if (EOF == car) {
      sprintf(err, "%s: hit EOF getting "
              "byte " _AIR_SIZE_T_CNV " of " _AIR_SIZE_T_CNV,
              me, nibIdx/2, nibNum/2);
    } else {
      sprintf(err, "%s: hit invalid character ('%c') getting "
              "byte " _AIR_SIZE_T_CNV " of " _AIR_SIZE_T_CNV,
              me, car, nibIdx/2, nibNum/2);
    }
    biffAdd(NRRD, err); return 1;
  }
  return 0;
}

int
_nrrdEncodingHex_write(FILE *file, const void *_data, size_t elNum,
                       const Nrrd *nrrd, NrrdIoState *nio) {
  /* char me[]="_nrrdEncodingHex_write", err[AIR_STRLEN_MED]; */
  unsigned char *data;
  size_t byteIdx, byteNum;

  AIR_UNUSED(nio);
  data = (unsigned char*)_data;
  byteNum = elNum*nrrdElementSize(nrrd);
  for (byteIdx=0; byteIdx<byteNum; byteIdx++) {
    fprintf(file, "%c%c",
            _nrrdWriteHexTable[(*data)>>4],
            _nrrdWriteHexTable[(*data)&15]);
    if (34 == byteIdx%35)
      fprintf(file, "\n");
    data++;
  }
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

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

void
_nrrdSwap16Endian(void *_data, size_t N) {
  short *data, s, fix;
  size_t I;
  
  if (_data) {
    data = (short *)_data;
    for (I=0; I<N; I++) {
      s = data[I];
      fix =  (s & 0x00FF);
      fix = ((s & 0xFF00) >> 0x08) | (fix << 0x08);
      data[I] = fix;
    }
  }
}

void
_nrrdSwap32Endian(void *_data, size_t N) {
  int *data, w, fix;
  size_t I;

  if (_data) {
    data = (int *)_data;
    for (I=0; I<N; I++) {
      w = data[I];
      fix =  (w & 0x000000FF);
      fix = ((w & 0x0000FF00) >> 0x08) | (fix << 0x08);
      fix = ((w & 0x00FF0000) >> 0x10) | (fix << 0x08);
      fix = ((w & 0xFF000000) >> 0x18) | (fix << 0x08);
      data[I] = fix;
    }
  }
}

void
_nrrdSwap64Endian(void *_data, size_t N) {
  airLLong *data, l, fix;
  size_t I;

  if (_data) {
    data = (airLLong *)_data;
    for (I=0; I<N; I++) {
      l = data[I];
      fix =  (l &           0x00000000000000FF);
      fix = ((l &           0x000000000000FF00) >> 0x08) | (fix << 0x08);
      fix = ((l &           0x0000000000FF0000) >> 0x10) | (fix << 0x08);
      fix = ((l &           0x00000000FF000000) >> 0x18) | (fix << 0x08);
      fix = ((l & AIR_LLONG(0x000000FF00000000)) >> 0x20) | (fix << 0x08);
      fix = ((l & AIR_LLONG(0x0000FF0000000000)) >> 0x28) | (fix << 0x08);
      fix = ((l & AIR_LLONG(0x00FF000000000000)) >> 0x30) | (fix << 0x08);
      fix = ((l & AIR_LLONG(0xFF00000000000000)) >> 0x38) | (fix << 0x08);
      data[I] = fix;
    }
  }
}

void
_nrrdNoopEndian(void *data, size_t N) {
  AIR_UNUSED(data);
  AIR_UNUSED(N);
  return;
}

void
_nrrdBlockEndian(void *data, size_t N) {
  char me[]="_nrrdBlockEndian";
  
  AIR_UNUSED(data);
  AIR_UNUSED(N);
  fprintf(stderr, "%s: WARNING: can't fix endiannes of nrrd type %s\n", me,
          airEnumStr(nrrdType, nrrdTypeBlock));
}

void
(*_nrrdSwapEndian[])(void *, size_t) = {
  _nrrdNoopEndian,         /*  0: nobody knows! */
  _nrrdNoopEndian,         /*  1:   signed 1-byte integer */
  _nrrdNoopEndian,         /*  2: unsigned 1-byte integer */
  _nrrdSwap16Endian,       /*  3:   signed 2-byte integer */
  _nrrdSwap16Endian,       /*  4: unsigned 2-byte integer */
  _nrrdSwap32Endian,       /*  5:   signed 4-byte integer */
  _nrrdSwap32Endian,       /*  6: unsigned 4-byte integer */
  _nrrdSwap64Endian,       /*  7:   signed 8-byte integer */
  _nrrdSwap64Endian,       /*  8: unsigned 8-byte integer */
  _nrrdSwap32Endian,       /*  9:          4-byte floating point */
  _nrrdSwap64Endian,       /* 10:          8-byte floating point */
  _nrrdBlockEndian         /* 11: size user defined at run time */
};

void
nrrdSwapEndian(Nrrd *nrrd) {
  
  if (nrrd 
      && nrrd->data 
      && !airEnumValCheck(nrrdType, nrrd->type)) {
    _nrrdSwapEndian[nrrd->type](nrrd->data, nrrdElementNumber(nrrd));
  }
  return;
}

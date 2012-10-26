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

static void
_nrrdSwap16Endian(void *_data, size_t N) {
  unsigned short *data, dd, fix, mask;
  size_t I;

  if (!_data) {
    return;
  }
  data = AIR_CAST(unsigned short *, _data);
  mask = AIR_CAST(unsigned short, 0x00FFu);
  for (I=0; I<N; I++) {
    dd = data[I];
    fix = (dd & mask); dd >>= 0x08;
    fix = (dd & mask) | AIR_CAST(unsigned short, fix << 0x08);
    data[I] = fix;
  }
}

static void
_nrrdSwap32Endian(void *_data, size_t N) {
  unsigned int *data, dd, fix, mask;
  size_t I;

  if (!_data) {
    return;
  }
  data = AIR_CAST(unsigned int *, _data);
  mask = 0x000000FFu;
  for (I=0; I<N; I++) {
    dd = data[I];
    fix = (dd & mask);                 dd >>= 0x08;
    fix = (dd & mask) | (fix << 0x08); dd >>= 0x08;
    fix = (dd & mask) | (fix << 0x08); dd >>= 0x08;
    fix = (dd & mask) | (fix << 0x08);
    data[I] = fix;
  }
}

static void
_nrrdSwap64Endian(void *_data, size_t N) {
  airULLong *data, dd, fix, mask;
  size_t I;

  if (!_data) {
    return;
  }
  data = AIR_CAST(airULLong *, _data);
  mask = AIR_ULLONG(0x00000000000000FF);
  for (I=0; I<N; I++) {
    dd = data[I];
    fix = (dd & mask);                 dd >>= 0x08;
    fix = (dd & mask) | (fix << 0x08); dd >>= 0x08;
    fix = (dd & mask) | (fix << 0x08); dd >>= 0x08;
    fix = (dd & mask) | (fix << 0x08); dd >>= 0x08;
    fix = (dd & mask) | (fix << 0x08); dd >>= 0x08;
    fix = (dd & mask) | (fix << 0x08); dd >>= 0x08;
    fix = (dd & mask) | (fix << 0x08); dd >>= 0x08;
    fix = (dd & mask) | (fix << 0x08);
    data[I] = fix;
  }
}

static void
_nrrdNoopEndian(void *data, size_t N) {
  AIR_UNUSED(data);
  AIR_UNUSED(N);
  return;
}

static void
_nrrdBlockEndian(void *data, size_t N) {
  char me[]="_nrrdBlockEndian";

  AIR_UNUSED(data);
  AIR_UNUSED(N);
  fprintf(stderr, "%s: WARNING: can't fix endiannes of nrrd type %s\n", me,
          airEnumStr(nrrdType, nrrdTypeBlock));
}

static void
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

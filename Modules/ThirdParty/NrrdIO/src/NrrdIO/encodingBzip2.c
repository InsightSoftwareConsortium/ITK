/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2013, 2012, 2011, 2010, 2009  University of Chicago
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
#include "privateNrrd.h"

int
_nrrdEncodingBzip2_available(void) {

  return AIR_FALSE;
}

int
_nrrdEncodingBzip2_read(FILE *file, void *data, size_t elementNum,
                        Nrrd *nrrd, struct NrrdIoState_t *nio) {
  char me[]="_nrrdEncodingBzip2_read", err[AIR_STRLEN_MED];

  AIR_UNUSED(file);
  AIR_UNUSED(data);
  AIR_UNUSED(elementNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  sprintf(err, "%s: Sorry, %s encoding not available in NrrdIO",
          me, nrrdEncodingBzip2->name);
  biffAdd(NRRD, err); return 1;
}

int
_nrrdEncodingBzip2_write(FILE *file, const void *data, size_t elementNum,
                         const Nrrd *nrrd, struct NrrdIoState_t *nio) {
  char me[]="_nrrdEncodingBzip2_write", err[AIR_STRLEN_MED];

  AIR_UNUSED(file);
  AIR_UNUSED(data);
  AIR_UNUSED(elementNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  sprintf(err, "%s: Sorry, %s encoding not available in NrrdIO",
          me, nrrdEncodingBzip2->name);
  biffAdd(NRRD, err); return 1;
}

const NrrdEncoding
_nrrdEncodingBzip2 = {
  "bzip2",     /* name */
  "raw.bz2",   /* suffix */
  AIR_TRUE,    /* endianMatters */
  AIR_TRUE,    /* isCompression */
  _nrrdEncodingBzip2_available,
  _nrrdEncodingBzip2_read,
  _nrrdEncodingBzip2_write
};

const NrrdEncoding *const
nrrdEncodingBzip2 = &_nrrdEncodingBzip2;

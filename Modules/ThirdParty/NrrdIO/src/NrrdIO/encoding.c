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

/*
** what a NrrdEncoding can assume:
** -- the given nrrd struct has been filled out for the sake of knowing
**    nrrd->dim, nrrd->axis[0].size, nrrd->type, and nrrd->blockSize
**    AND NOTHING ELSE.  See nrrd.h for why those fields, of all things
**    are needed for {en/de}coding
**
** what a NrrdEncoding has to do:
** -- read data from file into the "data" argument (BUT NOT nrrd->data!!),
**     or vice versa.
** -- respect nrrdStateVerboseIO with messages to stderr, if possible
** -- in case of error, put text error messages into biff via
**    biffAddf(NRRD, <error char*> ...)
**
** The "unknown" encoding below is intended to serve as a template for
** any new encodings being developed.
*/

static int
_nrrdEncodingUnknown_available(void) {

  /* insert code here */

  return AIR_FALSE;
}

static int
_nrrdEncodingUnknown_read(FILE *file, void *data,
                          size_t elementNum, Nrrd *nrrd,
                          struct NrrdIoState_t *nio) {
  static const char me[]="_nrrdEncodingUnknown_read";

  /* insert code here, and remove error handling below */
  AIR_UNUSED(file);
  AIR_UNUSED(data);
  AIR_UNUSED(elementNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);

  biffAddf(NRRD, "%s: ERROR!!! trying to read unknown encoding", me);
  return 1;
}

static int
_nrrdEncodingUnknown_write(FILE *file, const void *data,
                           size_t elementNum, const Nrrd *nrrd,
                           struct NrrdIoState_t *nio) {
  static const char me[]="_nrrdEncodingUnknown_write";

  /* insert code here, and remove error handling below */
  AIR_UNUSED(file);
  AIR_UNUSED(data);
  AIR_UNUSED(elementNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);

  biffAddf(NRRD, "%s: ERROR!!! trying to write unknown encoding", me);
  return 1;
}

const NrrdEncoding
_nrrdEncodingUnknown = {
  "unknown",  /* name */
  "unknown",  /* suffix */
  AIR_FALSE,  /* endianMatters */
  AIR_FALSE,  /* isCompression */
  _nrrdEncodingUnknown_available,
  _nrrdEncodingUnknown_read,
  _nrrdEncodingUnknown_write
};

const NrrdEncoding *const
nrrdEncodingUnknown = &_nrrdEncodingUnknown;

const NrrdEncoding *const
nrrdEncodingArray[NRRD_ENCODING_TYPE_MAX+1] = {
  &_nrrdEncodingUnknown,
  &_nrrdEncodingRaw,
  &_nrrdEncodingAscii,
  &_nrrdEncodingHex,
  &_nrrdEncodingGzip,
  &_nrrdEncodingBzip2
};


/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998 University of Utah
 
  These source files have been copied and/or modified from teem,
  Gordon Kindlmann's research software; <http://teem.sourceforge.net>.
  Teem is licensed under the GNU Lesser Public License. The
  non-copyleft licensing defined here applies to only the source files
  in the NrrdIO distribution (not the rest of teem), and only to the
  files originating with NrrdIO (not analogous files in teem).
 
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
** -- the given nrrd struct has been filled out so that functions
**    like nrrdElementNumber and nrrdElementSize will work correctly.
**
** what a NrrdEncoding has to do:
** -- allocate nrrd->data for the amount of space required, or use existing
**    memory described by nio->oldData and nio->oldDataSize.  This is most
**    easily done via _nrrdCalloc(). Allocation has to be done here because
**    of the restrictions imposed by DirectIO (used by nrrdEncodingRaw).
** -- do nothing on read/write if nio->skipData
** -- read data from nio->dataFile into nrrd->data, or vice versa.
** -- respect nrrdStateVerboseIO with messages to stderr, if possible
** -- in case of error, put text error messages into biff via
**    biffAdd(NRRD, <error char*>)
**
** The "unknown" encoding below is intended to serve as a template for 
** any new encodings being developed.
*/

int
_nrrdEncodingUnknown_available(void) {

  /* insert code here */

  return AIR_FALSE;
}

int
_nrrdEncodingUnknown_read(Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdEncodingUnknown_read", err[AIR_STRLEN_MED];

  if (nio->skipData) {
    return 0;
  }

  /* insert code here, and remove error handling below */

  sprintf(err, "%s: ERROR!!! trying to read unknown encoding", me);
  biffAdd(NRRD, err);
  return 1;
}

int
_nrrdEncodingUnknown_write(const Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdEncodingUnknown_write", err[AIR_STRLEN_MED];

  if (nio->skipData) {
    return 0;
  }

  /* insert code here, and remove error handling below */

  sprintf(err, "%s: ERROR!!! trying to write unknown encoding", me);
  biffAdd(NRRD, err);
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


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
** what a NrrdFormat can assume:
** -- that nio->format has been set to you already
** -- for read(): that nio->path has been set to the path of the file being
**    read in, if the information was ever available
** -- for contentStartsLike() and read(): that nio->line contains the
**    first line of of the file, in order to determine the file type
**
** what a NrrdFormat has to do:
** -- respect nio->skipData to whatever extent makes sense on top of how the
**    NrrdEncoding respects it (by making read and write no-ops).
**    nrrdFormatNRRD, for instance, won't create empty detached data files
**    if nio->skipData.
** -- determine what NrrdEncoding to use, if there's a choice
** -- respect nrrdStateVerboseIO with messages to stderr, if possible
**
** The "unknown" format is intended as a template for writing new formats.
*/

static int
_nrrdFormatUnknown_available(void) {

  /* insert code here */

  return AIR_FALSE;
}

static int
_nrrdFormatUnknown_nameLooksLike(const char *filename) {

  /* insert code here */
  AIR_UNUSED(filename);

  return AIR_FALSE;
}

static int
_nrrdFormatUnknown_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding,
                            int useBiff) {
  static const char me[]="_nrrdFormatUnknown_fitsInto";

  if (!(nrrd && encoding)) {
    biffMaybeAddf(useBiff, NRRD, "%s: got NULL nrrd (%p) or encoding (%p)",
                  me, AIR_CVOIDP(nrrd), AIR_CVOIDP(encoding));
    return AIR_FALSE;
  }

  /* insert code here */

  return AIR_FALSE;
}

static int
_nrrdFormatUnknown_contentStartsLike(NrrdIoState *nio) {

  /* insert code here */
  AIR_UNUSED(nio);

  return AIR_FALSE;
}

static int
_nrrdFormatUnknown_read(FILE *file, Nrrd *nrrd,
                        NrrdIoState *nio) {
  static const char me[]="_nrrdFormatUnknown_read";

  /* insert code here, and remove error handling below */
  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);

  biffAddf(NRRD, "%s: ERROR!!! trying to read unknown format", me);
  return 1;
}

static int
_nrrdFormatUnknown_write(FILE *file, const Nrrd *nrrd,
                         NrrdIoState *nio) {
  static const char me[]="_nrrdFormatUnknown_write";

  /* insert code here, and remove error handling below */
  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);

  biffAddf(NRRD, "%s: ERROR!!! trying to write unknown format", me);
  return 1;
}

static const NrrdFormat
_nrrdFormatUnknown = {
  "unknown",
  AIR_FALSE,  /* isImage */
  AIR_TRUE,   /* readable */
  AIR_FALSE,  /* usesDIO */
  _nrrdFormatUnknown_available,
  _nrrdFormatUnknown_nameLooksLike,
  _nrrdFormatUnknown_fitsInto,
  _nrrdFormatUnknown_contentStartsLike,
  _nrrdFormatUnknown_read,
  _nrrdFormatUnknown_write
};

const NrrdFormat *const
nrrdFormatUnknown = &_nrrdFormatUnknown;

const NrrdFormat *const
nrrdFormatArray[NRRD_FORMAT_TYPE_MAX+1] = {
  &_nrrdFormatUnknown,
  &_nrrdFormatNRRD,
  &_nrrdFormatPNM,
  &_nrrdFormatPNG,
  &_nrrdFormatVTK,
  &_nrrdFormatText,
  &_nrrdFormatEPS
};

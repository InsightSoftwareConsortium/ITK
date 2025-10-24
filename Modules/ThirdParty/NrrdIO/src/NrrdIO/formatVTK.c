/*
  NrrdIO: stand-alone code for basic nrrd functionality
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

static int
_nrrdFormatVTK_available(void) {

  return AIR_FALSE;
}

static int
_nrrdFormatVTK_nameLooksLike(const char *fname) {

  return airEndsWith(fname, NRRD_EXT_VTK);
}

static int /* Biff: maybe:3:AIR_FALSE */
_nrrdFormatVTK_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding, int useBiff) {
  static const char me[] = "_nrrdFormatVTK_fitsInto";
  char err[AIR_STRLEN_MED];

  AIR_UNUSED(nrrd);
  AIR_UNUSED(encoding);
  AIR_UNUSED(useBiff);
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO", me, nrrdFormatVTK->name);
  biffMaybeAdd(NRRD, err, useBiff);
  return AIR_FALSE;
}

static int
_nrrdFormatVTK_contentStartsLike(NrrdIoState *nio) {

  AIR_UNUSED(nio);
  return AIR_FALSE;
}

static int /* Biff: 1 */
_nrrdFormatVTK_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[] = "_nrrdFormatVTK_read";
  char err[AIR_STRLEN_MED];

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO", me, nrrdFormatVTK->name);
  biffAdd(NRRD, err);
  return 1;
}

/* this strongly assumes that nrrdFitsInFormat() was true */
static int /* Biff: 1 */
_nrrdFormatVTK_write(FILE *file, const Nrrd *_nrrd, NrrdIoState *nio) {
  static const char me[] = "_nrrdFormatVTK_write";
  char err[AIR_STRLEN_MED];

  AIR_UNUSED(file);
  AIR_UNUSED(_nrrd);
  AIR_UNUSED(nio);
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO", me, nrrdFormatVTK->name);
  biffAdd(NRRD, err);
  return 1;
}

const NrrdFormat _nrrdFormatVTK = {"VTK",
                                   AIR_FALSE, /* isImage */
                                   AIR_FALSE, /* readable */
                                   _nrrdFormatVTK_available,
                                   _nrrdFormatVTK_nameLooksLike,
                                   _nrrdFormatVTK_fitsInto,
                                   _nrrdFormatVTK_contentStartsLike,
                                   _nrrdFormatVTK_read,
                                   _nrrdFormatVTK_write};

const NrrdFormat *const nrrdFormatVTK = &_nrrdFormatVTK;

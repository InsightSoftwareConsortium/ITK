/*
  Teem: Tools to process and visualize scientific data and images
  Copyright (C) 2009--2023  University of Chicago
  Copyright (C) 2005--2008  Gordon Kindlmann
  Copyright (C) 1998--2004  University of Utah

  This library is free software; you can redistribute it and/or modify it under the terms
  of the GNU Lesser General Public License (LGPL) as published by the Free Software
  Foundation; either version 2.1 of the License, or (at your option) any later version.
  The terms of redistributing and/or modifying this software also include exceptions to
  the LGPL that facilitate static linking.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
  You should have received a copy of the GNU Lesser General Public License
  along with this library; if not, see <https://www.gnu.org/licenses/>.
*/

#include "NrrdIO.h"
#include "privateNrrd.h"

static int
_nrrdFormatText_available(void) {
  return AIR_FALSE;
}

static int
_nrrdFormatText_nameLooksLike(const char *fname) {

  return (airEndsWith(fname, NRRD_EXT_TEXT) || airEndsWith(fname, ".text")
          || airEndsWith(fname, ".ascii"));
}

static int /* Biff: maybe:3:AIR_FALSE */
_nrrdFormatText_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding, int useBiff) {
  static const char me[] = "_nrrdFormatText_fitsInto";

  AIR_UNUSED(nrrd);
  AIR_UNUSED(encoding);
  AIR_UNUSED(useBiff);
  char err[AIR_STRLEN_MED];
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO", me, nrrdFormatText->name);
  biffMaybeAdd(NRRD, err, useBiff);
  return AIR_FALSE;
}

static int
_nrrdFormatText_contentStartsLike(NrrdIoState *nio) {

  AIR_UNUSED(nio);
  return AIR_FALSE;
}

static int /* Biff: 1 */
_nrrdFormatText_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[] = "_nrrdReadText";

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  char err[AIR_STRLEN_MED];
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO", me, nrrdFormatText->name);
  biffAdd(NRRD, err);
  return 1;
}

static int
_nrrdFormatText_write(FILE *file, const Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[] = "_nrrdFormatText_write";

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  char err[AIR_STRLEN_MED];
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO", me, nrrdFormatText->name);
  biffAdd(NRRD, err);
  return 1;
}

const NrrdFormat _nrrdFormatText = {"text",
                                    AIR_FALSE, /* isImage */
                                    AIR_FALSE, /* readable */
                                    _nrrdFormatText_available,
                                    _nrrdFormatText_nameLooksLike,
                                    _nrrdFormatText_fitsInto,
                                    _nrrdFormatText_contentStartsLike,
                                    _nrrdFormatText_read,
                                    _nrrdFormatText_write};

const NrrdFormat *const nrrdFormatText = &_nrrdFormatText;

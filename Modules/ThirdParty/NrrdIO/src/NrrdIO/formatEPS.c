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
_nrrdFormatEPS_available(void) {

  return AIR_FALSE;
}

static int
_nrrdFormatEPS_nameLooksLike(const char *filename) {

  return airEndsWith(filename, NRRD_EXT_EPS);
}

static int /* Biff: maybe:3:AIR_FALSE */
_nrrdFormatEPS_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding, int useBiff) {
  static const char me[] = "_nrrdFormatEPS_fitsInto";

  AIR_UNUSED(nrrd);
  AIR_UNUSED(encoding);
  AIR_UNUSED(useBiff);
  char err[AIR_STRLEN_MED];
  snprintf(err, AIR_STRLEN_MED, "%s: Sorry, %s format not available in NrrdIO", me,
           nrrdFormatEPS->name);
  biffMaybeAdd(NRRD, err, useBiff);
  return AIR_FALSE;
}

static int
_nrrdFormatEPS_contentStartsLike(NrrdIoState *nio) {

  AIR_UNUSED(nio);
  /* this is a write-only format */
  return AIR_FALSE;
}

static int /* Biff: 1 */
_nrrdFormatEPS_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[] = "_nrrdReadEPS";

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  char err[AIR_STRLEN_MED];
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO", me, nrrdFormatEPS->name);
  biffAdd(NRRD, err);
  return 1;
}

static int /* Biff: 1 */
_nrrdFormatEPS_write(FILE *file, const Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[] = "_nrrdFormatEPS_write";

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  char err[AIR_STRLEN_MED];
  snprintf(err, AIR_STRLEN_MED, "%s: Sorry, %s format not available in NrrdIO", me,
           nrrdFormatEPS->name);
  biffAdd(NRRD, err);
  return 1;
}

const NrrdFormat _nrrdFormatEPS = {"EPS",
                                   AIR_FALSE, /* isImage */
                                   AIR_FALSE, /* readable */
                                   _nrrdFormatEPS_available,
                                   _nrrdFormatEPS_nameLooksLike,
                                   _nrrdFormatEPS_fitsInto,
                                   _nrrdFormatEPS_contentStartsLike,
                                   _nrrdFormatEPS_read,
                                   _nrrdFormatEPS_write};

const NrrdFormat *const nrrdFormatEPS = &_nrrdFormatEPS;

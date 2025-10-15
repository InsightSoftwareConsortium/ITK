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
_nrrdEncodingBzip2_available(void) {

  return AIR_FALSE;
}

static int /* Biff: 1 */
_nrrdEncodingBzip2_read(FILE *file, void *_data, size_t elNum, Nrrd *nrrd,
                        NrrdIoState *nio) {
  static const char me[] = "_nrrdEncodingBzip2_read";

  AIR_UNUSED(file);
  AIR_UNUSED(_data);
  AIR_UNUSED(elNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  char err[AIR_STRLEN_MED];
  snprintf(err, AIR_STRLEN_MED, "%s: Sorry, %s encoding not available in NrrdIO", me,
           nrrdEncodingBzip2->name);
  biffAdd(NRRD, err);
  return 1;
}

static int /* Biff: 1 */
_nrrdEncodingBzip2_write(FILE *file, const void *data, size_t elementNum,
                         const Nrrd *nrrd, struct NrrdIoState_t *nio) {
  static const char me[] = "_nrrdEncodingBzip2_write";

  AIR_UNUSED(file);
  AIR_UNUSED(data);
  AIR_UNUSED(elementNum);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  char err[AIR_STRLEN_MED];
  sprintf(err, "%s: Sorry, %s encoding not available in NrrdIO", me,
          nrrdEncodingBzip2->name);
  biffAdd(NRRD, err);
  return 1;
}

const NrrdEncoding _nrrdEncodingBzip2 = {"bzip2",   /* name */
                                         "raw.bz2", /* suffix */
                                         AIR_TRUE,  /* endianMatters */
                                         AIR_TRUE,  /* isCompression */
                                         _nrrdEncodingBzip2_available,
                                         _nrrdEncodingBzip2_read,
                                         _nrrdEncodingBzip2_write};

const NrrdEncoding *const nrrdEncodingBzip2 = &_nrrdEncodingBzip2;

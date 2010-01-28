/*
  Teem: Tools to process and visualize scientific data and images              
  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  (LGPL) as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  The terms of redistributing and/or modifying this software also
  include exceptions to the LGPL that facilitate static linking.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this library; if not, write to Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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

int
_nrrdFormatUnknown_available(void) {

  /* insert code here */

  return AIR_FALSE;
}

int
_nrrdFormatUnknown_nameLooksLike(const char *filename) {
  
  /* insert code here */
  AIR_UNUSED(filename);

  return AIR_FALSE;
}

int
_nrrdFormatUnknown_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding,
                            int useBiff) {
  static const char me[]="_nrrdFormatUnknown_fitsInto";
  
  if (!(nrrd && encoding)) {
    biffMaybeAddf(useBiff, NRRD, "%s: got NULL nrrd (%p) or encoding (%p)",
                  me, AIR_CAST(void*, nrrd), AIR_CAST(void*, encoding)); 
    return AIR_FALSE;
  }

  /* insert code here */

  return AIR_FALSE;
}

int
_nrrdFormatUnknown_contentStartsLike(NrrdIoState *nio) {
  
  /* insert code here */
  AIR_UNUSED(nio);

  return AIR_FALSE;
}

int
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

int
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

const NrrdFormat
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

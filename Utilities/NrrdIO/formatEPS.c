/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998 University of Utah
 
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
_nrrdFormatEPS_available(void) {
  
  return AIR_FALSE;
}

int
_nrrdFormatEPS_nameLooksLike(const char *filename) {
  
  return airEndsWith(filename, NRRD_EXT_EPS);
}

int
_nrrdFormatEPS_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding,
                        int useBiff) {
  char me[]="_nrrdFormatEPS_fitsInto", err[AIR_STRLEN_MED];
  
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO",
          me, nrrdFormatEPS->name);
  biffMaybeAdd(NRRD, err, useBiff); 
  return AIR_FALSE;

}

int
_nrrdFormatEPS_contentStartsLike(NrrdIoState *nio) {

  return AIR_FALSE;

}

int
_nrrdFormatEPS_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdReadEPS", err[AIR_STRLEN_MED];

  sprintf(err, "%s: Sorry, %s format not available in NrrdIO",
          me, nrrdFormatEPS->name);
  biffAdd(NRRD, err);
  return 1;
}

/* this strongly assumes that nrrdFitsInFormat() was true */
int
_nrrdFormatEPS_write(FILE *file, const Nrrd *_nrrd, NrrdIoState *nio) {
  char me[]="_nrrdFormatEPS_write", err[AIR_STRLEN_MED];

  sprintf(err, "%s: Sorry, %s format not available in NrrdIO",
          me, nrrdFormatEPS->name);
  biffAdd(NRRD, err);
  return 1;
}

const NrrdFormat
_nrrdFormatEPS = {
  "EPS",
  AIR_FALSE,  /* isImage */
  AIR_FALSE,  /* readable */
  AIR_FALSE,  /* usesDIO */
  _nrrdFormatEPS_available,
  _nrrdFormatEPS_nameLooksLike,
  _nrrdFormatEPS_fitsInto,
  _nrrdFormatEPS_contentStartsLike,
  _nrrdFormatEPS_read,
  _nrrdFormatEPS_write
};

const NrrdFormat *const
nrrdFormatEPS = &_nrrdFormatEPS;

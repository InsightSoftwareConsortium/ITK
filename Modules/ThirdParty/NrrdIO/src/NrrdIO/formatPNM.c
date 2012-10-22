/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2012, 2011, 2010, 2009  University of Chicago
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
_nrrdFormatPNM_available(void) {
  
  return AIR_FALSE;
}

int
_nrrdFormatPNM_nameLooksLike(const char *filename) {
  
  return (airEndsWith(filename, NRRD_EXT_PGM)
          || airEndsWith(filename, NRRD_EXT_PPM));
}

int
_nrrdFormatPNM_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding,
                        int useBiff) {
  char me[]="_nrrdFormatPNM_fitsInto", err[AIR_STRLEN_MED];
  
  AIR_UNUSED(nrrd);
  AIR_UNUSED(encoding);
  AIR_UNUSED(useBiff);
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO",
          me, nrrdFormatPNM->name);
  biffMaybeAdd(NRRD, err, useBiff); 
  return AIR_FALSE;

}

int
_nrrdFormatPNM_contentStartsLike(NrrdIoState *nio) {

  AIR_UNUSED(nio);
  return AIR_FALSE;

}

int
_nrrdFormatPNM_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdReadPNM", err[AIR_STRLEN_MED];

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO",
          me, nrrdFormatPNM->name);
  biffAdd(NRRD, err);
  return 1;
}

int
_nrrdFormatPNM_write(FILE *file, const Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdFormatPNM_write", err[AIR_STRLEN_MED];

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  sprintf(err, "%s: Sorry, %s format not available in NrrdIO",
          me, nrrdFormatPNM->name);
  biffAdd(NRRD, err);
  return 1;
}

const NrrdFormat
_nrrdFormatPNM = {
  "PNM",
  AIR_FALSE,  /* isImage */
  AIR_FALSE,  /* readable */
  AIR_FALSE,  /* usesDIO */
  _nrrdFormatPNM_available,
  _nrrdFormatPNM_nameLooksLike,
  _nrrdFormatPNM_fitsInto,
  _nrrdFormatPNM_contentStartsLike,
  _nrrdFormatPNM_read,
  _nrrdFormatPNM_write
};

const NrrdFormat *const
nrrdFormatPNM = &_nrrdFormatPNM;

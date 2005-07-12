/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2005  Gordon Kindlmann
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
_nrrdEncodingAscii_available(void) {

  return AIR_TRUE;
}

int
_nrrdEncodingAscii_read(FILE *file, void *_data, size_t elNum,
                        Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdEncodingAscii_read", err[AIR_STRLEN_MED],
    numbStr[AIR_STRLEN_HUGE];  /* HEY: fix this */
  size_t I;
  char *data;
  int tmp;

  AIR_UNUSED(nio);
  if (nrrdTypeBlock == nrrd->type) {
    sprintf(err, "%s: can't read nrrd type %s from %s", me,
            airEnumStr(nrrdType, nrrdTypeBlock),
            nrrdEncodingAscii->name);
    biffAdd(NRRD, err); return 1;
  }
  data = (char*)_data;
  for (I=0; I<elNum; I++) {
    if (1 != fscanf(file, "%s", numbStr)) {
      sprintf(err, "%s: couldn't parse element " _AIR_SIZE_T_CNV
              " of " _AIR_SIZE_T_CNV, me, I+1, elNum);
      biffAdd(NRRD, err); return 1;
    }
    if (nrrd->type >= nrrdTypeInt) {
      /* sscanf supports putting value directly into this type */
      if (1 != airSingleSscanf(numbStr, nrrdTypePrintfStr[nrrd->type], 
                               (void*)(data + I*nrrdElementSize(nrrd)))) {
        sprintf(err, "%s: couln't parse %s " _AIR_SIZE_T_CNV
                " of " _AIR_SIZE_T_CNV " (\"%s\")", me,
                airEnumStr(nrrdType, nrrd->type),
                I+1, elNum, numbStr);
        biffAdd(NRRD, err); return 1;
      }
    } else {
      /* sscanf value into an int first */
      if (1 != airSingleSscanf(numbStr, "%d", &tmp)) {
        sprintf(err, "%s: couln't parse element " _AIR_SIZE_T_CNV
                " of " _AIR_SIZE_T_CNV " (\"%s\")",
                me, I+1, elNum, numbStr);
        biffAdd(NRRD, err); return 1;
      }
      nrrdIInsert[nrrd->type](data, I, tmp);
    }
  }
  
  return 0;
}

int
_nrrdEncodingAscii_write(FILE *file, const void *_data, size_t elNum,
                         const Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdEncodingAscii_write", err[AIR_STRLEN_MED],
    buff[AIR_STRLEN_MED];
  unsigned int bufflen, linelen;
  const char *data;
  size_t I;
  
  if (nrrdTypeBlock == nrrd->type) {
    sprintf(err, "%s: can't write nrrd type %s to %s", me,
            airEnumStr(nrrdType, nrrdTypeBlock),
            nrrdEncodingAscii->name);
    biffAdd(NRRD, err); return 1;
  }
  data = (char*)_data;
  linelen = 0;
  for (I=0; I<elNum; I++) {
    nrrdSprint[nrrd->type](buff, data);
    if (1 == nrrd->dim) {
      fprintf(file, "%s\n", buff);
    } else if (nrrd->dim == 2 
               && nrrd->axis[0].size <= nio->valsPerLine) {
      fprintf(file, "%s%c", buff,
              (I+1)%(nrrd->axis[0].size) ? ' ' : '\n');
    } else {
      bufflen = strlen(buff);
      if (linelen+bufflen+1 <= nio->charsPerLine) {
        fprintf(file, "%s%s", I ? " " : "", buff);
        linelen += (I ? 1 : 0) + bufflen;
      } else {
        fprintf(file, "\n%s", buff);
        linelen = bufflen;
      }
    }
    data += nrrdElementSize(nrrd);
  }
  /* just to be sure, we always end with a carraige return */
  fprintf(file, "\n");
  
  return 0;
}

const NrrdEncoding
_nrrdEncodingAscii = {
  "ASCII",      /* name */
  "ascii",      /* suffix */
  AIR_FALSE,   /* endianMatters */
  AIR_FALSE,   /* isCompression */
  _nrrdEncodingAscii_available,
  _nrrdEncodingAscii_read,
  _nrrdEncodingAscii_write
};

const NrrdEncoding *const
nrrdEncodingAscii = &_nrrdEncodingAscii;

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
_nrrdEncodingAscii_available(void) {

  return AIR_TRUE;
}

int
_nrrdEncodingAscii_read(Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdEncodingAscii_read", err[AIR_STRLEN_MED],
    numbStr[AIR_STRLEN_HUGE];  /* HEY: fix this */
  size_t I, num;
  char *data;
  int size, tmp;
  
  if (nio->skipData) {
    return 0;
  }
  if (nrrdTypeBlock == nrrd->type) {
    sprintf(err, "%s: can't read nrrd type %s from ascii", me,
            airEnumStr(nrrdType, nrrdTypeBlock));
    biffAdd(NRRD, err); return 1;
  }
  num = nrrdElementNumber(nrrd);
  if (_nrrdCalloc(nrrd, nio)) {
    sprintf(err, "%s: couldn't allocate sufficient memory for all data", me);
    biffAdd(NRRD, err); return 1;
  }
  data = nrrd->data;
  size = nrrdElementSize(nrrd);
  for (I=0; I<num; I++) {
    if (1 != fscanf(nio->dataFile, "%s", numbStr)) {
      sprintf(err, "%s: couldn't parse element " _AIR_SIZE_T_FMT
              " of " _AIR_SIZE_T_FMT, me, I+1, num);
      biffAdd(NRRD, err); return 1;
    }
    if (nrrd->type >= nrrdTypeInt) {
      /* sscanf supports putting value directly into this type */
      if (1 != airSingleSscanf(numbStr, nrrdTypePrintfStr[nrrd->type], 
                               (void*)(data + I*size))) {
        sprintf(err, "%s: couln't parse %s " _AIR_SIZE_T_FMT
                " of " _AIR_SIZE_T_FMT " (\"%s\")", me,
                airEnumStr(nrrdType, nrrd->type),
                I+1, num, numbStr);
        biffAdd(NRRD, err); return 1;
      }
    } else {
      /* sscanf value into an int first */
      if (1 != airSingleSscanf(numbStr, "%d", &tmp)) {
        sprintf(err, "%s: couln't parse element " _AIR_SIZE_T_FMT
                " of " _AIR_SIZE_T_FMT " (\"%s\")",
                me, I+1, num, numbStr);
        biffAdd(NRRD, err); return 1;
      }
      nrrdIInsert[nrrd->type](data, I, tmp);
    }
  }
  
  return 0;
}

int
_nrrdEncodingAscii_write(const Nrrd *nrrd, NrrdIoState *nio) {
  char me[]="_nrrdEncodingAscii_write", err[AIR_STRLEN_MED],
    buff[AIR_STRLEN_MED];
  int size, bufflen, linelen;
  char *data;
  size_t I, num;
  
  if (nio->skipData) {
    return 0;
  }
  if (nrrdTypeBlock == nrrd->type) {
    sprintf(err, "%s: can't write nrrd type %s to ascii", me,
            airEnumStr(nrrdType, nrrdTypeBlock));
    biffAdd(NRRD, err); return 1;
  }
  /* this shouldn't actually be necessary ... */
  if (!nrrdElementSize(nrrd)) {
    sprintf(err, "%s: nrrd reports zero element size!", me);
    biffAdd(NRRD, err); return 1;
  }
  data = nrrd->data;
  size = nrrdElementSize(nrrd);
  num = nrrdElementNumber(nrrd);
  linelen = 0;
  for (I=0; I<num; I++) {
    nrrdSprint[nrrd->type](buff, data);
    if (1 == nrrd->dim) {
      fprintf(nio->dataFile, "%s\n", buff);
    } else if (nrrd->dim == 2 
               && nrrd->axis[0].size <= nio->valsPerLine) {
      fprintf(nio->dataFile, "%s%c", buff,
              (I+1)%(nrrd->axis[0].size) ? ' ' : '\n');
    } else {
      bufflen = strlen(buff);
      if (linelen+bufflen+1 <= nio->charsPerLine) {
        fprintf(nio->dataFile, "%s%s", I ? " " : "", buff);
        linelen += (I ? 1 : 0) + bufflen;
      } else {
        fprintf(nio->dataFile, "\n%s", buff);
        linelen = bufflen;
      }
    }
    data += size;
  }
  /* just to be sure, we always end with a carraige return */
  fprintf(nio->dataFile, "\n");
  
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

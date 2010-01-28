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

/***
**** NONE of the nrrdKeyValue functions use biff.
**** They don't use them now, and they never should.
**** Unless I change my mind.
***/

/*
******** nrrdKeyValueSize
**
** returns the number of key/value pairs in a nrrd
*/
unsigned int
nrrdKeyValueSize(const Nrrd *nrrd) {
  
  if (!nrrd) {
    return 0;
  }
  return nrrd->kvpArr->len;
}

/*
******** nrrdKeyValueIndex
**
** given an int in [0 .. #key/value pairs - 1], sets *keyP and *valueP
** to put to the corresponding key and value.
**
** NOTE: whether or not *keyP and *valueP are set to pointers to memory
** "inside" the nrrd struct (pointers which you had better not free()!)
** is controlled by nrrdStateKeyValueReturnInternalPointers, which defaults
** to AIR_FALSE
*/
void
nrrdKeyValueIndex(const Nrrd *nrrd, char **keyP, char **valueP,
                  unsigned int ki) {
  
  if (!( nrrd && keyP && valueP && ki < nrrd->kvpArr->len )) {
    if (keyP) {
      *keyP = NULL;
    }
    if (valueP) {
      *valueP = NULL;
    }
    return;
  }
  if (nrrdStateKeyValueReturnInternalPointers) {
    *keyP = nrrd->kvp[0 + 2*ki];
    *valueP = nrrd->kvp[1 + 2*ki];
  } else {
    *keyP = airStrdup(nrrd->kvp[0 + 2*ki]);
    *valueP = airStrdup(nrrd->kvp[1 + 2*ki]);
  }
  return;
}

int
_nrrdKeyValueIdxFind(const Nrrd *nrrd, const char *key) {
  unsigned int nk, ki;

  nk = nrrd->kvpArr->len;
  for (ki=0; ki<nk; ki++) {
    if (!strcmp(nrrd->kvp[0 + 2*ki], key)) {
      break;
    }
  }
  return (ki<nk ? (int)ki : -1);  /* HEY scrutinize cast */
}

void
nrrdKeyValueClear(Nrrd *nrrd) {
  unsigned int nk, ki;

  if (!nrrd) {
    return;
  }

  nk = nrrd->kvpArr->len;
  for (ki=0; ki<nk; ki++) {
    nrrd->kvp[0 + 2*ki] = (char *)airFree(nrrd->kvp[0 + 2*ki]);
    nrrd->kvp[1 + 2*ki] = (char *)airFree(nrrd->kvp[1 + 2*ki]);
  }
  airArrayLenSet(nrrd->kvpArr, 0);
  
  return;
}

int
nrrdKeyValueErase(Nrrd *nrrd, const char *key) {
  unsigned int nk;
  int ki;
  
  if (!( nrrd && key )) {
    /* got NULL pointer */
    return 1;
  }
  ki = _nrrdKeyValueIdxFind(nrrd, key);
  if (-1 == ki) {
    return 0;
  }
  nrrd->kvp[0 + 2*ki] = (char *)airFree(nrrd->kvp[0 + 2*ki]);
  nrrd->kvp[1 + 2*ki] = (char *)airFree(nrrd->kvp[1 + 2*ki]);
  nk = nrrd->kvpArr->len;
  for (; ki<(int)nk-1; ki++) {  /* HEY scrutize cast */
    nrrd->kvp[0 + 2*ki] = nrrd->kvp[0 + 2*(ki+1)];
    nrrd->kvp[1 + 2*ki] = nrrd->kvp[1 + 2*(ki+1)];
  }
  airArrayLenIncr(nrrd->kvpArr, -1);

  return 0;
}

/*
******** nrrdKeyValueAdd
**
** This will COPY the given strings, and so does not depend on
** them existing past the return of this function
**
** does NOT use BIFF
*/
int
nrrdKeyValueAdd(Nrrd *nrrd, const char *key, const char *value) {
  int ki;

  if (!( nrrd && key && value )) {
    /* got NULL pointer */
    return 1;
  }
  if (!strlen(key)) {
    /* reject empty keys */
    return 1;
  }
  if (-1 != (ki = _nrrdKeyValueIdxFind(nrrd, key))) {
    nrrd->kvp[1 + 2*ki] = (char *)airFree(nrrd->kvp[1 + 2*ki]);
    nrrd->kvp[1 + 2*ki] = airStrdup(value);
  } else {
    ki = airArrayLenIncr(nrrd->kvpArr, 1);
    nrrd->kvp[0 + 2*ki] = airStrdup(key);
    nrrd->kvp[1 + 2*ki] = airStrdup(value);
  }

  return 0;
}

/*
******** nrrdKeyValueGet
**
** NOTE: whether or not *keyP and *valueP are set to pointers to memory
** "inside" the nrrd struct (pointers which you had better not free()!)
** is controlled by nrrdStateKeyValueReturnInternalPointers, which defaults
** to AIR_FALSE
**
** does NOT use BIFF
*/
char *
nrrdKeyValueGet(const Nrrd *nrrd, const char *key) {
  char *ret;
  int ki;
  
  if (!( nrrd && key )) {
    /* got NULL pointer */
    return NULL;
  }
  if (-1 != (ki = _nrrdKeyValueIdxFind(nrrd, key))) {
    if (nrrdStateKeyValueReturnInternalPointers) {
      ret = nrrd->kvp[1 + 2*ki];
    } else {
      ret = airStrdup(nrrd->kvp[1 + 2*ki]);
    }
  } else {
    ret = NULL;
  }
  return ret;
}

void
_nrrdWriteEscaped(FILE *file, char *dst, const char *str) {
  size_t ci, sl;

  for (ci=0; ci<strlen(str); ci++) {
    switch(str[ci]) {
    case '\n':
      if (file) {
        fprintf(file, "\\n");
      } else {
        strcat(dst, "\\n");
      }
      break;
    case '\\':
      if (file) {
        fprintf(file, "\\\\");
      } else {
        strcat(dst, "\\\\");
      }
      break;
    default:
      if (file) {
        fputc(str[ci], file);
      } else {
        sl = strlen(dst);
        dst[sl++] = str[ci];
        dst[sl] = '\0';
      }
      break;
    }
  }
  return;
}

/*
** _nrrdKeyValueWrite
**
** writes a given key and value to a file, starting with the given
** prefix (if non-NULL), and ending with "\n"
*/
int
_nrrdKeyValueWrite(FILE *file, char **stringP, const char *prefix, 
                   const char *key, const char *value) {
  
  if (!( (file || stringP) && key && value )) {
    return 1;
  }
  if (stringP) {
    /* 2*strlen() because at worst all characters will be escaped */
    *stringP = (char *)malloc(airStrlen(prefix) + 2*airStrlen(key)
                              + strlen(":=") + 2*airStrlen(value)
                              + strlen("\n") + 1);
    /* HEY error checking */
    strcpy(*stringP, "");
  }
  if (prefix) {
    if (file) {
      fprintf(file, "%s", prefix);
    } else {
      strcat(*stringP, prefix);
    }
  }
  if (file) {
    _nrrdWriteEscaped(file, NULL, key);
    fprintf(file, ":=");
    _nrrdWriteEscaped(file, NULL, value);
    fprintf(file, "\n");
  } else {
    _nrrdWriteEscaped(NULL, *stringP, key);
    strcat(*stringP, ":=");
    _nrrdWriteEscaped(NULL, *stringP, value);
    strcat(*stringP, "\n");
  }
  return 0;
}

/*
******** nrrdKeyValueCopy()
**
** copies key/value pairs from one nrrd to another
** Existing key/value pairs in nout are blown away
*/
int
nrrdKeyValueCopy(Nrrd *nout, const Nrrd *nin) {
  char *key, *value;
  unsigned int ki;

  if (!(nout && nin)) {
    /* got NULL pointer */
    return 1;
  }
  if (nout == nin) {
    /* can't satisfy semantics of copying with nout==nin */
    return 2;
  }

  nrrdKeyValueClear(nout);
  for (ki=0; ki<nin->kvpArr->len; ki++) {
    key = nin->kvp[0 + 2*ki];
    value = nin->kvp[1 + 2*ki];
    if (nrrdKeyValueAdd(nout, key, value)) {
      return 3;
    }
  }

  return 0;
}

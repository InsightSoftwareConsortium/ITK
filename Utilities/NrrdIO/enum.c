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

/*
******** airEnumUnknown
**
** return the value representing "unknown" in an enum
*/
int
airEnumUnknown(airEnum *enm) {
  
  if (enm && enm->val) {
    return enm->val[0];
  }
  else {
    return 0;
  }
}

/*
** _airEnumIndex()
**
** given an enum "enm" and value "val", return the index into enm->str[] 
** and enm->desc[] which correspond to that value.  To be safe, when
** given an invalid enum value, we return zero.
*/
int
_airEnumIndex(airEnum *enm, int val) {
  int i, ret;

  ret = 0;
  if (enm->val) {
    for (i=1; i<=enm->M; i++) {
      if (val == enm->val[i]) {
        ret = i;
        break;
      }
    }
  } else {
    ret = AIR_IN_CL(0, val, enm->M) ? val : 0;
  }
  return ret;
}

int
airEnumValCheck(airEnum *enm, int val) {

  return (0 == _airEnumIndex(enm, val));
}

const char *
airEnumStr(airEnum *enm, int val) {
  int idx;

  idx = _airEnumIndex(enm, val);
  return enm->str[idx];
}

const char *
airEnumDesc(airEnum *enm, int val) {
  int idx;

  idx = _airEnumIndex(enm, val);
  return enm->desc[idx];
}

int 
airEnumVal(airEnum *enm, const char *str) {
  char *strCpy, test[AIR_STRLEN_SMALL];
  int i;

  if (!str)
    return airEnumUnknown(enm);
  
  strCpy = airStrdup(str);
  if (!enm->sense) {
    airToLower(strCpy);
  }

  if (enm->strEqv) {
    for (i=0; strlen(enm->strEqv[i]); i++) {
      strncpy(test, enm->strEqv[i], AIR_STRLEN_SMALL);
      test[AIR_STRLEN_SMALL-1] = '\0';
      if (!enm->sense)
        airToLower(test);
      if (!strcmp(test, strCpy)) {
        free(strCpy);
        return enm->valEqv[i];
      }
    }
  }
  else {
    /* enm->strEqv NULL */
    for (i=1; i<=enm->M; i++) {
      strncpy(test, enm->str[i], AIR_STRLEN_SMALL);
      test[AIR_STRLEN_SMALL-1] = '\0';
      if (!enm->sense)
        airToLower(test);
      if (!strcmp(test, strCpy)) {
        free(strCpy);
        return enm->val ? enm->val[i] : i;
      }      
    }
  }

  /* else we never matched a string */
  free(strCpy);
  return airEnumUnknown(enm);
}

/*
******** airEnumFmtDesc()
**
** Formats a description line for one element "val" of airEnum "enm",
** and puts the result in a NEWLY ALLOCATED string which is the return
** of this function.  The formatting is done via sprintf(), as governed
** by "fmt", which should contain to "%s" conversion sequences, the
** first for the string version "val", and the second for the
** description If "canon", then the canonical string representation
** will be used (the one in enm->str[]), otherwise the shortest string
** representation will be used (which differs from the canonical one
** when there is a strEqv[]/valEqv[] pair defining a shorter string)
*/
char *
airEnumFmtDesc(airEnum *enm, int val, int canon, const char *fmt) {
  char *buff, *desc, ident[AIR_STRLEN_SMALL];
  const char *_ident;
  int i;
  size_t len;

  if (!(enm && enm->desc && fmt)) {
    return airStrdup("(airEnumDesc: invalid args)");
  }
  if (airEnumValCheck(enm, val)) {
    val = airEnumUnknown(enm);
  }
  _ident = airEnumStr(enm, val);
  if (!canon && enm->strEqv) {
    len = strlen(_ident);
    for (i=0; strlen(enm->strEqv[i]); i++) {
      if (val != enm->valEqv[i]) {
        /* this isn't a string representing the value we care about */
        continue;
      }
      if (strlen(enm->strEqv[i]) < len) {
        /* this one is shorter */
        len = strlen(enm->strEqv[i]);
        _ident = enm->strEqv[i];
      }
    }
  }
  strcpy(ident, _ident);
  if (!enm->sense) {
    airToLower(ident);
  }
  desc = enm->desc[_airEnumIndex(enm, val)];
  buff = (char *)calloc(strlen(fmt) + strlen(ident) + strlen(desc) + 1,
                        sizeof(char));
  if (buff) {
    sprintf(buff, fmt, ident, desc);
  }
  return buff;
}

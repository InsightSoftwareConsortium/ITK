/*
  NrrdIO: C library for NRRD file IO (with optional compressions)
  Copyright (C) 2009--2026  University of Chicago
  Copyright (C) 2005--2008  Gordon Kindlmann
  Copyright (C) 1998--2004  University of Utah

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

/* TO THE CITIZENS OF THE WORLD:
   I (GLK) would like to apologize for not supporting localizations
   that would change how commas vs periods are interpreted during
   number parsing. Please forgive me, or help me fix it :) */

static int
encodingAscii_available(void) {

  return AIR_TRUE;
}

/* fgetWord:
 *
 * TeemV2 replacement for old code that used unsafe `fscanf(file, "%s", buff)`
 * If nrrdStateEncodingAsciiCommaAllow, this treats commas a little bit like
 * white-space, and strives to reproduce behavior of old code wrt commas:
 * - ignores contiguous (non-white-space-separated) commas preceding number
 * - ignores contiguous commas following number
 * - ignores solo white-space-separated commas (e.g. " , ")
 * - breaks on white-space-separated commas (2 or more)
 */
static int
fgetWord(FILE *file, size_t II, char buff[AIR_STRLEN_SMALL + 1]) {
  static const char me[] = "fgetWord";
  uint idx = 0, comCount = 0;
  buff[idx] = '\0';
  /* in following comments, "charn" means "any CHARacter we attribute to the ascii
     representation of a Number" == a non-white-space (and possibly non-,) character */
  AIR_UNUSED(II);
  /* fprintf(stderr, "!%s(%u): ---------\n", me, AIR_UINT(II)); */
  while (1) {
    int gotWsp = AIR_FALSE;
    uint gotCom = 0; /* unsigned to match comCount */
    int car = getc(file);
    if (EOF == car) {
      if (!idx) {
        biffAddf(NRRD, "%s: hit EOF before getting any digit characters", me);
        return 1;
      }
      /* else we have read some charns, but now at EOF we're done; cool */
      break;
    }
    gotWsp = !!strchr(AIR_WHITESPACE, car);
    gotCom = nrrdStateEncodingAsciiCommaAllow && ',' == car;
    /* at most 1 of gotWsp,gotCom can be true; impossible for both to be true */
    comCount += gotCom;
    /* fprintf(stderr, "|%c|(%u)(w%d c%d(%u))\n", car, idx, gotWsp, gotCom, comCount); */
    if (gotWsp || gotCom) {
      if (!idx) { /* if have not gotten any charns yet */
        if (comCount && gotWsp) {
          if (comCount > 1) {
            biffAddf(NRRD,
                     "%s: saw %u contiguous commas and then whitespace "
                     "(which can't be a number)",
                     me, comCount);
            return 1;
          } else {
            /* saw a solo comma and then white-space; old code bizarrely ignored this;
               but nrrdStateEncodingAsciiCommaAllow means we're here to emulate it */
            comCount = 0;
            continue;
          }
        }
        /* we haven't gotten charns yet, keep trying */
        continue;
      } else {
        /* idx > 0 (and: gotWsp || gotCom) while we do have some charns */
        if (gotCom) {
          /* this is a *trailing* comma; it may be the only separation between this
             number (just collected) and the next (which we must treat as a delimiter),
             or it could be one of multiple trailing commas (which we ignore).
             Peek ahead to find out. */
          int peek = getc(file);
          if (EOF != peek) {
            if (',' != peek) {
              /* put back the non-comma (one character of pushback always allowed) */
              ungetc(peek, file);
              /* fprintf(stderr, "!%s: putting back peek %c\n", me, peek); */
              /* and we're done; break out */
              break;
            }
            /* else it's another trailing comma; ignore */
            continue;
          }
        }
        break;
      }
    } else {
      /* we got a new (non-whitespace, non-comma) charn;
         if we have space for it, record it in buff[idx] */
      if (idx == AIR_STRLEN_SMALL) {
        biffAddf(NRRD,
                 "%s: reading %u contiguous characters hit limit of parsing buffer; "
                 "is this really a file of space-separated%snumbers?",
                 me, AIR_STRLEN_SMALL,
                 nrrdStateEncodingAsciiCommaAllow ? " or comma-separated " : " ");
        return 1;
      }
      buff[idx] = (char)car;
      idx++;
    }
  }
  /* now \0-terminate the string */
  buff[idx] = '\0';
  return 0;
}

static int /* Biff: 1 */
encodingAscii_read(FILE *file, void *_data, size_t elNum, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[] = "encodingAscii_read";
  /* fprintf(stderr, "!%s: hi -------------------- \n", me); */
  int (*istore)(void *v, int j);
  char *valPtr;
  const char *conv;
  size_t I, elSize;

  AIR_UNUSED(nio);
  if (nrrdTypeBlock == nrrd->type) {
    biffAddf(NRRD, "%s: can't read nrrd type %s from %s", me,
             airEnumStr(nrrdType, nrrdTypeBlock), nrrdEncodingAscii->name);
    return 1;
  }
  valPtr = _data; /* initial pointer to single value, to point to first value */
  I = 0;
  elSize = nrrdElementSize(nrrd);
  istore = nrrdIStore[nrrd->type];
  conv = nrrdTypePrintfStr[nrrd->type];
  while (I < elNum) {
    int pret;
    char numbStr[AIR_STRLEN_SMALL + 1], stmp[2][AIR_STRLEN_SMALL + 1];
    if (fgetWord(file, I, numbStr)) {
      biffAddf(NRRD, "%s: couldn't fread value %s of %s", me,
               airSprintSize_t(stmp[0], I + 1), airSprintSize_t(stmp[1], elNum));
      return 1;
    }
    if (nrrd->type >= nrrdTypeInt) {
      /* can parse value directly into this type */
      pret = airSingleSscanf(numbStr, conv, valPtr);
    } else {
      /* sscanf value into an int first (but why?) */
      int tmpint;
      pret = airSingleSscanf(numbStr, "%d", &tmpint);
      if (1 == pret) {
        istore(valPtr, tmpint);
      }
    }
    if (1 != pret) {
      biffAddf(NRRD, "%s: couldn't parse \"%s\" as %s %s of %s", me, numbStr,
               airEnumStr(nrrdType, nrrd->type), airSprintSize_t(stmp[0], I + 1),
               airSprintSize_t(stmp[1], elNum));
      return 1;
    }
    valPtr += elSize; /* advance pointer to next value location */
    I++;
  }

  return 0;
}

static int /* Biff: 1 */
encodingAscii_write(FILE *file, const void *_data, size_t elNum, const Nrrd *nrrd,
                    NrrdIoState *nio) {
  static const char me[] = "encodingAscii_write";
  /* fprintf(stderr, "!%s: hi\n", me); */
  char buff[AIR_STRLEN_SMALL + 1];
  size_t buffSize = sizeof(buff);
  size_t bufflen, linelen;
  const char *data;
  size_t I, elSize;
  int newlined = AIR_FALSE;
  int (*sprint)(char *, size_t, const void *);

  if (nrrdTypeBlock == nrrd->type) {
    biffAddf(NRRD, "%s: can't write nrrd type %s to %s", me,
             airEnumStr(nrrdType, nrrdTypeBlock), nrrdEncodingAscii->name);
    return 1;
  }
  data = _data;
  linelen = 0;
  elSize = nrrdElementSize(nrrd);
  sprint = nrrdSprint[nrrd->type];
  for (I = 0; I < elNum; I++) {
    sprint(buff, buffSize, data);
    if (strlen(buff) + 1 == buffSize) {
      char stmp[2][AIR_STRLEN_SMALL + 1];
      biffAddf(NRRD,
               "%s: value %s of %s printed to string of max "
               "length (%u) for its buffer (which can't be right)",
               me, airSprintSize_t(stmp[0], I + 1), airSprintSize_t(stmp[1], elNum),
               AIR_STRLEN_SMALL);
      return 1;
    }
    if (1 == nrrd->dim) {
      fprintf(file, "%s\n", buff);
      newlined = AIR_TRUE;
    } else if (nrrd->dim == 2 && nrrd->axis[0].size <= nio->valsPerLine) {
      int nonewline = AIR_INT((I + 1) % (nrrd->axis[0].size));
      fprintf(file, "%s%c", buff, nonewline ? ' ' : '\n');
      newlined = !nonewline;
    } else {
      bufflen = strlen(buff);
      if (linelen + bufflen + 1 <= nio->charsPerLine) {
        fprintf(file, "%s%s", I ? " " : "", buff);
        linelen += (I ? 1 : 0) + bufflen;
      } else {
        fprintf(file, "\n%s", buff);
        linelen = bufflen;
      }
      newlined = AIR_FALSE;
    }
    data += elSize;
  }
  if (!newlined) {
    /* always end file with a carraige return; but guard with this
       conditional so we don't create a final blank line */
    fprintf(file, "\n");
  }
  fflush(file);

  return 0;
}

const NrrdEncoding nrrd__EncodingAscii = {"ASCII",   /* name */
                                          "ascii",   /* suffix */
                                          AIR_FALSE, /* endianMatters */
                                          AIR_FALSE, /* isCompression */
                                          encodingAscii_available,
                                          encodingAscii_read,
                                          encodingAscii_write};

const NrrdEncoding *const nrrdEncodingAscii = &nrrd__EncodingAscii;

/*
  NrrdIO: C library for NRRD file IO (with optional compressions)
  Copyright (C) 2009--2025  University of Chicago
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

/* this was part of an old hail Mary debugging effort;
  bug has probably been found :)
  Removing this since it is a weird symbol to see in the "nm" output */
/* static FILE *_fileSave = NULL; */

static int
_nrrdEncodingAscii_available(void) {

  return AIR_TRUE;
}

static int /* Biff: 1 */
_nrrdEncodingAscii_read(FILE *file, void *_data, size_t elNum, Nrrd *nrrd,
                        NrrdIoState *nio) {
  static const char me[] = "_nrrdEncodingAscii_read";
  char numbStr[AIR_STRLEN_HUGE + 1]; /* HEY: fix this */
  char *nstr;
  size_t I;
  char *data;
  int tmp;

  AIR_UNUSED(nio);
  /* _fileSave = file; */
  if (nrrdTypeBlock == nrrd->type) {
    biffAddf(NRRD, "%s: can't read nrrd type %s from %s", me,
             airEnumStr(nrrdType, nrrdTypeBlock), nrrdEncodingAscii->name);
    return 1;
  }
  data = (char *)_data;
  I = 0;
  while (I < elNum) {
    char stmp[2][AIR_STRLEN_SMALL + 1];
    /* HEY: we can easily suffer here from a standard buffer overflow problem;
       this was a source of a mysterious unu crash:
         echo "0 0 0 0 1 0 0 0 0" \
          | unu reshape -s 9 1 1 \
          | unu pad -min 0 0 0 -max 8 8 8 \
          | unu make -s 9 9 9 -t float -e ascii -ls 10 \
            -spc LPS -orig "(0,0,0)" -dirs "(1,0,0) (0,1,0) (0,0,1)"
       This particular case is resolved by changing AIR_STRLEN_HUGE
       to AIR_STRLEN_HUGE*100, but the general problem remains.  This
       motivated adding the memory corruption test
       HEY HEY: 2023 GLK does not know what buffer AIR_STRLEN_HUGE*100
       could be describing; what was this?? */
    if (1 != fscanf(file, "%s", numbStr)) {
      biffAddf(NRRD, "%s: couldn't parse element %s of %s", me,
               airSprintSize_t(stmp[0], I + 1), airSprintSize_t(stmp[1], elNum));
      return 1;
    }
    /*
    if (file != _fileSave) {
      fprintf(stderr, "%s: PANIC memory corruption detected\n", me);
      / * this may crash, hence the fprintf above to help debug * /
      biffAddf(NRRD, "%s: PANIC memory corruption detected", me);
      return 1;
    }
    */
    if (!strcmp(",", numbStr)) {
      /* its an isolated comma, not a value, pass over this */
      continue;
    }
    /* get past any commas prefixing a number (without space) */
    nstr = numbStr + strspn(numbStr, ",");
    if (nrrd->type >= nrrdTypeInt) {
      /* sscanf supports putting value directly into this type */
      if (1
          != airSingleSscanf(nstr, nrrdTypePrintfStr[nrrd->type],
                             (void *)(data + I * nrrdElementSize(nrrd)))) {
        biffAddf(NRRD, "%s: couldn't parse %s %s of %s (\"%s\")", me,
                 airEnumStr(nrrdType, nrrd->type), airSprintSize_t(stmp[0], I + 1),
                 airSprintSize_t(stmp[1], elNum), nstr);
        return 1;
      }
    } else {
      /* sscanf value into an int first */
      if (1 != airSingleSscanf(nstr, "%d", &tmp)) {
        biffAddf(NRRD, "%s: couldn't parse element %s of %s (\"%s\")", me,
                 airSprintSize_t(stmp[0], I + 1), airSprintSize_t(stmp[1], elNum), nstr);
        return 1;
      }
      nrrdIInsert[nrrd->type](data, I, tmp);
    }
    I++;
  }

  return 0;
}

static int /* Biff: 1 */
_nrrdEncodingAscii_write(FILE *file, const void *_data, size_t elNum, const Nrrd *nrrd,
                         NrrdIoState *nio) {
  static const char me[] = "_nrrdEncodingAscii_write";
  char buff[AIR_STRLEN_MED + 1];
  size_t bufflen, linelen;
  const char *data;
  size_t I;
  int newlined = AIR_FALSE;

  if (nrrdTypeBlock == nrrd->type) {
    biffAddf(NRRD, "%s: can't write nrrd type %s to %s", me,
             airEnumStr(nrrdType, nrrdTypeBlock), nrrdEncodingAscii->name);
    return 1;
  }
  data = AIR_CAST(const char *, _data);
  linelen = 0;
  for (I = 0; I < elNum; I++) {
    nrrdSprint[nrrd->type](buff, data);
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
    data += nrrdElementSize(nrrd);
  }
  if (!newlined) {
    /* always end file with a carraige return; but guard with this
       conditional so we don't create a final blank line */
    fprintf(file, "\n");
  }
  fflush(file);

  return 0;
}

const NrrdEncoding _nrrdEncodingAscii = {"ASCII",   /* name */
                                         "ascii",   /* suffix */
                                         AIR_FALSE, /* endianMatters */
                                         AIR_FALSE, /* isCompression */
                                         _nrrdEncodingAscii_available,
                                         _nrrdEncodingAscii_read,
                                         _nrrdEncodingAscii_write};

const NrrdEncoding *const nrrdEncodingAscii = &_nrrdEncodingAscii;

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

#define MAGIC_P6 "P6"
#define MAGIC_P5 "P5"
#define MAGIC_P3 "P3"
#define MAGIC_P2 "P2"

int
_nrrdFormatPNM_available(void) {
  
  return AIR_TRUE;
}

int
_nrrdFormatPNM_nameLooksLike(const char *filename) {
  
  return (airEndsWith(filename, NRRD_EXT_PGM)
          || airEndsWith(filename, NRRD_EXT_PPM));
}

int
_nrrdFormatPNM_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding,
                        int useBiff) {
  static const char me[]="_nrrdFormatPNM_fitsInto";
  int ret;
  
  if (!( nrrd && encoding )) {
    biffMaybeAddf(useBiff, NRRD, "%s: got NULL nrrd (%p) or encoding (%p)",
                  me, AIR_CAST(void*, nrrd), AIR_CAST(void*, encoding)); 
    return AIR_FALSE;
  }
  if (nrrdTypeUChar != nrrd->type) {
    biffMaybeAddf(useBiff, NRRD, "%s: type must be %s (not %s)", me,
                  airEnumStr(nrrdType, nrrdTypeUChar),
                  airEnumStr(nrrdType, nrrd->type)); 
    return AIR_FALSE;
  }
  if (!( nrrdEncodingRaw == encoding || nrrdEncodingAscii == encoding)) {
    biffMaybeAddf(useBiff, NRRD, "%s: encoding can only be %s or %s", me,
                  nrrdEncodingRaw->name, nrrdEncodingAscii->name); 
    return AIR_FALSE;
  }
  if (2 == nrrd->dim) {
    /* its a gray-scale image */
    ret = 2;
  } else if (3 == nrrd->dim) {
    if (1 == nrrd->axis[0].size) {
      /* its a faux-3D image, really grayscale */
      ret = 2;
    } else if (3 == nrrd->axis[0].size) {
      /* its a real color image */
      ret = 3;
    } else {
      /* else its no good */
      biffMaybeAddf(useBiff, NRRD,
                    "%s: dim is 3, but 1st axis size is " _AIR_SIZE_T_CNV 
                    ", not 1 or 3", me, nrrd->axis[0].size); 
      return AIR_FALSE;
    }
  } else {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: dimension is %d, not 2 or 3", me, nrrd->dim);
    return AIR_FALSE;
  }
  return ret;
}

int
_nrrdFormatPNM_contentStartsLike(NrrdIoState *nio) {
  
  return (!strcmp(MAGIC_P6, nio->line)
          || !strcmp(MAGIC_P5, nio->line)
          || !strcmp(MAGIC_P3, nio->line)
          || !strcmp(MAGIC_P2, nio->line));
}

int
_nrrdFormatPNM_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdFormatPNM_read";
  const char *fs;
  char *perr;
  int color, got, want, ret, val[5], sx, sy, max, magic;
  unsigned int i, llen;

  if (!_nrrdFormatPNM_contentStartsLike(nio)) {
    biffAddf(NRRD, "%s: this doesn't look like a %s file", me, 
             nrrdFormatPNM->name);
    return 1;
  }
  nrrd->type = nrrdTypeUChar;
  if (!strcmp(MAGIC_P6, nio->line)) {
    magic = 6;
  } else if (!strcmp(MAGIC_P5, nio->line)) {
    magic = 5;
  } else if (!strcmp(MAGIC_P3, nio->line)) {
    magic = 3;
  } else if (!strcmp(MAGIC_P2, nio->line)) {
    magic = 2;
  } else {
    fprintf(stderr, "%s: PANIC: magic \"%s\" not handled\n", me, nio->line);
    biffAddf(NRRD, "%s: PANIC: magic \"%s\" not handled\n", me, nio->line);
    return 1;
  }
  
  switch(magic) {
  case 2:
    color = AIR_FALSE;
    nio->encoding = nrrdEncodingAscii;
    nrrd->dim = 2;
    break;
  case 3:
    color = AIR_TRUE;
    nio->encoding = nrrdEncodingAscii;
    nrrd->dim = 3;
    break;
  case 5:
    color = AIR_FALSE;
    nio->encoding = nrrdEncodingRaw;
    nrrd->dim = 2;
    break;
  case 6:
    color = AIR_TRUE;
    nio->encoding = nrrdEncodingRaw;
    nrrd->dim = 3;
    break;
  default:
    biffAddf(NRRD, "%s: sorry, PNM magic %d not implemented", me, magic);
    return 1;
    break;
  }
  val[0] = val[1] = val[2] = 0;
  got = 0;
  want = 3;
  while (got < want) {
    nio->pos = 0;
    if (_nrrdOneLine(&llen, nio, file)) {
      biffAddf(NRRD, "%s: failed to get line from PNM header", me);
      return 1;
    }
    if (!(0 < llen)) {
      biffAddf(NRRD, "%s: hit EOF in header with %d of %d ints parsed",
               me, got, want);
      return 1;
    }
    if ('#' == nio->line[0]) {
      if (strncmp(nio->line, NRRD_PNM_COMMENT, strlen(NRRD_PNM_COMMENT))) {
        /* this is a generic comment */
        ret = 0;
        goto plain;
      }
      /* else this PNM comment is trying to tell us something */
      nio->pos = AIR_CAST(int, strlen(NRRD_PNM_COMMENT));
      nio->pos += AIR_CAST(int, strspn(nio->line + nio->pos, _nrrdFieldSep));
      ret = _nrrdReadNrrdParseField(nio, AIR_FALSE);
      if (!ret) {
        if (1 <= nrrdStateVerboseIO) {
          fprintf(stderr, "(%s: unparsable field \"%s\" --> plain comment)\n",
                  me, nio->line);
        }
        goto plain;
      }
      if (nrrdField_comment == ret) {
        ret = 0;
        goto plain;
      }
      fs = airEnumStr(nrrdField, ret);
      if (!_nrrdFieldValidInImage[ret]) {
        if (1 <= nrrdStateVerboseIO) {
          fprintf(stderr, "(%s: field \"%s\" (not allowed in PNM) "
                  "--> plain comment)\n", me, fs);
        }
        ret = 0;
        goto plain;
      }
      if (!nio->seen[ret] 
          && nrrdFieldInfoParse[ret](file, nrrd, nio, AIR_TRUE)) {
        perr = biffGetDone(NRRD);
        if (1 <= nrrdStateVerboseIO) {
          fprintf(stderr, "(%s: unparsable info for field \"%s\" "
                  "--> plain comment:\n%s)\n", me, fs, perr);
        }
        free(perr);
        ret = 0;
        goto plain;
      }
      nio->seen[ret] = AIR_TRUE;
    plain:
      if (!ret) {
        if (nrrdCommentAdd(nrrd, nio->line+1)) {
          biffAddf(NRRD, "%s: couldn't add comment", me);
          return 1;
        }
      }
      continue;
    }
    
    if (3 == sscanf(nio->line, "%d%d%d", val+got+0, val+got+1, val+got+2)) {
      got += 3;
      continue;
    }
    if (2 == sscanf(nio->line, "%d%d", val+got+0, val+got+1)) {
      got += 2;
      continue;
    }
    if (1 == sscanf(nio->line, "%d", val+got+0)) {
      got += 1;
      continue;
    }

    /* else, we couldn't parse ANY numbers on this line, which is okay
       as long as the line contains nothing but white space */
    for (i=0; i<=strlen(nio->line)-1 && isspace(nio->line[i]); i++)
      ;
    if (i != strlen(nio->line)) {
      biffAddf(NRRD, "%s: \"%s\" has no integers but isn't just whitespace", 
               me, nio->line);
      return 1;
    }
  }
  /* this assumes 3 == want */
  sx = val[0];
  sy = val[1];
  max = val[2];
  if (!(sx > 0 && sy > 0 && max > 0)) {
    biffAddf(NRRD, "%s: sx,sy,max of %d,%d,%d has problem", me, sx, sy, max);
    return 1;
  }
  if (255 != max) {
    biffAddf(NRRD, "%s: sorry, can only deal with max value 255 (not %d)", 
             me, max);
    return 1;
  }

  /* we know what we need in order to set nrrd fields and read data */
  if (color) {
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoSize,
                       AIR_CAST(size_t, 3),
                       AIR_CAST(size_t, sx),
                       AIR_CAST(size_t, sy));
  } else {
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoSize,
                       AIR_CAST(size_t, sx),
                       AIR_CAST(size_t, sy));
  }
  if (!nio->skipData) {
    if (_nrrdCalloc(nrrd, nio, file)) {
      biffAddf(NRRD, "%s: couldn't allocate memory for data", me);
      return 1;
    }
    if (nio->encoding->read(file, nrrd->data, nrrdElementNumber(nrrd),
                            nrrd, nio)) {
      biffAddf(NRRD, "%s:", me);
      return 1;
    }
  } else {
    nrrd->data = NULL;
  }

  return 0;
}

int
_nrrdFormatPNM_write(FILE *file, const Nrrd *_nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdFormatPNM_write";
  int color, sx, sy, magic, fi;
  unsigned int ci;
  Nrrd *nrrd;
  airArray *mop;
  
  mop = airMopNew();
  airMopAdd(mop, nrrd = nrrdNew(), (airMopper)nrrdNuke, airMopAlways);
  if (nrrdCopy(nrrd, _nrrd)) {
    biffAddf(NRRD, "%s: couldn't make private copy", me);
    airMopError(mop); return 1;
  }
  if (3 == nrrd->dim && 1 == nrrd->axis[0].size) {
    if (nrrdAxesDelete(nrrd, nrrd, 0)) {
      biffAddf(NRRD, "%s:", me);
      airMopError(mop); return 1;
    }
  }
  color = (3 == nrrd->dim);
  if (!color) {
    magic = (nrrdEncodingAscii == nio->encoding ? 2 : 5);
    sx = AIR_CAST(int, nrrd->axis[0].size);
    sy = AIR_CAST(int, nrrd->axis[1].size);
  } else {
    magic = (nrrdEncodingAscii == nio->encoding ? 3 : 6);
    sx = AIR_CAST(int, nrrd->axis[1].size);
    sy = AIR_CAST(int, nrrd->axis[2].size);
  }
  
  fprintf(file, "P%d\n", magic);
  fprintf(file, "%d %d\n", sx, sy);
  for (fi=nrrdField_unknown+1; fi<nrrdField_last; fi++) {
    if (_nrrdFieldValidInImage[fi] 
        && _nrrdFieldInteresting(nrrd, nio, fi)) {
      _nrrdFprintFieldInfo(file, NRRD_PNM_COMMENT, nrrd, nio, fi);
    }
  }
  for (ci=0; ci<nrrd->cmtArr->len; ci++) {
    fprintf(file, "# %s\n", nrrd->cmt[ci]);
  }
  fprintf(file, "255\n");

  if (!nio->skipData) {
    if (nio->encoding->write(file, nrrd->data, nrrdElementNumber(nrrd),
                             nrrd, nio)) {
      biffAddf(NRRD, "%s:", me);
      airMopError(mop); return 1;
    }
  }
  
  airMopError(mop); 
  return 0;
}

const NrrdFormat
_nrrdFormatPNM = {
  "PNM",
  AIR_TRUE,   /* isImage */
  AIR_TRUE,   /* readable */
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

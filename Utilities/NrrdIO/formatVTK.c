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

#define MAGIC1 "# vtk DataFile Version 1.0"
#define MAGIC2 "# vtk DataFile Version 2.0"
#define MAGIC3 "# vtk DataFile Version 3.0"

int
_nrrdFormatVTK_available(void) {
  
  return AIR_TRUE;
}

int
_nrrdFormatVTK_nameLooksLike(const char *fname) {
  
  return airEndsWith(fname, NRRD_EXT_VTK);
}

int
_nrrdFormatVTK_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding,
                        int useBiff) {
  static const char me[]="_nrrdFormatVTK_fitsInto";
  
  if (!( nrrd && encoding )) {
    biffMaybeAddf(useBiff, NRRD, "%s: got NULL nrrd (%p) or encoding (%p)",
                  me, AIR_CAST(void*, nrrd), AIR_CAST(void*, encoding));
    return AIR_FALSE;
  }
  if (!( nrrdEncodingRaw == encoding || nrrdEncodingAscii == encoding)) {
    biffMaybeAddf(useBiff, NRRD, "%s: encoding can only be %s or %s", me,
                  nrrdEncodingRaw->name, nrrdEncodingAscii->name); 
    return AIR_FALSE;
  }
  if (!( nrrdTypeUChar == nrrd->type
         || nrrdTypeChar == nrrd->type
         || nrrdTypeUShort == nrrd->type
         || nrrdTypeShort == nrrd->type
         || nrrdTypeUInt == nrrd->type
         || nrrdTypeInt == nrrd->type
         || nrrdTypeFloat == nrrd->type
         || nrrdTypeDouble == nrrd->type )) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: type %s doesn't fit in VTK (as currently implemented)",
                  me, airEnumStr(nrrdType, nrrd->type));
    return AIR_FALSE;
  }
  if (!( 3 == nrrd->dim
         || (4 == nrrd->dim && 3 == nrrd->axis[0].size)
         || (4 == nrrd->dim && 9 == nrrd->axis[0].size) )) {
    biffMaybeAddf(useBiff, NRRD, "%s: nrrd didn't look like a volume of "
                  "scalars, vectors, or matrices", me); 
    return AIR_FALSE;
  }
  return AIR_TRUE;
}

int
_nrrdFormatVTK_contentStartsLike(NrrdIoState *nio) {
  
  return (!strcmp(MAGIC1, nio->line)
          || !strcmp(MAGIC2, nio->line)
          || !strcmp(MAGIC3, nio->line));
}

int
_nrrdFormatVTK_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdReadVTK";
  char *three[3];
  int sx, sy, sz, ret, N;
  double xm=0.0, ym=0.0, zm=0.0, xs=1.0, ys=1.0, zs=1.0;
  airArray *mop;
  unsigned int llen;

  if (!_nrrdFormatVTK_contentStartsLike(nio)) {
    biffAddf(NRRD, "%s: this doesn't look like a %s file", me, 
             nrrdFormatVTK->name);
    return 1;
  }

#define GETLINE(what)                                        \
  do {                                                       \
    ret = _nrrdOneLine(&llen, nio, file);                    \
  } while (!ret && (1 == llen));                             \
  if (ret || !llen) {                                        \
    biffAddf(NRRD, "%s: couldn't get " #what " line", me);   \
    return 1;                                                \
  }
  
  /* read in content */
  GETLINE(content);
  if (strcmp(NRRD_UNKNOWN, nio->line)) {
    if (!(nrrd->content = airStrdup(nio->line))) {
      biffAddf(NRRD, "%s: couldn't read or copy content string", me);
      return 1;
    }
  }
  GETLINE(encoding); airToUpper(nio->line);
  if (!strcmp("ASCII", nio->line)) {
    nio->encoding = nrrdEncodingAscii;
  } else if (!strcmp("BINARY", nio->line)) {
    nio->encoding = nrrdEncodingRaw;
  } else {
    biffAddf(NRRD, "%s: encoding \"%s\" wasn't \"ASCII\" or \"BINARY\"",
             me, nio->line);
    return 1;
  }
  GETLINE(DATASET); airToUpper(nio->line);
  if (!strstr(nio->line, "STRUCTURED_POINTS")) {
    biffAddf(NRRD,
             "%s: sorry, only STRUCTURED_POINTS data is nrrd-ready", me);
    return 1;
  }
  GETLINE(DIMENSIONS); airToUpper(nio->line);
  if (!strstr(nio->line, "DIMENSIONS")
      || 3 != sscanf(nio->line, "DIMENSIONS %d %d %d", &sx, &sy, &sz)) {
    biffAddf(NRRD, "%s: couldn't parse DIMENSIONS line (\"%s\")",
             me, nio->line);
    return 1;
  }
  GETLINE(next); airToUpper(nio->line);
  while (!strstr(nio->line, "POINT_DATA")) {
    if (strstr(nio->line, "ORIGIN")) {
      if (3 != sscanf(nio->line, "ORIGIN %lf %lf %lf", &xm, &ym, &zm)) {
        biffAddf(NRRD, "%s: couldn't parse ORIGIN line (\"%s\")",
                 me, nio->line);
        return 1;
      }
    } else if (strstr(nio->line, "SPACING")) {
      if (3 != sscanf(nio->line, "SPACING %lf %lf %lf",
                      &xs, &ys, &zs)) {
        biffAddf(NRRD, "%s: couldn't parse SPACING line (\"%s\")",
                 me, nio->line);
        return 1;
      }      
    } else if (strstr(nio->line, "ASPECT_RATIO")) {
      if (3 != sscanf(nio->line, "ASPECT_RATIO %lf %lf %lf",
                      &xs, &ys, &zs)) {
        biffAddf(NRRD, "%s: couldn't parse ASPECT_RATIO line (\"%s\")",
                 me, nio->line);
        return 1;
      }      
    }
    GETLINE(next); airToUpper(nio->line);
  }
  if (1 != sscanf(nio->line, "POINT_DATA %d", &N)) {
    biffAddf(NRRD, "%s: couldn't parse POINT_DATA line (\"%s\")",
             me, nio->line);
    return 1;
  }
  if (N != sx*sy*sz) {
    biffAddf(NRRD,
             "%s: product of sizes (%d*%d*%d == %d) != # elements (%d)", 
             me, sx, sy, sz, sx*sy*sz, N);
    return 1;
  }
  GETLINE(attribute declaration);
  mop = airMopNew();
  if (3 != airParseStrS(three, nio->line, AIR_WHITESPACE, 3, AIR_FALSE)) {
    biffAddf(NRRD,
             "%s: didn't see three words in attribute declaration \"%s\"",
             me, nio->line);
    return 1;
  }
  airMopAdd(mop, three[0], airFree, airMopAlways);
  airMopAdd(mop, three[1], airFree, airMopAlways);
  airMopAdd(mop, three[2], airFree, airMopAlways);
  airToLower(three[2]);
  if (!strcmp(three[2], "bit")) {
    if (nrrdEncodingAscii == nio->encoding) {
      fprintf(stderr, "%s: WARNING: \"bit\"-type data will be read in as "
              "unsigned char\n", me);
      nrrd->type = nrrdTypeUChar;
    } else {
      biffAddf(NRRD, "%s: can't read in \"bit\"-type data as BINARY", me);
      return 1;
    }
  } else if (!strcmp(three[2], "unsigned_char")) {
    nrrd->type = nrrdTypeUChar;
  } else if (!strcmp(three[2], "char")) {
    nrrd->type = nrrdTypeChar;
  } else if (!strcmp(three[2], "unsigned_short")) {
    nrrd->type = nrrdTypeUShort;
  } else if (!strcmp(three[2], "short")) {
    nrrd->type = nrrdTypeShort;
  } else if (!strcmp(three[2], "unsigned_int")) {
    nrrd->type = nrrdTypeUInt;
  } else if (!strcmp(three[2], "int")) {
    nrrd->type = nrrdTypeInt;
  } else if (!strcmp(three[2], "float")) {
    nrrd->type = nrrdTypeFloat;
  } else if (!strcmp(three[2], "double")) {
    nrrd->type = nrrdTypeDouble;
  } else {
    /* "unsigned_long" and "long" fall in here- I don't know what
       the VTK people mean by these types, since always mean different
       things on 32-bit versus 64-bit architectures */
    biffAddf(NRRD, "%s: type \"%s\" not recognized", me, three[2]);
    airMopError(mop); return 1;
  }
  airToUpper(three[0]);
  if (!strncmp("SCALARS", three[0], strlen("SCALARS"))) {
    GETLINE(LOOKUP_TABLE); airToUpper(nio->line);
    if (strcmp(nio->line, "LOOKUP_TABLE DEFAULT")) {
      biffAddf(NRRD,
               "%s: sorry, can only deal with default LOOKUP_TABLE", me);
      airMopError(mop); return 1;
    }
    nrrd->dim = 3;
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoSize,
                       AIR_CAST(size_t, sx),
                       AIR_CAST(size_t, sy),
                       AIR_CAST(size_t, sz));
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoSpacing, xs, ys, zs);
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoMin, xm, ym, zm);
  } else if (!strncmp("VECTORS", three[0], strlen("VECTORS"))) {
    nrrd->dim = 4;
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoSize,
                       AIR_CAST(size_t, 3),
                       AIR_CAST(size_t, sx),
                       AIR_CAST(size_t, sy),
                       AIR_CAST(size_t, sz));
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoSpacing, AIR_NAN, xs, ys, zs);
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoMin, AIR_NAN, xm, ym, zm);
    nrrd->axis[0].kind = nrrdKind3Vector;
  } else if (!strncmp("TENSORS", three[0], strlen("TENSORS"))) {
    nrrd->dim = 4;
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoSize,
                       AIR_CAST(size_t, 9),
                       AIR_CAST(size_t, sx),
                       AIR_CAST(size_t, sy),
                       AIR_CAST(size_t, sz));
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoSpacing, AIR_NAN, xs, ys, zs);
    nrrdAxisInfoSet_va(nrrd, nrrdAxisInfoMin, AIR_NAN, xm, ym, zm);
    nrrd->axis[0].kind = nrrdKind3DMatrix;
  } else {
    biffAddf(NRRD,
             "%s: sorry, can only deal with SCALARS, VECTORS, and TENSORS "
             "currently, so couldn't parse attribute declaration \"%s\"",
             me, nio->line);
    airMopError(mop); return 1;
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
    if (1 < nrrdElementSize(nrrd)
        && nio->encoding->endianMatters
        && airMyEndian != airEndianBig) {
      /* encoding exposes endianness, and its big, but we aren't */
      nrrdSwapEndian(nrrd);
    }
  } else {
    nrrd->data = NULL;
  }
    
  airMopOkay(mop);
  return 0;
}

/* this strongly assumes that nrrdFitsInFormat() was true */
int
_nrrdFormatVTK_write(FILE *file, const Nrrd *_nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdFormatVTK_write";
  int i, sx, sy, sz, sax;
  double xs, ys, zs, xm, ym, zm;
  char type[AIR_STRLEN_MED], name[AIR_STRLEN_SMALL];
  Nrrd *nrrd;
  airArray *mop;

  /* HEY: should this copy be done more conservatively */
  mop = airMopNew();
  airMopAdd(mop, nrrd=nrrdNew(), (airMopper)nrrdNuke, airMopAlways);
  if (nrrdCopy(nrrd, _nrrd)) {
    biffAddf(NRRD, "%s: couldn't make private copy", me);
    airMopError(mop); return 1;
  }
  if (!( 3 == nrrd->dim || 
         (4 == nrrd->dim && (3 == nrrd->axis[0].size ||
                             9 == nrrd->axis[0].size)) )) {
    biffAddf(NRRD, "%s: doesn't seem to be scalar, vector, or matrix", me);
    airMopError(mop); return 1;
  }
  sax = nrrd->dim - 3;
  xs = nrrd->axis[sax+0].spacing;
  ys = nrrd->axis[sax+1].spacing;
  zs = nrrd->axis[sax+2].spacing;
  if (!( AIR_EXISTS(xs) && AIR_EXISTS(ys) && AIR_EXISTS(zs) )) {
    xs = ys = zs = 1.0;
  }
  xm = nrrd->axis[sax+0].min;
  ym = nrrd->axis[sax+1].min;
  zm = nrrd->axis[sax+2].min;
  if (!( AIR_EXISTS(xm) && AIR_EXISTS(ym) && AIR_EXISTS(zm) )) {
    xm = ym = zm = 0.0;
  }
  sx = AIR_CAST(int, nrrd->axis[sax+0].size);
  sy = AIR_CAST(int, nrrd->axis[sax+1].size);
  sz = AIR_CAST(int, nrrd->axis[sax+2].size);

  switch(nrrd->type) {
  case nrrdTypeUChar:
    strcpy(type, "unsigned_char");
    break;
  case nrrdTypeChar:
    strcpy(type, "char");
    break;
  case nrrdTypeUShort:
    strcpy(type, "unsigned_short");
    break;
  case nrrdTypeShort:
    strcpy(type, "short");
    break;
  case nrrdTypeUInt:
    strcpy(type, "unsigned_int");
    break;
  case nrrdTypeInt:
    strcpy(type, "int");
    break;
  case nrrdTypeFloat:
    strcpy(type, "float");
    break;
  case nrrdTypeDouble:
    strcpy(type, "double");
    break;
  default:
    biffAddf(NRRD, "%s: can't put %s-type nrrd into VTK", me, 
             airEnumStr(nrrdType, nrrd->type));
    airMopError(mop); return 1;
  }
  fprintf(file, "%s\n", MAGIC3);
  /* there is a file-format-imposed limit on the length of the "content" */
  if (nrrd->content) {
    /* when the "250" below was previously "255", vtk didn't deal */
    for (i=0; i<=250 && nrrd->content[i]; i++) {
      fputc(nrrd->content[i], file);
    }
    fputc('\n', file);
  } else {
    fprintf(file, NRRD_UNKNOWN "\n");
  }
  if (nrrdEncodingRaw == nio->encoding) {
    fprintf(file, "BINARY\n");
  } else {
    fprintf(file, "ASCII\n");
  }
  fprintf(file, "DATASET STRUCTURED_POINTS\n");
  fprintf(file, "DIMENSIONS %d %d %d\n", sx, sy, sz);
  fprintf(file, "ORIGIN %g %g %g\n", xm, ym, zm);
  fprintf(file, "SPACING %g %g %g\n", xs, ys, zs);
  fprintf(file, "POINT_DATA %d\n", sx*sy*sz);
  airSrandMT(AIR_CAST(unsigned int, airTime()));
  sprintf(name, "nrrd%05d", airRandInt(100000));
  if (3 == nrrd->dim) {
    fprintf(file, "SCALARS %s %s\n", name, type);
    fprintf(file, "LOOKUP_TABLE default\n");
  } else {
    /* 4 == nrrd->dim */
    if (3 == nrrd->axis[0].size) {
      fprintf(file, "VECTORS %s %s\n", name, type);
    } else {
      fprintf(file, "TENSORS %s %s\n", name, type);
    }
  }
  if (1 < nrrdElementSize(nrrd)
      && nio->encoding->endianMatters
      && airMyEndian != airEndianBig) {
    /* encoding exposes endianness, and we're not big, as req.d by VTK */
    nrrdSwapEndian(nrrd);
  }
  if (nio->encoding->write(file, nrrd->data, nrrdElementNumber(nrrd),
                           nrrd, nio)) {
    biffAddf(NRRD, "%s:", me);
    airMopError(mop); return 1;
  }
  
  airMopOkay(mop); 
  return 0;
}

const NrrdFormat
_nrrdFormatVTK = {
  "VTK",
  AIR_FALSE,  /* isImage */
  AIR_TRUE,   /* readable */
  AIR_FALSE,  /* usesDIO */
  _nrrdFormatVTK_available,
  _nrrdFormatVTK_nameLooksLike,
  _nrrdFormatVTK_fitsInto,
  _nrrdFormatVTK_contentStartsLike,
  _nrrdFormatVTK_read,
  _nrrdFormatVTK_write
};

const NrrdFormat *const
nrrdFormatVTK = &_nrrdFormatVTK;

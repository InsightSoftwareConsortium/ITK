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

int
_nrrdFormatEPS_available(void) {

  /* but only for writing ... */
  return AIR_TRUE;
}

int
_nrrdFormatEPS_nameLooksLike(const char *filename) {
  
  return airEndsWith(filename, NRRD_EXT_EPS);
}

int
_nrrdFormatEPS_fitsInto(const Nrrd *nrrd, const NrrdEncoding *encoding,
                        int useBiff) {
  static const char me[]="_nrrdFormatEPS_fitsInto";
  int ret;

  AIR_UNUSED(encoding);
  /* encoding information is ignored- its always going to be hex */
  if (!nrrd) {
    biffMaybeAddf(useBiff, NRRD, "%s: got NULL nrrd (%p)",
                  me, AIR_CAST(void*, nrrd)); 
    return AIR_FALSE;
  }
  if (nrrdTypeUChar != nrrd->type) {
    biffMaybeAddf(useBiff, NRRD, "%s: type must be %s (not %s)", me,
                  airEnumStr(nrrdType, nrrdTypeUChar),
                  airEnumStr(nrrdType, nrrd->type)); 
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
      /* its a real RGB color image */
      ret = 3;
    } else if (4 == nrrd->axis[0].size) {
      /* its a real CMYK (our best guess) color image */
      ret = 3;
    } else {
      /* else its no good */
      biffMaybeAddf(useBiff, NRRD,
                    "%s: dim is 3, but 1st axis size is " _AIR_SIZE_T_CNV
                    ", not 1, 3, or 4", me, nrrd->axis[0].size); 
      return AIR_FALSE;
    }
  } else {
    biffMaybeAddf(useBiff, NRRD, "%s: dimension is %d, not 2 or 3",
                  me, nrrd->dim); 
    return AIR_FALSE;
  }
  return ret;
}

int
_nrrdFormatEPS_contentStartsLike(NrrdIoState *nio) {

  AIR_UNUSED(nio);
  /* this is a write-only format */
  return AIR_FALSE;
}

int
_nrrdFormatEPS_read(FILE *file, Nrrd *nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdFormatEPS_read";

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  biffAddf(NRRD, "%s: sorry, this is a write-only format", me);
  return 1;
}

int
_nrrdFormatEPS_write(FILE *file, const Nrrd *_nrrd, NrrdIoState *nio) {
  static const char me[]="_nrrdFormatEPS_write";
  int color, cmyk, sx, sy;
  Nrrd *nrrd;
  double aspect, minX, minY, maxX, maxY, scale;
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
  color = (3 == nrrd->dim) && (3 == nrrd->axis[0].size 
                               || 4 == nrrd->axis[0].size);
  cmyk = color && 4 == nrrd->axis[0].size;
  if (color) {
    sx = AIR_CAST(int, nrrd->axis[1].size);
    sy = AIR_CAST(int, nrrd->axis[2].size);
  } else {
    sx = AIR_CAST(int, nrrd->axis[0].size);
    sy = AIR_CAST(int, nrrd->axis[1].size);
  }
  aspect = AIR_CAST(double, sx)/sy;
  if (aspect > 7.5/10) {
    /* image has a wider aspect ratio than safely printable page area */
    minX = 0.5;
    maxX = 8.0;
    minY = 5.50 - 7.5*sy/sx/2;
    maxY = 5.50 + 7.5*sy/sx/2;
    scale = 7.5/sx;
  } else {
    /* image is taller ... */
    minX = 4.25 - 10.0*sx/sy/2;
    maxX = 4.25 + 10.0*sx/sy/2;
    minY = 0.5;
    maxY = 10.5;
    scale = 10.0/sy;
  }
  minX *= 72; minY *= 72;
  maxX *= 72; maxY *= 72;
  scale *= 72;

  fprintf(file, "%%!PS-Adobe-3.0 EPSF-3.0\n");
  fprintf(file, "%%%%Creator: Nrrd Utilities From the "
          "Great Nation of Deseret\n");
  fprintf(file, "%%%%Title: %s\n", 
          nrrd->content ? nrrd->content : "A lovely little image");
  fprintf(file, "%%%%Pages: 1\n");
  fprintf(file, "%%%%BoundingBox: %d %d %d %d\n",
          (int)floor(minX), (int)floor(minY),
          (int)ceil(maxX), (int)ceil(maxY));
  fprintf(file, "%%%%HiResBoundingBox: %g %g %g %g\n", 
          minX, minY, maxX, maxY);
  fprintf(file, "%%%%EndComments\n");
  fprintf(file, "%%%%BeginProlog\n");
  fprintf(file, "%% linestr creates an empty string to hold "
          "one scanline\n");
  fprintf(file, "/linestr %d string def\n", sx*(color 
                                                ? (cmyk 
                                                   ? 4
                                                   : 3)
                                                : 1));
  fprintf(file, "%%%%EndProlog\n");
  fprintf(file, "%%%%Page: 1 1\n");
  fprintf(file, "gsave\n");
  fprintf(file, "%g %g moveto\n", minX, minY);
  fprintf(file, "%g %g lineto\n", maxX, minY);
  fprintf(file, "%g %g lineto\n", maxX, maxY);
  fprintf(file, "%g %g lineto\n", minX, maxY);
  fprintf(file, "closepath\n");
  fprintf(file, "clip\n");
  fprintf(file, "gsave newpath\n");
  fprintf(file, "%g %g translate\n", minX, minY);
  fprintf(file, "%g %g scale\n", sx*scale, sy*scale);
  fprintf(file, "%d %d 8\n", sx, sy);
  fprintf(file, "[%d 0 0 -%d 0 %d]\n", sx, sy, sy);
  if (color) {
    fprintf(file, "{currentfile linestr readhexstring pop} "
            "false %d colorimage\n", cmyk ? 4 : 3);
  } else {
    fprintf(file, "{currentfile linestr readhexstring pop} image\n");
  }
  nrrdEncodingHex->write(file, nrrd->data, nrrdElementNumber(nrrd),
                         nrrd, nio);
  fprintf(file, "\n");
  fprintf(file, "grestore\n");
  fprintf(file, "grestore\n");
  
  airMopError(mop); 
  return 0;
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

/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2013, 2012, 2011, 2010, 2009  University of Chicago
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

/*
** _nrrdReadNrrdParseField()
**
** This is for parsing the stuff BEFORE the colon
*/
int
_nrrdReadNrrdParseField(NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParseField";
  char *next, *buff, *colon, *keysep;
  int ret, fld=nrrdField_unknown, noField, badField=AIR_FALSE;

  next = nio->line + nio->pos;

  /* determining if the line is a comment is simple */
  if (NRRD_COMMENT_CHAR == next[0]) {
    return nrrdField_comment;
  }

  if (!( buff = airStrdup(next) )) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't allocate buffer!", me);
    return nrrdField_unknown;
  }

  /* #1: "...if you see a colon, then look for an equal sign..." */

  /* Look for colon: if no colon, or failed to parse as a field, look for
   * equal sign, if that failed then error */

  /* Let the separator be := */
  /* Escape \n */

  colon = strstr(buff, ": ");
  noField = !colon;
  if (colon) {
    *colon = '\0';
    badField = ( nrrdField_unknown == (fld = airEnumVal(nrrdField, buff)) );
  }
  if (noField || badField) {
    keysep = strstr(buff, ":=");
    if (!keysep) {
      if (noField) {
        biffMaybeAddf(useBiff, NRRD,
                      "%s: didn't see \": \" or \":=\" in line",
                      me);
      } else {
        biffMaybeAddf(useBiff, NRRD,
                      "%s: failed to parse \"%s\" as field identifier",
                      me, buff);
      }
      free(buff); return nrrdField_unknown;
    }

    free(buff);
    ret = nrrdField_keyvalue;
  } else {

    /* *colon = '\0'; */
    /* else we successfully parsed a field identifier */
    next += strlen(buff) + 2;
    free(buff);

    /* skip whitespace prior to start of first field descriptor */
    next += strspn(next, _nrrdFieldSep);
    nio->pos = AIR_CAST(int, next - nio->line);

    ret = fld;
  }
  return ret;
}

/*
** NOTE: it is a common but unfortunate property of these parsers that
** they set values in the nrrd first, and then check their validity
** later.  The reason for this is mostly the desire to centralize
** validity checking in one place, and right now that's in the
** _nrrdFieldCheck[] array of checkers
*/

static int
_nrrdReadNrrdParse_nonfield(FILE *file, Nrrd *nrrd,
                            NrrdIoState *nio, int useBiff) {
  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  AIR_UNUSED(useBiff);
  /*
  char c;

  c= 10; write(2,&c,1); c= 69; write(2,&c,1); c=108; write(2,&c,1);
  c= 32; write(2,&c,1); c= 67; write(2,&c,1); c=104; write(2,&c,1);
  c=101; write(2,&c,1); c= 32; write(2,&c,1); c= 86; write(2,&c,1);
  c=105; write(2,&c,1); c=118; write(2,&c,1); c=101; write(2,&c,1);
  c= 33; write(2,&c,1); c= 10; write(2,&c,1); c= 10; write(2,&c,1);
  */
  return 0;
}

static int
_nrrdReadNrrdParse_comment(FILE *file, Nrrd *nrrd,
                           NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_comment";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  /* this skips the '#' at nio->line[nio->pos] and any other ' ' and '#' */
  if (nrrdCommentAdd(nrrd, info)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble adding comment", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_content(FILE *file, Nrrd *nrrd,
                           NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_content";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  if (strlen(info) && !(nrrd->content = airStrdup(info))) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't strdup() content", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_number(FILE *file, Nrrd *nrrd,
                          NrrdIoState *nio, int useBiff) {
  /*
  static const char me[]="_nrrdReadNrrdParse_number";
  char *info;

  info = nio->line + nio->pos;
  if (1 != sscanf(info, NRRD_BIG_INT_PRINTF, &(nrrd->num))) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: couldn't parse number \"%s\"", me, info); return 1;
  }
  */

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  AIR_UNUSED(useBiff);
  /* It was decided to just completely ignore this field.  "number" is
  ** entirely redundant with the (required) sizes field, and there is no
  ** need to save it to, or learn it from, the header.  In fact the "num"
  ** field was eliminated from the Nrrd struct some time ago, in favor of
  ** the nrrdElementNumber() function.  It may seem odd or unfortunate that
  **
  **   number: Hank Hill sells propane and propane accessories
  **
  ** is a valid field specification, but at least Peggy is proud ...
  */

  return 0;
}

static int
_nrrdReadNrrdParse_type(FILE *file, Nrrd *nrrd,
                        NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_type";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  if (!(nrrd->type = airEnumVal(nrrdType, info))) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse type \"%s\"", me, info);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_type](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

#define _PARSE_ONE_VAL(FIELD, CONV, TYPE)                         \
  if (1 != sscanf(info, CONV, &(FIELD))) {                        \
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse " TYPE       \
                  " from \"%s\"", me, info);                      \
    return 1;                                                     \
  }

static int
_nrrdReadNrrdParse_block_size(FILE *file, Nrrd *nrrd,
                              NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_block_size";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  if (1 != airSingleSscanf(info, "%z", &(nrrd->blockSize))) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse size_t"
                  " from \"%s\"", me, info);
  }
  /* because blockSize and type fields may appear in any order,
     we can't use _nrrdFieldCheck[] */
  return 0;
}

static int
_nrrdReadNrrdParse_dimension(FILE *file, Nrrd *nrrd,
                             NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_dimension";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  _PARSE_ONE_VAL(nrrd->dim, "%u", "unsigned int");
  if (_nrrdFieldCheck[nrrdField_dimension](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

/*
** checking nrrd->dim against zero is valid because it is initialized
** to zero, and, _nrrdReadNrrdParse_dimension() won't allow it to be
** set to anything outside the range [1, NRRD_DIM_MAX]
*/
#define _CHECK_HAVE_DIM                                           \
  if (0 == nrrd->dim) {                                           \
    biffMaybeAddf(useBiff, NRRD,                                  \
                  "%s: don't yet have a valid dimension", me);    \
    return 1;                                                     \
  }

#define _CHECK_HAVE_SPACE_DIM                                           \
  if (0 == nrrd->spaceDim) {                                            \
    biffMaybeAddf(useBiff, NRRD,                                        \
                  "%s: don't yet have a valid space dimension", me);    \
    return 1;                                                           \
  }

#define _CHECK_GOT_ALL_VALUES                                     \
  if (nrrd->dim != ret) {                                         \
    biffMaybeAddf(useBiff, NRRD,                                  \
                  "%s: parsed %d values, but dimension is %d",    \
                  me, ret, nrrd->dim);                            \
    return 1;                                                     \
  }

static int
_nrrdReadNrrdParse_sizes(FILE *file, Nrrd *nrrd,
                         NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_sizes";
  unsigned int ret;
  size_t val[NRRD_DIM_MAX];
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  _CHECK_HAVE_DIM;
  ret = airParseStrZ(val, info, _nrrdFieldSep, nrrd->dim);
  _CHECK_GOT_ALL_VALUES;
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoSize, val);
  /* HEY: this is a very imperfect check of excess info */
  if (nrrd->dim+1 == airParseStrZ(val, info, _nrrdFieldSep, nrrd->dim+1)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d sizes",
                  me, nrrd->dim);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_sizes](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_spacings(FILE *file, Nrrd *nrrd,
                            NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_spacings";
  unsigned int ret;
  double val[NRRD_DIM_MAX];
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  _CHECK_HAVE_DIM;
  ret = airParseStrD(val, info, _nrrdFieldSep, nrrd->dim);
  _CHECK_GOT_ALL_VALUES;
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoSpacing, val);
  /* HEY: this is a very imperfect check of excess info */
  if (nrrd->dim+1 == airParseStrD(val, info, _nrrdFieldSep, nrrd->dim+1)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d spacings",
                  me, nrrd->dim);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_spacings](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_thicknesses(FILE *file, Nrrd *nrrd,
                               NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_thicknesses";
  unsigned int ret;
  double val[NRRD_DIM_MAX];
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  _CHECK_HAVE_DIM;
  ret = airParseStrD(val, info, _nrrdFieldSep, nrrd->dim);
  _CHECK_GOT_ALL_VALUES;
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoThickness, val);
  /* HEY: this is a very imperfect check of excess info */
  if (nrrd->dim+1 == airParseStrD(val, info, _nrrdFieldSep, nrrd->dim+1)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d thicknesses",
                  me, nrrd->dim);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_thicknesses](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_axis_mins(FILE *file, Nrrd *nrrd,
                             NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_axis_mins";
  unsigned int ret;
  double val[NRRD_DIM_MAX];
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  _CHECK_HAVE_DIM;
  ret = airParseStrD(val, info, _nrrdFieldSep, nrrd->dim);
  _CHECK_GOT_ALL_VALUES;
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoMin, val);
  /* HEY: this is a very imperfect check of excess info */
  if (nrrd->dim+1 == airParseStrD(val, info, _nrrdFieldSep, nrrd->dim+1)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d axis mins",
                  me, nrrd->dim);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_axis_mins](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_axis_maxs(FILE *file, Nrrd *nrrd,
                             NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_axis_maxs";
  unsigned int ret;
  double val[NRRD_DIM_MAX];
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  _CHECK_HAVE_DIM;
  ret = airParseStrD(val, info, _nrrdFieldSep, nrrd->dim);
  _CHECK_GOT_ALL_VALUES;
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoMax, val);
  /* HEY: this is a very imperfect check of excess info */
  if (nrrd->dim+1 == airParseStrD(val, info, _nrrdFieldSep, nrrd->dim+1)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d axis maxs",
                  me, nrrd->dim);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_axis_maxs](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

int
_nrrdSpaceVectorParse(double val[NRRD_SPACE_DIM_MAX],
                      char **hhP, unsigned int spaceDim, int useBiff) {
  static const char me[]="_nrrdSpaceVectorParse";
  char *hh, *buff, sep[]=",)";
  airArray *mop;
  unsigned int ret, dd;
  size_t length;

  mop = airMopNew();

  hh = *hhP;
  /* skip past space */
  length = strspn(hh, _nrrdFieldSep);
  hh += length;

  /* make sure we have something */
  if (!*hh) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: hit end of string before seeing (", me);
    airMopError(mop); return 1;
  }
  /* first, see if we're getting the non-vector */
  if ( (strstr(hh, _nrrdNoSpaceVector) == hh) ) {
    if (!hh[strlen(_nrrdNoSpaceVector)]
        || strchr(_nrrdFieldSep, hh[strlen(_nrrdNoSpaceVector)])) {
      /* yes, we got the non-vector */
      for (dd=0; dd<spaceDim; dd++) {
        val[dd] = AIR_NAN;
      }
      length += strlen(_nrrdNoSpaceVector);
    } else {
      /* we got something that started out looking like the non-vector */
      biffMaybeAddf(useBiff, NRRD,
                    "%s: couldn't parse non-vector \"%s\"", me, hh);
      airMopError(mop); return 1;
    }
  } else {
    /* this isn't a non-vector */
    /* make sure we have an open paren */
    if ('(' != *hh) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: first vector in \"%s\" didn't start with '('",
                    me, hh);
      airMopError(mop); return 1;
    }
    /* copy string (including open paren) for local fiddling */
    if (!(buff = airStrdup(hh))) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: couldn't allocate local buffer", me);
      airMopError(mop); return 1;
    }
    airMopAdd(mop, buff, airFree, airMopAlways);
    /* scan for close paren */
    hh = buff+1;
    while (*hh) {
      if (')' == *hh) {
        break;
      } else {
        hh++;
      }
    }
    if (')' != *hh) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: didn't see ')' at end of first vector in \"%s\"",
                    me, hh);
      airMopError(mop); return 1;
    }
    /* terminate at end paren */
    *(hh+1) = 0;
    length += strlen(buff);
    /* see if we have too many fields */
    ret = airStrntok(buff+1, sep);
    if (ret > spaceDim) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: space dimension is %d, but seem to have %d "
                    "coefficients", me, spaceDim, ret);
      airMopError(mop); return 1;
    }
    /* try to parse the values */
    ret = airParseStrD(val, buff+1, ",", spaceDim);
    if (spaceDim != ret) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: parsed %d values, but space dimension is %d",
                    me, ret, spaceDim);
      airMopError(mop); return 1;
    }
  }
  /* probably not useful */
  for (dd=spaceDim; dd<NRRD_SPACE_DIM_MAX; dd++) {
    val[dd] = AIR_NAN;
  }
  /* make sure all coefficients exist or not together */
  for (dd=1; dd<spaceDim; dd++) {
    if (!!AIR_EXISTS(val[0]) ^ !!AIR_EXISTS(val[dd])) {
      biffMaybeAddf(useBiff, NRRD, "%s: existance of all space vector "
                    "coefficients must be consistent (val[0] not like "
                    "val[%d])", me, dd);
      airMopError(mop); return 1;
    }
  }
  for (dd=0; dd<spaceDim; dd++) {
    if (airIsInf_d(val[dd])) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: vector coefficient %d can't be infinite",
                    me, dd);
      airMopError(mop); return 1;
    }
  }
  *hhP += length;
  airMopOkay(mop);
  return 0;
}

static int
_nrrdReadNrrdParse_space_directions(FILE *file, Nrrd *nrrd,
                                    NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_space_directions";
  unsigned int dd;
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  _CHECK_HAVE_DIM;
  _CHECK_HAVE_SPACE_DIM;

  for (dd=0; dd<nrrd->dim; dd++) {
    if (_nrrdSpaceVectorParse(nrrd->axis[dd].spaceDirection,
                              &info, nrrd->spaceDim, useBiff)) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: trouble getting space vector %d of %d",
                    me, dd+1, nrrd->dim);
      return 1;
    }
  }
  if (strlen(info) != strspn(info, _nrrdFieldSep)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d directions",
                  me, nrrd->dim);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_space_directions](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_centers(FILE *file, Nrrd *nrrd,
                           NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_centers";
  unsigned int ai;
  char *tok, *info, *last;
  airArray *mop;

  AIR_UNUSED(file);
  mop = airMopNew();
  info = airStrdup(nio->line + nio->pos);
  airMopAdd(mop, info, airFree, airMopAlways);
  _CHECK_HAVE_DIM;
  for (ai=0; ai<nrrd->dim; ai++) {
    tok = airStrtok(!ai ? info : NULL, _nrrdFieldSep, &last);
    if (!tok) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: couldn't extract string for center %d of %d",
                    me, ai+1, nrrd->dim);
      airMopError(mop); return 1;
    }
    if (!strcmp(tok, NRRD_UNKNOWN)) {
      nrrd->axis[ai].center = nrrdCenterUnknown;
      continue;
    }
    if (!strcmp(tok, NRRD_NONE)) {
      nrrd->axis[ai].center = nrrdCenterUnknown;
      continue;
    }
    if (!(nrrd->axis[ai].center = airEnumVal(nrrdCenter, tok))) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: couldn't parse center \"%s\" for axis %d",
                    me, tok, ai);
      airMopError(mop); return 1;
    }
  }
  if (airStrtok(!ai ? info : NULL, _nrrdFieldSep, &last)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d centers",
                  me, nrrd->dim);
    airMopError(mop); return 1;
  }
  if (_nrrdFieldCheck[nrrdField_centers](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    airMopError(mop); return 1;
  }
  airMopOkay(mop);
  return 0;
}

static int
_nrrdReadNrrdParse_kinds(FILE *file, Nrrd *nrrd,
                         NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_kinds";
  unsigned int ai;
  char *info, *tok, *last;
  airArray *mop;

  AIR_UNUSED(file);
  mop = airMopNew();
  info = airStrdup(nio->line + nio->pos);
  airMopAdd(mop, info, airFree, airMopAlways);
  _CHECK_HAVE_DIM;
  for (ai=0; ai<nrrd->dim; ai++) {
    tok = airStrtok(!ai ? info : NULL, _nrrdFieldSep, &last);
    if (!tok) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: couldn't extract string for kind %d of %d",
                    me, ai+1, nrrd->dim);
      airMopError(mop); return 1;
    }
    if (!strcmp(tok, NRRD_UNKNOWN)) {
      nrrd->axis[ai].kind = nrrdKindUnknown;
      continue;
    }
    if (!strcmp(tok, NRRD_NONE)) {
      nrrd->axis[ai].center = nrrdKindUnknown;
      continue;
    }
    if (!(nrrd->axis[ai].kind = airEnumVal(nrrdKind, tok))) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: couldn't parse \"%s\" kind %d of %d",
                    me, tok, ai+1, nrrd->dim);
      airMopError(mop); return 1;
    }
  }
  if (airStrtok(!ai ? info : NULL, _nrrdFieldSep, &last)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d kinds",
                  me, nrrd->dim);
    airMopError(mop); return 1;
  }
  /* can't run this now because kinds can come before sizes, in which
     case the kind/size check in _nrrdFieldCheck_kinds will incorrectly
     flag an error ...
  if (_nrrdFieldCheck[nrrdField_kinds](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    airMopError(mop); return 1;
    }
  */
  airMopOkay(mop);
  return 0;
}

static char *
_nrrdGetQuotedString(char **hP, int useBiff) {
  static const char me[]="_nrrdGetQuotedString";
  char *h, *buff, *ret;
  airArray *buffArr;
  unsigned int pos;
  airPtrPtrUnion appu;

  h = *hP;
  /* skip past space */
  /* printf("!%s: h |%s|\n", me, h);*/
  h += strspn(h, _nrrdFieldSep);
  /* printf("!%s: h |%s|\n", me, h);*/

  /* make sure we have something */
  if (!*h) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: hit end of string before seeing opening \"", me);
    return NULL;
  }
  /* make sure we have a starting quote */
  if ('"' != *h) {
    biffMaybeAddf(useBiff, NRRD, "%s: didn't start with \"", me);
    return NULL;
  }
  h++;

  /* parse string until end quote */
  buff = NULL;
  appu.c = &buff;
  buffArr = airArrayNew(appu.v, NULL, sizeof(char), 2);
  if (!buffArr) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't create airArray", me);
    return NULL;
  }
  pos = airArrayLenIncr(buffArr, 1);  /* pos should get 0 */
  while (h[pos]) {
    /* printf("!%s: h+%d |%s|\n", me, pos, h+pos); */
    if ('\"' == h[pos]) {
      break;
    }
    if ('\\' == h[pos] && '\"' == h[pos+1]) {
      h += 1;
    }
    buff[pos] = h[pos];
    pos = airArrayLenIncr(buffArr, 1);
  }
  if ('\"' != h[pos]) {
    biffMaybeAddf(useBiff, NRRD, "%s: didn't see ending \" soon enough", me);
    return NULL;
  }
  h += pos + 1;
  buff[pos] = 0;

  ret = airStrdup(buff);
  airArrayNuke(buffArr);
  *hP = h;

  return ret;
}

static int
_nrrdReadNrrdParse_labels(FILE *file, Nrrd *nrrd,
                          NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_labels";
  char *h;  /* this is the "here" pointer which gradually progresses
               through all the labels (for all axes) */
  unsigned int ai;
  char *info;

  AIR_UNUSED(file);
  /* because we have to correctly interpret quote marks, we
     can't simply rely on airParseStrS */
  info = nio->line + nio->pos;
  /* printf("!%s: info |%s|\n", me, info); */
  _CHECK_HAVE_DIM;
  h = info;
  for (ai=0; ai<nrrd->dim; ai++) {
    if (!( nrrd->axis[ai].label = _nrrdGetQuotedString(&h, useBiff) )) {
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't get get label %d of %d\n",
                    me, ai+1, nrrd->dim);
      return 1;
    }
  }
  if (strlen(h) != strspn(h, _nrrdFieldSep)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d labels",
                  me, nrrd->dim);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_labels](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_units(FILE *file, Nrrd *nrrd,
                         NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_units";
  char *h;  /* this is the "here" pointer which gradually progresses
               through all the units (for all axes) */
  unsigned int ai;
  char *info;

  AIR_UNUSED(file);
  /* because we have to correctly interpret quote marks, we
     can't simply rely on airParseStrS */
  info = nio->line + nio->pos;
  /* printf("!%s: info |%s|\n", me, info); */
  _CHECK_HAVE_DIM;
  h = info;
  for (ai=0; ai<nrrd->dim; ai++) {
    if (!( nrrd->axis[ai].units = _nrrdGetQuotedString(&h, useBiff) )) {
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't get get unit %d of %d\n",
                    me, ai+1, nrrd->dim);
      return 1;
    }
  }
  if (strlen(h) != strspn(h, _nrrdFieldSep)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d units",
                  me, nrrd->dim);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_units](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_min(FILE *file, Nrrd *nrrd,
                       NrrdIoState *nio, int useBiff) {

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  AIR_UNUSED(useBiff);

  /* This field is no longer assumed to be anything meaningful,
     because nrrd->min no longer exists with the advent of NrrdRange.
     But, having the field is not an error, to not trip on older
     NRRD00.01 and NRRD0001 files which (legitimately) used it */

  return 0;
}

static int
_nrrdReadNrrdParse_max(FILE *file, Nrrd *nrrd,
                       NrrdIoState *nio, int useBiff) {

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  AIR_UNUSED(useBiff);

  /* nrrd->max no longer exists, see above */

  return 0;
}

static int
_nrrdReadNrrdParse_old_min(FILE *file, Nrrd *nrrd,
                           NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_old_min";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  _PARSE_ONE_VAL(nrrd->oldMin, "%lg", "double");
  if (_nrrdFieldCheck[nrrdField_old_min](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_old_max(FILE *file, Nrrd *nrrd,
                           NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_old_max";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  _PARSE_ONE_VAL(nrrd->oldMax, "%lg", "double");
  if (_nrrdFieldCheck[nrrdField_old_max](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_endian(FILE *file, Nrrd *nrrd,
                          NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_endian";
  char *info;

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  info = nio->line + nio->pos;
  if (!(nio->endian = airEnumVal(airEndian, info))) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: couldn't parse endian \"%s\"", me, info);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_encoding(FILE *file, Nrrd *nrrd,
                            NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_encoding";
  char *info;
  int etype;

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  info = nio->line + nio->pos;
  if (!(etype = airEnumVal(nrrdEncodingType, info))) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: couldn't parse encoding \"%s\"", me, info);
    return 1;
  }

  nio->encoding = nrrdEncodingArray[etype];
  return 0;
}

static int
_nrrdReadNrrdParse_line_skip(FILE *file, Nrrd *nrrd,
                             NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_line_skip";
  char *info;

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  info = nio->line + nio->pos;
  _PARSE_ONE_VAL(nio->lineSkip, "%u", "unsigned int");
  /* now that its unsigned, what error checking can I do?
  if (!(0 <= nio->lineSkip)) {
    biffMaybeAddf(useBiff, NRRD,
    "%s: lineSkip value %d invalid", me, nio->lineSkip);
    return 1;
    }
  */
  return 0;
}

static int
_nrrdReadNrrdParse_byte_skip(FILE *file, Nrrd *nrrd,
                             NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_byte_skip";
  char *info;

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  info = nio->line + nio->pos;
  _PARSE_ONE_VAL(nio->byteSkip, "%ld", "long int");
  /* this check is being removed to enable the undocumented
     (in the file format spec) ability to say "byte skip: -N-1"
     in order to skip backwards from EOF by N bytes
  ** if (!(-1 <= nio->byteSkip)) {
  **   biffMaybeAddf(useBiff, NRRD,
  **                 "%s: byteSkip value %ld invalid", me, nio->byteSkip);
  **   return 1;
  ** }
  */
  return 0;
}

static int
_nrrdReadNrrdParse_keyvalue(FILE *file, Nrrd *nrrd,
                            NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_keyvalue";
  char *keysep, *line, *key, *value;

  AIR_UNUSED(file);
  /* we know this will find something */
  line = airStrdup(nio->line + nio->pos);
  if (!line) {
    biffMaybeAddf(useBiff, NRRD, "%s: can't allocate parse line", me);
    return 1;
  }
  keysep = strstr(line, ":=");
  if (!keysep) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: didn't see \":=\" key/value delimiter in \"%s\"",
                  me, line);
    free(line); return 1;
  }
  keysep[0] = 0;
  keysep[1] = 0;
  key = line;
  value = keysep+2;

  /* convert escape sequences */
  airUnescape(key);
  airUnescape(value);

  nrrdKeyValueAdd(nrrd, key, value);

  free(line);
  return 0;
}

static int
_nrrdReadNrrdParse_sample_units(FILE *file, Nrrd *nrrd,
                                NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_sample_units";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;

  if (strlen(info) && !(nrrd->sampleUnits = airStrdup(info))) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: couldn't strdup() sampleUnits", me);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_sample_units](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_space(FILE *file, Nrrd *nrrd,
                         NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_space";
  char *info;
  int space;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  if (nio->seen[nrrdField_space_dimension]) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: can't specify space after specifying "
                  "space dimension (%d)", me, nrrd->spaceDim);
    return 1;
  }
  if (!(space = airEnumVal(nrrdSpace, info))) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: couldn't parse space \"%s\"", me, info);
    return 1;
  }
  if (nrrdSpaceSet(nrrd, space)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_space](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_space_dimension(FILE *file, Nrrd *nrrd,
                                   NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_space_dimension";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  if (nio->seen[nrrdField_space]) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: can't specify space dimension after specifying "
                  "space (%s)", me, airEnumStr(nrrdSpace, nrrd->space));
    return 1;
  }
  _PARSE_ONE_VAL(nrrd->spaceDim, "%u", "unsigned int");
  if (_nrrdFieldCheck[nrrdField_space_dimension](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_space_units(FILE *file, Nrrd *nrrd,
                               NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_space_units";
  char *h;  /* this is the "here" pointer which gradually progresses
               through all the units (for all axes) */
  unsigned int ai;
  char *info;

  AIR_UNUSED(file);
  /* because we have to correctly interpret quote marks, we
     can't simply rely on airParseStrS */
  info = nio->line + nio->pos;
  /* printf("!%s: info |%s|\n", me, info); */
  _CHECK_HAVE_SPACE_DIM;
  h = info;
  for (ai=0; ai<nrrd->spaceDim; ai++) {
    if (!( nrrd->spaceUnits[ai] = _nrrdGetQuotedString(&h, useBiff) )) {
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't get get space unit %d of %d",
                    me, ai+1, nrrd->spaceDim);
      return 1;
    }
  }
  if (_nrrdGetQuotedString(&h, AIR_FALSE)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seemed to have more than expected %d space units",
                  me, nrrd->spaceDim);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_space_units](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_space_origin(FILE *file, Nrrd *nrrd,
                                NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_space_origin";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;

  _CHECK_HAVE_SPACE_DIM;

  if (_nrrdSpaceVectorParse(nrrd->spaceOrigin, &info,
                            nrrd->spaceDim, useBiff)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: couldn't parse origin \"%s\"", me, info);
    return 1;
  }
  if (_nrrdFieldCheck[nrrdField_space_origin](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
_nrrdReadNrrdParse_measurement_frame(FILE *file, Nrrd *nrrd,
                                     NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_measurement_frame";
  double colvec[NRRD_SPACE_DIM_MAX];
  unsigned int dd, ii;
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;

  _CHECK_HAVE_SPACE_DIM;

  for (dd=0; dd<nrrd->spaceDim; dd++) {
    /* we are going through the *columns* of the mf matrix */
    if (_nrrdSpaceVectorParse(colvec, &info, nrrd->spaceDim, useBiff)) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: trouble getting space vector %d of %d",
                    me, dd+1, nrrd->spaceDim);
      return 1;
    }
    for (ii=0; ii<NRRD_SPACE_DIM_MAX; ii++) {
      nrrd->measurementFrame[dd][ii] = (ii < nrrd->spaceDim
                                        ? colvec[ii]
                                        : AIR_NAN);
    }
  }
  if (strlen(info) != strspn(info, _nrrdFieldSep)) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: seem to have more than expected %d directions",
                  me, nrrd->spaceDim);
    return 1;
  }
  for (dd=nrrd->spaceDim; dd<NRRD_SPACE_DIM_MAX; dd++) {
    for (ii=0; ii<NRRD_SPACE_DIM_MAX; ii++) {
      nrrd->measurementFrame[dd][ii] = AIR_NAN;
    }
  }
  if (_nrrdFieldCheck[nrrdField_measurement_frame](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

int
_nrrdContainsPercentThisAndMore(const char *str, char thss) {
  const char *hh, *tmp;

  tmp = str;
  do {
    hh = strchr(tmp, '%');
    if (!( hh && hh[1] )) {
      return 0;
    }
    if ('%' == hh[1]) {
      /* its an escaped % */
      tmp = hh + 2;
    } else {
      break;
    }
  } while (tmp[0]);
  hh++;
  hh += strspn(hh, "0123456789");
  if (!( hh[0] == thss )) {
    return 0;
  }
  hh += strcspn(hh, _nrrdFieldSep);
  return !!hh;
}

unsigned int
_nrrdDataFNNumber(NrrdIoState *nio) {
  unsigned int ret;
  int ii;

  if (nio->dataFNFormat) {
    /* datafiles given in iterator form; count number of values */
    ret = 0;
    for (ii = nio->dataFNMin;
         ((nio->dataFNStep > 0 && ii <= nio->dataFNMax)
          || (nio->dataFNStep < 0 && ii >= nio->dataFNMax));
         ii += nio->dataFNStep) {
      ret += 1;
    }
  } else if (nio->dataFNArr->len) {
    /* datafiles given as an explicit list, or as a single file name,
       and in either case, nrrdDataFNAdd() is used to add them to
       the dataFNArr */
    ret = nio->dataFNArr->len;
  } else {
    /* datafile is same as (attached) header file */
    ret = 1;
  }
  return ret;
}

/*
** this always requires that the per-axis size fields have been set
*/
int
_nrrdDataFNCheck(NrrdIoState *nio, Nrrd *nrrd, int useBiff) {
  static const char me[]="_nrrdDataFNCheck";
  size_t pieceSize, pieceNum;
  char stmp[AIR_STRLEN_SMALL];

  if (!nio->seen[nrrdField_sizes]) {
    biffMaybeAddf(useBiff, NRRD, "%s: sorry, currently can't handle "
                  "multiple detached data files without first knowing "
                  "the \"%s\" field",
                  me, airEnumStr(nrrdField, nrrdField_sizes));
    return 1;
  }
  if (nio->dataFileDim < nrrd->dim) {
    /* this requires that the per-axis size fields have been set */
    _nrrdSplitSizes(&pieceSize, &pieceNum, nrrd, nio->dataFileDim);
    if (pieceNum != _nrrdDataFNNumber(nio)) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: expected %s filenames (of %u-D pieces) "
                    "but got %u", me,
                    airSprintSize_t(stmp, pieceNum), nio->dataFileDim,
                    _nrrdDataFNNumber(nio));
      return 1;
    }
  } else {
    /* we're getting data in "slabs" with the same dimension as the
       nrrd, so for simplicity we assume that they're all equal size */
    if (_nrrdDataFNNumber(nio) > nrrd->axis[nrrd->dim-1].size) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: can't have more pieces (%u) than axis %u "
                    "slices (%s) when nrrd dimension and "
                    "datafile dimension are both %u", me,
                    _nrrdDataFNNumber(nio),
                    nrrd->dim-1,
                    airSprintSize_t(stmp, nrrd->axis[nrrd->dim-1].size),
                    nrrd->dim);
      return 1;
    }
    if ((double)nrrd->axis[nrrd->dim-1].size/_nrrdDataFNNumber(nio)
        != nrrd->axis[nrrd->dim-1].size/_nrrdDataFNNumber(nio)) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: number of datafiles (%d) doesn't divide into "
                    "number of axis %u slices (%s)", me,
                    (int)_nrrdDataFNNumber(nio), nrrd->dim-1,
                    airSprintSize_t(stmp, nrrd->axis[nrrd->dim-1].size));
      return 1;
    }
  }
  return 0;
}

/*
** Sat Jan 29 16:44:50 EST 2005: this used to "open the separate
** datafile, and set the FILE* in nio->dataFile, which otherwise will
** stay NULL", but now we support multiple detached data files.  So.
**
** The job of this function is to map the "data file" specification to
** one or more filenames that can be passed direction to fopen for
** reading in the data.  This involves parsing the various formats for
** identifying multiple data files, and possibly prefixing them with
** nio->path.
*/
static int
_nrrdReadNrrdParse_data_file(FILE *ffile, Nrrd *nrrd,
                             NrrdIoState *nio, int useBiff) {
  static const char me[]="_nrrdReadNrrdParse_data_file";
  char *info, *nums;
  unsigned int linelen, tmp;
  airArray *mop;

  mop = airMopNew();
  info = airStrdup(nio->line + nio->pos);
  if (!info) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't copy line!", me);
    return 1;
  }
  airMopAdd(mop, info, airFree, airMopAlways);

  /* HEY: this change should be made someday
  if (_nrrdContainsPercentThisAndMore(info, 'd')
      || _nrrdContainsPercentThisAndMore(info, 'u')) { */
  if (_nrrdContainsPercentThisAndMore(info, 'd')) {
    /* ---------------------------------------------------------- */
    /* --------- format.%d <min> <max> <step> [<dim>] ----------- */
    /* ---------------------------------------------------------- */
    size_t sspn;
    _CHECK_HAVE_DIM;
    nums = info + strcspn(info, _nrrdFieldSep);
    sspn = strspn(nums, _nrrdFieldSep);
    nums[0] = 0;   /* terminate so that format is now in info */
    nums += sspn;
    if (!( 3 == sscanf(nums, "%d %d %d",&(nio->dataFNMin),
                       &(nio->dataFNMax), &(nio->dataFNStep)) )) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: couldn't parse three ints (min, max, step) after "
                    "data filename template", me);
      airMopError(mop); return 1;
    }
    if ( 4 == sscanf(nums, "%d %d %d %u", &(nio->dataFNMin),
                     &(nio->dataFNMax), &(nio->dataFNStep),
                     &(nio->dataFileDim)) ) {
      if (!AIR_IN_CL(1, nio->dataFileDim, nrrd->dim)) {
        biffMaybeAddf(useBiff, NRRD,
                      "%s: datafile dimension %u outside valid range [1,%u]",
                      me, nio->dataFileDim, nrrd->dim);
        airMopError(mop); return 1;
      }
    } else {
      nio->dataFileDim = nrrd->dim-1;
    }
    if (0 == nio->dataFNStep) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: file number step must be non-zero", me);
      airMopError(mop); return 1;
    }
    if ((nio->dataFNMax - nio->dataFNMin)*(nio->dataFNStep) < 0) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: file number max %d not approached from min %d "
                    "by step %d", me,
                    nio->dataFNMax, nio->dataFNMin, nio->dataFNStep);
      airMopError(mop); return 1;
    }
    if (!( nio->dataFNFormat = airStrdup(info) )) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: couldn't copy data filename format", me);
      airMopError(mop); return 1;
    }
    if (_nrrdDataFNCheck(nio, nrrd, useBiff)) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: trouble with number of datafiles", me);
      airMopError(mop); return 1;
    }
  } else if (!strncmp(info, NRRD_LIST_FLAG, strlen(NRRD_LIST_FLAG))) {
    /* ---------------------------------------------------------- */
    /* ------------------------- LIST --------------------------- */
    /* ---------------------------------------------------------- */
    _CHECK_HAVE_DIM;
    if (_nrrdHeaderCheck(nrrd, nio, AIR_TRUE)) {
      biffMaybeAddf(useBiff, NRRD, "%s: NRRD header is incomplete. \""
                    NRRD_LIST_FLAG "\" data file specification must be "
                    "contiguous with end of header!", me);
      airMopError(mop); return 1;
    }
    info += strlen(NRRD_LIST_FLAG);
    if (info[0]) {
      if (1 == sscanf(info, "%u", &(nio->dataFileDim))) {
        if (!AIR_IN_CL(1, nio->dataFileDim, nrrd->dim)) {
          biffMaybeAddf(useBiff, NRRD, "%s: datafile dimension %u outside "
                        "valid range [1,%u]",
                        me, nio->dataFileDim, nrrd->dim);
          airMopError(mop); return 1;
        }
      } else {
        biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse info after \""
                      NRRD_LIST_FLAG "\" as an int", me);
        airMopError(mop); return 1;
      }
    } else {
      /* nothing after NRRD_LIST_FLAG, so dataFileDim is implicit */
      nio->dataFileDim = nrrd->dim-1;
    }
    /* read in all the datafile names */
    do {
      /* yes, nio->line is re-used/over-written here, but I don't
         think that's a problem */
      if (_nrrdOneLine(&linelen, nio, ffile)) {
        biffMaybeAddf(useBiff, NRRD,
                      "%s: trouble getting file name line", me);
        airMopError(mop); return 1;
      }
      if (linelen > 0) {
        tmp = airArrayLenIncr(nio->dataFNArr, 1);
        nio->dataFN[tmp] = airStrdup(nio->line);
      }
    } while (linelen > 0);
    if (_nrrdDataFNCheck(nio, nrrd, useBiff)) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: trouble with number of datafiles", me);
      airMopError(mop); return 1;
    }
  } else {
    /* ---------------------------------------------------------- */
    /* -------------------- (single filename) ------------------- */
    /* ---------------------------------------------------------- */
    /* there is apparently only a single detached data file; for
       this its okay to not yet know nrrd->dim */
    tmp = airArrayLenIncr(nio->dataFNArr, 1);
    nio->dataFN[tmp] = airStrdup(info);
    nio->dataFileDim = 0;
  }
  airMopOkay(mop);
  return 0;
}

/*
******** nrrdFieldInfoParse[NRRD_FIELD_MAX+1]()
**
** These are all for parsing the stuff AFTER the colon
*/
int
(*nrrdFieldInfoParse[NRRD_FIELD_MAX+1])(FILE *, Nrrd *,
                                        NrrdIoState *, int) = {
  _nrrdReadNrrdParse_nonfield,
  _nrrdReadNrrdParse_comment,
  _nrrdReadNrrdParse_content,
  _nrrdReadNrrdParse_number,
  _nrrdReadNrrdParse_type,
  _nrrdReadNrrdParse_block_size,
  _nrrdReadNrrdParse_dimension,
  _nrrdReadNrrdParse_space,
  _nrrdReadNrrdParse_space_dimension,
  _nrrdReadNrrdParse_sizes,
  _nrrdReadNrrdParse_spacings,
  _nrrdReadNrrdParse_thicknesses,
  _nrrdReadNrrdParse_axis_mins,
  _nrrdReadNrrdParse_axis_maxs,
  _nrrdReadNrrdParse_space_directions,
  _nrrdReadNrrdParse_centers,
  _nrrdReadNrrdParse_kinds,
  _nrrdReadNrrdParse_labels,
  _nrrdReadNrrdParse_units,
  _nrrdReadNrrdParse_min,
  _nrrdReadNrrdParse_max,
  _nrrdReadNrrdParse_old_min,
  _nrrdReadNrrdParse_old_max,
  _nrrdReadNrrdParse_endian,
  _nrrdReadNrrdParse_encoding,
  _nrrdReadNrrdParse_line_skip,
  _nrrdReadNrrdParse_byte_skip,
  _nrrdReadNrrdParse_keyvalue,
  _nrrdReadNrrdParse_sample_units,
  _nrrdReadNrrdParse_space_units,
  _nrrdReadNrrdParse_space_origin,
  _nrrdReadNrrdParse_measurement_frame,
  _nrrdReadNrrdParse_data_file
};

/* kernel parsing is all in kernel.c */


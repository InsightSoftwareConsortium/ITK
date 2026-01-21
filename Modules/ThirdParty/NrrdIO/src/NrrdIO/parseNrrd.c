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

/*
** nrrd__ReadNrrdParseField()
**
** This is for parsing the stuff BEFORE the colon
*/
int /* Biff: (private) maybe:2:nrrdField_unknown */
nrrd__ReadNrrdParseField(NrrdIoState *nio, int useBiff) {
  static const char me[] = "nrrd__ReadNrrdParseField";
  char *next, *buff, *colon, *keysep;
  int ret, fld = nrrdField_unknown, noField, badField = AIR_FALSE;

  next = nio->line + nio->pos;

  /* determining if the line is a comment is simple */
  if (NRRD_COMMENT_CHAR == next[0]) {
    return nrrdField_comment;
  }

  if (!(buff = airStrdup(next))) {
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
    badField = (nrrdField_unknown == (fld = airEnumVal(nrrdField, buff)));
  }
  if (noField || badField) {
    keysep = strstr(buff, ":=");
    if (!keysep) {
      if (noField) {
        biffMaybeAddf(useBiff, NRRD, "%s: didn't see \": \" or \":=\" in line", me);
      } else {
        biffMaybeAddf(useBiff, NRRD, "%s: failed to parse \"%s\" as field identifier",
                      me, buff);
      }
      free(buff);
      return nrrdField_unknown;
    }

    free(buff);
    ret = nrrdField_keyvalue;
  } else {

    /* *colon = '\0'; */
    /* else we successfully parsed a field identifier */
    next += strlen(buff) + 2;
    free(buff);

    /* skip whitespace prior to start of first field descriptor */
    next += strspn(next, nrrd__FieldSep);
    nio->pos = AIR_INT(next - nio->line);

    ret = fld;
  }
  return ret;
}

/*
** NOTE: it is a common but unfortunate property of these parsers that
** they set values in the nrrd first, and then check their validity
** later.  The reason for this is mostly the desire to centralize
** validity checking in one place, and right now that's in the
** nrrd__FieldCheck[] array of checkers
*/

static int
rnParse_nonfield(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
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

static int /* Biff: maybe:4:1 */
rnParse_comment(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_comment";
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

static int /* Biff: maybe:4:1 */
rnParse_content(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_content";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  if (strlen(info) && !(nrrd->content = airStrdup(info))) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't strdup() content", me);
    return 1;
  }
  return 0;
}

static int /* Biff: nope # unlike other parsers, for reasons described below */
rnParse_number(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  /*
   *static const char me[] = "rnParse_number";
   *char *info;
   *
   *info = nio->line + nio->pos;
   *if (1 != sscanf(info, NRRD_BIG_INT_PRINTF, &(nrrd->num))) {
   *  biffMaybeAddf(useBiff, NRRD,
   *                "%s: couldn't parse number \"%s\"", me, info); return 1;
   *}
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

static int /* Biff: maybe:4:1 */
rnParse_type(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_type";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  if (!(nrrd->type = airEnumVal(nrrdType, info))) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse type \"%s\"", me, info);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_type](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

#define PARSE_ONE_VAL(FIELD, CONV, TYPE)                                                \
  if (1 != airSingleSscanf(info, CONV, &(FIELD))) {                                     \
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse " TYPE " from \"%s\"", me, info);  \
    return 1;                                                                           \
  }

static int /* Biff: maybe:4:1 */
rnParse_block_size(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_block_size";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  if (1 != airSingleSscanf(info, "%zu", &(nrrd->blockSize))) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: couldn't parse size_t"
                  " from \"%s\"",
                  me, info);
    /* For >15 years this return was missing; highlighting that it was missing was thanks
     * to the biff auto-scan in teem/src/_util/scan-symbols.py */
    return 1;
  }
  /* because blockSize and type fields may appear in any order,
     we can't use nrrd__FieldCheck[] */
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_dimension(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_dimension";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  PARSE_ONE_VAL(nrrd->dim, "%u", "unsigned int");
  if (nrrd__FieldCheck[nrrdField_dimension](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

/*
** checking nrrd->dim against zero is valid because it is initialized
** to zero, and, rnParse_dimension() won't allow it to be
** set to anything outside the range [1, NRRD_DIM_MAX]
*/
#define CHECK_HAVE_DIM                                                                  \
  if (0 == nrrd->dim) {                                                                 \
    biffMaybeAddf(useBiff, NRRD, "%s: don't yet have a valid dimension", me);           \
    return 1;                                                                           \
  }

#define CHECK_HAVE_SPACE_DIM                                                            \
  if (0 == nrrd->spaceDim) {                                                            \
    biffMaybeAddf(useBiff, NRRD, "%s: don't yet have a valid space dimension", me);     \
    return 1;                                                                           \
  }

#define CHECK_GOT_ALL_VALUES                                                            \
  if (nrrd->dim != ret) {                                                               \
    biffMaybeAddf(useBiff, NRRD, "%s: parsed %u values, but dimension is %u", me, ret,  \
                  nrrd->dim);                                                           \
    return 1;                                                                           \
  }

static int /* Biff: maybe:4:1 */
rnParse_sizes(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_sizes";
  unsigned int ret;
  size_t val[NRRD_DIM_MAX];
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  CHECK_HAVE_DIM;
  ret = airParseStrZ(val, info, nrrd__FieldSep, nrrd->dim);
  CHECK_GOT_ALL_VALUES;
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoSize, val);
  /* HEY: this is a very imperfect check of excess info */
  if (nrrd->dim + 1 == airParseStrZ(val, info, nrrd__FieldSep, nrrd->dim + 1)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u sizes", me,
                  nrrd->dim);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_sizes](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_spacings(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_spacings";
  unsigned int ret;
  double val[NRRD_DIM_MAX];
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  CHECK_HAVE_DIM;
  ret = airParseStrD(val, info, nrrd__FieldSep, nrrd->dim);
  CHECK_GOT_ALL_VALUES;
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoSpacing, val);
  /* HEY: this is a very imperfect check of excess info */
  if (nrrd->dim + 1 == airParseStrD(val, info, nrrd__FieldSep, nrrd->dim + 1)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u spacings", me,
                  nrrd->dim);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_spacings](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_thicknesses(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_thicknesses";
  unsigned int ret;
  double val[NRRD_DIM_MAX];
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  CHECK_HAVE_DIM;
  ret = airParseStrD(val, info, nrrd__FieldSep, nrrd->dim);
  CHECK_GOT_ALL_VALUES;
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoThickness, val);
  /* HEY: this is a very imperfect check of excess info */
  if (nrrd->dim + 1 == airParseStrD(val, info, nrrd__FieldSep, nrrd->dim + 1)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u thicknesses",
                  me, nrrd->dim);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_thicknesses](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_axis_mins(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_axis_mins";
  unsigned int ret;
  double val[NRRD_DIM_MAX];
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  CHECK_HAVE_DIM;
  ret = airParseStrD(val, info, nrrd__FieldSep, nrrd->dim);
  CHECK_GOT_ALL_VALUES;
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoMin, val);
  /* HEY: this is a very imperfect check of excess info */
  if (nrrd->dim + 1 == airParseStrD(val, info, nrrd__FieldSep, nrrd->dim + 1)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u axis mins", me,
                  nrrd->dim);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_axis_mins](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_axis_maxs(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_axis_maxs";
  unsigned int ret;
  double val[NRRD_DIM_MAX];
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  CHECK_HAVE_DIM;
  ret = airParseStrD(val, info, nrrd__FieldSep, nrrd->dim);
  CHECK_GOT_ALL_VALUES;
  nrrdAxisInfoSet_nva(nrrd, nrrdAxisInfoMax, val);
  /* HEY: this is a very imperfect check of excess info */
  if (nrrd->dim + 1 == airParseStrD(val, info, nrrd__FieldSep, nrrd->dim + 1)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u axis maxs", me,
                  nrrd->dim);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_axis_maxs](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
spaceVectorParse(double val[NRRD_SPACE_DIM_MAX], char **hhP, unsigned int spaceDim,
                 int useBiff) {
  static const char me[] = "spaceVectorParse";
  char *hh, *buff, sep[] = ",)";
  airArray *mop;
  unsigned int ret, dd;
  size_t length;

  mop = airMopNew();

  hh = *hhP;
  /* skip past space */
  length = strspn(hh, nrrd__FieldSep);
  hh += length;

  /* make sure we have something */
  if (!*hh) {
    biffMaybeAddf(useBiff, NRRD, "%s: hit end of string before seeing (", me);
    airMopError(mop);
    return 1;
  }
  /* first, see if we're getting the non-vector */
  if ((strstr(hh, nrrd__NoSpaceVector) == hh)) {
    if (!hh[strlen(nrrd__NoSpaceVector)]
        || strchr(nrrd__FieldSep, hh[strlen(nrrd__NoSpaceVector)])) {
      /* yes, we got the non-vector */
      for (dd = 0; dd < spaceDim; dd++) {
        val[dd] = AIR_NAN;
      }
      length += strlen(nrrd__NoSpaceVector);
    } else {
      /* we got something that started out looking like the non-vector */
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse non-vector \"%s\"", me, hh);
      airMopError(mop);
      return 1;
    }
  } else {
    /* this isn't a non-vector */
    /* make sure we have an open paren */
    if ('(' != *hh) {
      biffMaybeAddf(useBiff, NRRD, "%s: first vector in \"%s\" didn't start with '('",
                    me, hh);
      airMopError(mop);
      return 1;
    }
    /* copy string (including open paren) for local fiddling */
    if (!(buff = airStrdup(hh))) {
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't allocate local buffer", me);
      airMopError(mop);
      return 1;
    }
    airMopAdd(mop, buff, airFree, airMopAlways);
    /* scan for close paren */
    hh = buff + 1;
    while (*hh) {
      if (')' == *hh) {
        break;
      } else {
        hh++;
      }
    }
    if (')' != *hh) {
      biffMaybeAddf(useBiff, NRRD, "%s: didn't see ')' at end of first vector in \"%s\"",
                    me, hh);
      airMopError(mop);
      return 1;
    }
    /* terminate at end paren */
    *(hh + 1) = 0;
    length += strlen(buff);
    /* see if we have too many fields */
    ret = airStrntok(buff + 1, sep);
    if (ret > spaceDim) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: space dimension is %u, but seem to have %u "
                    "coefficients",
                    me, spaceDim, ret);
      airMopError(mop);
      return 1;
    }
    /* try to parse the values */
    ret = airParseStrD(val, buff + 1, ",", spaceDim);
    if (spaceDim != ret) {
      biffMaybeAddf(useBiff, NRRD, "%s: parsed %u values, but space dimension is %u", me,
                    ret, spaceDim);
      airMopError(mop);
      return 1;
    }
  }
  /* probably not useful */
  for (dd = spaceDim; dd < NRRD_SPACE_DIM_MAX; dd++) {
    val[dd] = AIR_NAN;
  }
  /* make sure all coefficients exist or not together */
  for (dd = 1; dd < spaceDim; dd++) {
    if (!!AIR_EXISTS(val[0]) ^ !!AIR_EXISTS(val[dd])) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: existance of all space vector "
                    "coefficients must be consistent (val[0] not like "
                    "val[%u])",
                    me, dd);
      airMopError(mop);
      return 1;
    }
  }
  for (dd = 0; dd < spaceDim; dd++) {
    if (airIsInf_d(val[dd])) {
      biffMaybeAddf(useBiff, NRRD, "%s: vector coefficient %u can't be infinite", me,
                    dd);
      airMopError(mop);
      return 1;
    }
  }
  *hhP += length;
  airMopOkay(mop);
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_space_directions(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_space_directions";
  unsigned int dd;
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  CHECK_HAVE_DIM;
  CHECK_HAVE_SPACE_DIM;

  for (dd = 0; dd < nrrd->dim; dd++) {
    if (spaceVectorParse(nrrd->axis[dd].spaceDirection, &info, nrrd->spaceDim,
                         useBiff)) {
      biffMaybeAddf(useBiff, NRRD, "%s: trouble getting space vector %u of %u", me,
                    dd + 1, nrrd->dim);
      return 1;
    }
  }
  if (strlen(info) != strspn(info, nrrd__FieldSep)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u directions", me,
                  nrrd->dim);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_space_directions](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_centers(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_centers";
  unsigned int ai;
  char *tok, *info, *last;
  airArray *mop;

  AIR_UNUSED(file);
  mop = airMopNew();
  info = airStrdup(nio->line + nio->pos);
  airMopAdd(mop, info, airFree, airMopAlways);
  CHECK_HAVE_DIM;
  for (ai = 0; ai < nrrd->dim; ai++) {
    tok = airStrtok(!ai ? info : NULL, nrrd__FieldSep, &last);
    if (!tok) {
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't extract string for center %u of %u", me,
                    ai + 1, nrrd->dim);
      airMopError(mop);
      return 1;
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
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse center \"%s\" for axis %u", me,
                    tok, ai);
      airMopError(mop);
      return 1;
    }
  }
  if (airStrtok(!ai ? info : NULL, nrrd__FieldSep, &last)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u centers", me,
                  nrrd->dim);
    airMopError(mop);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_centers](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    airMopError(mop);
    return 1;
  }
  airMopOkay(mop);
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_kinds(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_kinds";
  unsigned int ai;
  char *info, *tok, *last;
  airArray *mop;

  AIR_UNUSED(file);
  mop = airMopNew();
  info = airStrdup(nio->line + nio->pos);
  airMopAdd(mop, info, airFree, airMopAlways);
  CHECK_HAVE_DIM;
  for (ai = 0; ai < nrrd->dim; ai++) {
    tok = airStrtok(!ai ? info : NULL, nrrd__FieldSep, &last);
    if (!tok) {
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't extract string for kind %u of %u", me,
                    ai + 1, nrrd->dim);
      airMopError(mop);
      return 1;
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
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse \"%s\" kind %u of %u", me, tok,
                    ai + 1, nrrd->dim);
      airMopError(mop);
      return 1;
    }
  }
  if (airStrtok(!ai ? info : NULL, nrrd__FieldSep, &last)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u kinds", me,
                  nrrd->dim);
    airMopError(mop);
    return 1;
  }
  /* can't run this now because kinds can come before sizes, in which
   *   case the kind/size check in nrrd__FieldCheck_kinds will incorrectly
   *   flag an error ...
   * if (nrrd__FieldCheck[nrrdField_kinds](nrrd, useBiff)) {
   *  biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
   *  airMopError(mop); return 1;
   *  }
   */
  airMopOkay(mop);
  return 0;
}

/*
 * The kind of double-quoted string extraction used for NRRD units, space units, and
 * labels: get the string within the "", with only one kind of escaping supported:
 * \" means "
 */
static char * /* Biff: maybe:2:NULL */
getQuotedString(char **hP, int useBiff) {
  static const char me[] = "getQuotedString";
  char *h, *buff, *ret;
  airArray *buffArr;
  unsigned int pos;
  airPtrPtrUnion appu;

  h = *hP;
  /* skip past space */
  /* printf("!%s: h |%s|\n", me, h);*/
  h += strspn(h, nrrd__FieldSep);
  /* printf("!%s: h |%s|\n", me, h);*/

  /* make sure we have something */
  if (!*h) {
    biffMaybeAddf(useBiff, NRRD, "%s: hit end of string before seeing opening \"", me);
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
  pos = airArrayLenIncr(buffArr, 1); /* pos should get 0 */
  while (h[pos]) {
    /* printf("!%s: h+%d |%s|\n", me, pos, h+pos); */
    if ('\"' == h[pos]) {
      break;
    }
    if ('\\' == h[pos] && '\"' == h[pos + 1]) {
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

static int /* Biff: maybe:4:1 */
rnParse_labels(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_labels";
  char *h; /* this is the "here" pointer which gradually progresses
              through all the labels (for all axes) */
  unsigned int ai;
  char *info;

  AIR_UNUSED(file);
  /* because we have to correctly interpret quote marks, we
     can't simply rely on airParseStrS */
  info = nio->line + nio->pos;
  /* printf("!%s: info |%s|\n", me, info); */
  CHECK_HAVE_DIM;
  h = info;
  for (ai = 0; ai < nrrd->dim; ai++) {
    if (!(nrrd->axis[ai].label = getQuotedString(&h, useBiff))) {
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't get get label %u of %u\n", me, ai + 1,
                    nrrd->dim);
      return 1;
    }
  }
  if (strlen(h) != strspn(h, nrrd__FieldSep)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u labels", me,
                  nrrd->dim);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_labels](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_units(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_units";
  char *h; /* this is the "here" pointer which gradually progresses
              through all the units (for all axes) */
  unsigned int ai;
  char *info;

  AIR_UNUSED(file);
  /* because we have to correctly interpret quote marks, we
     can't simply rely on airParseStrS */
  info = nio->line + nio->pos;
  /* printf("!%s: info |%s|\n", me, info); */
  CHECK_HAVE_DIM;
  h = info;
  for (ai = 0; ai < nrrd->dim; ai++) {
    if (!(nrrd->axis[ai].units = getQuotedString(&h, useBiff))) {
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't get get unit %u of %u\n", me, ai + 1,
                    nrrd->dim);
      return 1;
    }
  }
  if (strlen(h) != strspn(h, nrrd__FieldSep)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u units", me,
                  nrrd->dim);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_units](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int
rnParse_min(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {

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
rnParse_max(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  AIR_UNUSED(nio);
  AIR_UNUSED(useBiff);

  /* nrrd->max no longer exists, see above */

  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_old_min(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_old_min";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  PARSE_ONE_VAL(nrrd->oldMin, "%lg", "double");
  if (nrrd__FieldCheck[nrrdField_old_min](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_old_max(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_old_max";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  PARSE_ONE_VAL(nrrd->oldMax, "%lg", "double");
  if (nrrd__FieldCheck[nrrdField_old_max](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_endian(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_endian";
  char *info;

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  info = nio->line + nio->pos;
  if (!(nio->endian = airEnumVal(airEndian, info))) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse endian \"%s\"", me, info);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_encoding(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_encoding";
  char *info;
  int etype;

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  info = nio->line + nio->pos;
  if (!(etype = airEnumVal(nrrdEncodingType, info))) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse encoding \"%s\"", me, info);
    return 1;
  }

  nio->encoding = nrrdEncodingArray[etype];
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_line_skip(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_line_skip";
  char *info;

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  info = nio->line + nio->pos;
  PARSE_ONE_VAL(nio->lineSkip, "%u", "unsigned int");
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
rnParse_byte_skip(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_byte_skip";
  char *info;

  AIR_UNUSED(file);
  AIR_UNUSED(nrrd);
  info = nio->line + nio->pos;
  PARSE_ONE_VAL(nio->byteSkip, "%ld", "long int");
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

static int /* Biff: maybe:4:1 */
rnParse_keyvalue(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_keyvalue";
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
    biffMaybeAddf(useBiff, NRRD, "%s: didn't see \":=\" key/value delimiter in \"%s\"",
                  me, line);
    free(line);
    return 1;
  }
  keysep[0] = 0;
  keysep[1] = 0;
  key = line;
  value = keysep + 2;

  /* convert escape sequences */
  airUnescape(key);
  airUnescape(value);

  nrrdKeyValueAdd(nrrd, key, value);

  free(line);
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_sample_units(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_sample_units";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;

  if (strlen(info) && !(nrrd->sampleUnits = airStrdup(info))) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't strdup() sampleUnits", me);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_sample_units](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_space(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_space";
  char *info;
  int space;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  if (nio->seen[nrrdField_space_dimension]) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: can't specify space after specifying "
                  "space dimension (%u)",
                  me, nrrd->spaceDim);
    return 1;
  }
  if (!(space = airEnumVal(nrrdSpace, info))) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse space \"%s\"", me, info);
    return 1;
  }
  if (nrrdSpaceSet(nrrd, space)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_space](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_space_dimension(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_space_dimension";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;
  if (nio->seen[nrrdField_space]) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: can't specify space dimension after specifying "
                  "space (%s)",
                  me, airEnumStr(nrrdSpace, nrrd->space));
    return 1;
  }
  PARSE_ONE_VAL(nrrd->spaceDim, "%u", "unsigned int");
  if (nrrd__FieldCheck[nrrdField_space_dimension](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_space_units(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_space_units";
  char *h; /* this is the "here" pointer which gradually progresses
              through all the units (for all axes) */
  unsigned int ai;
  char *info;

  AIR_UNUSED(file);
  /* because we have to correctly interpret quote marks, we
     can't simply rely on airParseStrS */
  info = nio->line + nio->pos;
  /* printf("!%s: info |%s|\n", me, info); */
  CHECK_HAVE_SPACE_DIM;
  h = info;
  for (ai = 0; ai < nrrd->spaceDim; ai++) {
    if (!(nrrd->spaceUnits[ai] = getQuotedString(&h, useBiff))) {
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't get get space unit %u of %u", me,
                    ai + 1, nrrd->spaceDim);
      return 1;
    }
  }
  if (getQuotedString(&h, AIR_FALSE)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seemed to have more than expected %u space units",
                  me, nrrd->spaceDim);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_space_units](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_space_origin(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_space_origin";
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;

  CHECK_HAVE_SPACE_DIM;

  if (spaceVectorParse(nrrd->spaceOrigin, &info, nrrd->spaceDim, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse origin \"%s\"", me, info);
    return 1;
  }
  if (nrrd__FieldCheck[nrrdField_space_origin](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

static int /* Biff: maybe:4:1 */
rnParse_measurement_frame(FILE *file, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_measurement_frame";
  double colvec[NRRD_SPACE_DIM_MAX];
  unsigned int dd, ii;
  char *info;

  AIR_UNUSED(file);
  info = nio->line + nio->pos;

  CHECK_HAVE_SPACE_DIM;

  for (dd = 0; dd < nrrd->spaceDim; dd++) {
    /* we are going through the *columns* of the mf matrix */
    if (spaceVectorParse(colvec, &info, nrrd->spaceDim, useBiff)) {
      biffMaybeAddf(useBiff, NRRD, "%s: trouble getting space vector %u of %u", me,
                    dd + 1, nrrd->spaceDim);
      return 1;
    }
    for (ii = 0; ii < NRRD_SPACE_DIM_MAX; ii++) {
      nrrd->measurementFrame[dd][ii] = (ii < nrrd->spaceDim ? colvec[ii] : AIR_NAN);
    }
  }
  if (strlen(info) != strspn(info, nrrd__FieldSep)) {
    biffMaybeAddf(useBiff, NRRD, "%s: seem to have more than expected %u directions", me,
                  nrrd->spaceDim);
    return 1;
  }
  for (dd = nrrd->spaceDim; dd < NRRD_SPACE_DIM_MAX; dd++) {
    for (ii = 0; ii < NRRD_SPACE_DIM_MAX; ii++) {
      nrrd->measurementFrame[dd][ii] = AIR_NAN;
    }
  }
  if (nrrd__FieldCheck[nrrdField_measurement_frame](nrrd, useBiff)) {
    biffMaybeAddf(useBiff, NRRD, "%s: trouble", me);
    return 1;
  }
  return 0;
}

int /* Biff: nope */
nrrdContainsPercentThisAndMore(const char *str, char thss) {
  const char *hh, *tmp;

  tmp = str;
  do {
    hh = strchr(tmp, '%');
    if (!(hh && hh[1])) {
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
  if (!(hh[0] == thss)) {
    return 0;
  }
  hh += strcspn(hh, nrrd__FieldSep);
  return !!hh;
}

unsigned int /* Biff: nope */
nrrdIoDataFNNumber(NrrdIoState *nio) {
  unsigned int ret;
  int ii;

  if (nio->dataFNFormat) {
    /* datafiles given in iterator form; count number of values */
    ret = 0;
    for (ii = nio->dataFNMin; ((nio->dataFNStep > 0 && ii <= nio->dataFNMax)
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
int /* Biff: maybe:3:1 */
nrrdIoDataFNCheck(NrrdIoState *nio, Nrrd *nrrd, int useBiff) {
  static const char me[] = "nrrdIoDataFNCheck";
  size_t pieceSize, pieceNum;
  char stmp[AIR_STRLEN_SMALL + 1];

  if (!nio->seen[nrrdField_sizes]) {
    biffMaybeAddf(useBiff, NRRD,
                  "%s: sorry, currently can't handle "
                  "multiple detached data files without first knowing "
                  "the \"%s\" field",
                  me, airEnumStr(nrrdField, nrrdField_sizes));
    return 1;
  }
  if (nio->dataFileDim < nrrd->dim) {
    /* this requires that the per-axis size fields have been set */
    nrrd__SplitSizes(&pieceSize, &pieceNum, nrrd, nio->dataFileDim);
    if (pieceNum != nrrdIoDataFNNumber(nio)) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: expected %s filenames (of %u-D pieces) "
                    "but got %u",
                    me, airSprintSize_t(stmp, pieceNum), nio->dataFileDim,
                    nrrdIoDataFNNumber(nio));
      return 1;
    }
  } else {
    /* we're getting data in "slabs" with the same dimension as the
       nrrd, so for simplicity we assume that they're all equal size */
    if (nrrdIoDataFNNumber(nio) > nrrd->axis[nrrd->dim - 1].size) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: can't have more pieces (%u) than axis %u "
                    "slices (%s) when nrrd dimension and "
                    "datafile dimension are both %u",
                    me, nrrdIoDataFNNumber(nio), nrrd->dim - 1,
                    airSprintSize_t(stmp, nrrd->axis[nrrd->dim - 1].size), nrrd->dim);
      return 1;
    }
    if ((double)nrrd->axis[nrrd->dim - 1].size / nrrdIoDataFNNumber(nio)
        != nrrd->axis[nrrd->dim - 1].size / nrrdIoDataFNNumber(nio)) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: number of datafiles (%d) doesn't divide into "
                    "number of axis %u slices (%s)",
                    me, (int)nrrdIoDataFNNumber(nio), nrrd->dim - 1,
                    airSprintSize_t(stmp, nrrd->axis[nrrd->dim - 1].size));
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
static int /* Biff: maybe:4:1 */
rnParse_data_file(FILE *ffile, Nrrd *nrrd, NrrdIoState *nio, int useBiff) {
  static const char me[] = "rnParse_data_file";
  char *info, *nums;
  unsigned int tmp;
  airArray *mop;
  size_t linelen;

  mop = airMopNew();
  info = airStrdup(nio->line + nio->pos);
  if (!info) {
    biffMaybeAddf(useBiff, NRRD, "%s: couldn't copy line!", me);
    return 1;
  }
  airMopAdd(mop, info, airFree, airMopAlways);

  /* HEY: this change should be made someday
  if (nrrdContainsPercentThisAndMore(info, 'd')
      || nrrdContainsPercentThisAndMore(info, 'u')) { */
  if (nrrdContainsPercentThisAndMore(info, 'd')) {
    /* ---------------------------------------------------------- */
    /* --------- format.%d <min> <max> <step> [<dim>] ----------- */
    /* ---------------------------------------------------------- */
    size_t sspn;
    CHECK_HAVE_DIM;
    nums = info + strcspn(info, nrrd__FieldSep);
    sspn = strspn(nums, nrrd__FieldSep);
    nums[0] = 0; /* terminate so that format is now in info */
    nums += sspn;
    if (!(3
          == sscanf(nums, "%d %d %d", &(nio->dataFNMin), &(nio->dataFNMax),
                    &(nio->dataFNStep)))) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: couldn't parse three ints (min, max, step) after "
                    "data filename template",
                    me);
      airMopError(mop);
      return 1;
    }
    if (4
        == sscanf(nums, "%d %d %d %u", &(nio->dataFNMin), &(nio->dataFNMax),
                  &(nio->dataFNStep), &(nio->dataFileDim))) {
      if (!AIR_IN_CL(1, nio->dataFileDim, nrrd->dim)) {
        biffMaybeAddf(useBiff, NRRD,
                      "%s: datafile dimension %u outside valid range [1,%u]", me,
                      nio->dataFileDim, nrrd->dim);
        airMopError(mop);
        return 1;
      }
    } else {
      nio->dataFileDim = nrrd->dim - 1;
    }
    if (0 == nio->dataFNStep) {
      biffMaybeAddf(useBiff, NRRD, "%s: file number step must be non-zero", me);
      airMopError(mop);
      return 1;
    }
    if ((nio->dataFNMax - nio->dataFNMin) * (nio->dataFNStep) < 0) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: file number max %d not approached from min %d "
                    "by step %d",
                    me, nio->dataFNMax, nio->dataFNMin, nio->dataFNStep);
      airMopError(mop);
      return 1;
    }
    if (!(nio->dataFNFormat = airStrdup(info))) {
      biffMaybeAddf(useBiff, NRRD, "%s: couldn't copy data filename format", me);
      airMopError(mop);
      return 1;
    }
    if (nrrdIoDataFNCheck(nio, nrrd, useBiff)) {
      biffMaybeAddf(useBiff, NRRD, "%s: trouble with number of datafiles", me);
      airMopError(mop);
      return 1;
    }
  } else if (!strncmp(info, NRRD_LIST_FLAG, strlen(NRRD_LIST_FLAG))
             || !strncmp(info, NRRD_SKIPLIST_FLAG, strlen(NRRD_SKIPLIST_FLAG))) {
    int skiplist;
    unsigned int lineidx;
    /* ---------------------------------------------------------- */
    /* -------------------- LIST or SKIPLIST -------------------- */
    /* ---------------------------------------------------------- */
    CHECK_HAVE_DIM;
    skiplist = !strncmp(info, NRRD_SKIPLIST_FLAG, strlen(NRRD_SKIPLIST_FLAG));
    if (nrrd__HeaderCheck(nrrd, nio, AIR_TRUE)) {
      biffMaybeAddf(useBiff, NRRD,
                    "%s: NRRD header is incomplete. "
                    "\"%s\" data file specification must be "
                    "contiguous with end of header!",
                    me, skiplist ? NRRD_SKIPLIST_FLAG : NRRD_LIST_FLAG);
      airMopError(mop);
      return 1;
    }
    info += strlen(skiplist ? NRRD_SKIPLIST_FLAG : NRRD_LIST_FLAG);
    if (info[0]) {
      if (1 == sscanf(info, "%u", &(nio->dataFileDim))) {
        if (!AIR_IN_CL(1, nio->dataFileDim, nrrd->dim)) {
          biffMaybeAddf(useBiff, NRRD,
                        "%s: datafile dimension %u outside "
                        "valid range [1,%u]",
                        me, nio->dataFileDim, nrrd->dim);
          airMopError(mop);
          return 1;
        }
      } else {
        biffMaybeAddf(useBiff, NRRD,
                      "%s: couldn't parse info after "
                      "\"%s\" as an int",
                      me, skiplist ? NRRD_SKIPLIST_FLAG : NRRD_LIST_FLAG);
        airMopError(mop);
        return 1;
      }
    } else {
      /* nothing after NRRD_LIST_FLAG or NRRD_SKIPLIST_FLAG,
         so dataFileDim is implicit */
      nio->dataFileDim = nrrd->dim - 1;
    }
    /* read in all the datafile names */
    lineidx = 0;
    do {
      /* yes, nio->line is re-used/over-written here, but I don't think that's a problem
         (2025 ha ha can I point out nioLineSave in formatNRRD.c/nrrd__FormatNRRD_read)
       */
      if (nrrdOneLine(&linelen, nio, ffile)) {
        biffMaybeAddf(useBiff, NRRD, "%s: trouble getting file name line %u", me,
                      lineidx);
        airMopError(mop);
        return 1;
      }
      if (linelen > 0) {
        /* we got a non-empty line */
        if (skiplist) {
          char *lhere;
          long int oneskip;
          if (1 != airSingleSscanf(nio->line, "%ld", &oneskip)) {
            biffMaybeAddf(useBiff, NRRD, "%s: couldn't parse skip on list line %u", me,
                          lineidx);
            airMopError(mop);
            return 1;
          }
          lhere = strchr(nio->line, ' ');
          if (!lhere) {
            biffMaybeAddf(useBiff, NRRD,
                          "%s: didn't see space after "
                          "skip on list line %u",
                          me, lineidx);
            airMopError(mop);
            return 1;
          }
          lhere++;
          if (!(lhere[0])) {
            biffMaybeAddf(useBiff, NRRD,
                          "%s: didn't see filename after "
                          "skip and space on list line %u",
                          me, lineidx);
            airMopError(mop);
            return 1;
          }
          airArrayLenIncr(nio->dataFSkipArr, 1);
          nio->dataFSkip[lineidx] = oneskip;
          airArrayLenIncr(nio->dataFNArr, 1);
          nio->dataFN[lineidx] = airStrdup(lhere);
        } else {
          airArrayLenIncr(nio->dataFNArr, 1);
          nio->dataFN[lineidx] = airStrdup(nio->line);
        }
      }
      ++lineidx;
    } while (linelen > 0);
    if (nrrdIoDataFNCheck(nio, nrrd, useBiff)) {
      biffMaybeAddf(useBiff, NRRD, "%s: trouble with number of datafiles", me);
      airMopError(mop);
      return 1;
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
int (*const nrrdFieldInfoParse[NRRD_FIELD_MAX + 1])(FILE *, Nrrd *, NrrdIoState *, int)
  = {rnParse_nonfield,
     rnParse_comment,
     rnParse_content,
     rnParse_number,
     rnParse_type,
     rnParse_block_size,
     rnParse_dimension,
     rnParse_space,
     rnParse_space_dimension,
     rnParse_sizes,
     rnParse_spacings,
     rnParse_thicknesses,
     rnParse_axis_mins,
     rnParse_axis_maxs,
     rnParse_space_directions,
     rnParse_centers,
     rnParse_kinds,
     rnParse_labels,
     rnParse_units,
     rnParse_min,
     rnParse_max,
     rnParse_old_min,
     rnParse_old_max,
     rnParse_endian,
     rnParse_encoding,
     rnParse_line_skip,
     rnParse_byte_skip,
     rnParse_keyvalue,
     rnParse_sample_units,
     rnParse_space_units,
     rnParse_space_origin,
     rnParse_measurement_frame,
     rnParse_data_file};

/* kernel parsing is all in kernel.c */

/* nrrdStringValsParse[]: parse N values of given type from string
   NB: based on air/parseAir.c
*/
#define P_ARGS (void *_out, const char *_s, const char *sep, size_t n)
#define P_BODY(type, ntype)                                                             \
  size_t i;                                                                             \
  char *tmp, *s, *last;                                                                 \
  const char *format;                                                                   \
  type *out;                                                                            \
                                                                                        \
  /* if we got NULL, there's nothing to do */                                           \
  if (!(_out && _s && sep)) return 0;                                                   \
  format = nrrdTypePrintfStr[ntype];                                                    \
  out = (type *)_out;                                                                   \
  /* copy the input so that we don't change it */                                       \
  s = airStrdup(_s);                                                                    \
                                                                                        \
  /* keep calling airStrtok() until we have everything */                               \
  for (i = 0; i < n; i++) {                                                             \
    tmp = airStrtok(i ? NULL : s, sep, &last);                                          \
    if (!tmp) {                                                                         \
      free(s);                                                                          \
      return i;                                                                         \
    }                                                                                   \
    if (1 != airSingleSscanf(tmp, format, out + i)) {                                   \
      free(s);                                                                          \
      return i;                                                                         \
    }                                                                                   \
  }                                                                                     \
  free(s);                                                                              \
  return n

/* clang-format off */
static size_t   parseChar P_ARGS { P_BODY(          char,   nrrdTypeChar); }
static size_t  parseUChar P_ARGS { P_BODY( unsigned char,  nrrdTypeUChar); }
static size_t  parseShort P_ARGS { P_BODY(         short,  nrrdTypeShort); }
static size_t parseUShort P_ARGS { P_BODY(unsigned short, nrrdTypeUShort); }
static size_t    parseInt P_ARGS { P_BODY(           int,    nrrdTypeInt); }
static size_t   parseUInt P_ARGS { P_BODY(  unsigned int,   nrrdTypeUInt); }
static size_t  parseLLong P_ARGS { P_BODY(      airLLong,  nrrdTypeLLong); }
static size_t parseULLong P_ARGS { P_BODY(     airULLong, nrrdTypeULLong); }
static size_t  parseFloat P_ARGS { P_BODY(         float,  nrrdTypeFloat); }
static size_t parseDouble P_ARGS { P_BODY(        double, nrrdTypeDouble); }
static size_t parseNoop(void *out, const char *s, const char *sep, size_t n) {
  AIR_UNUSED(out);
  AIR_UNUSED(s);
  AIR_UNUSED(sep);
  AIR_UNUSED(n);
  return 0;
}
#undef P_ARGS
#undef P_BODY

size_t
(*const nrrdStringValsParse[NRRD_TYPE_MAX+1])(void *out, const char *s,
                                              const char *sep, size_t n)
= {
   parseNoop, /* 0 = unknown */
   parseChar,
   parseUChar,
   parseShort,
   parseUShort,
   parseInt,
   parseUInt,
   parseLLong,
   parseULLong,
   parseFloat,
   parseDouble,
   parseNoop /* block */
};
/* clang-format on */

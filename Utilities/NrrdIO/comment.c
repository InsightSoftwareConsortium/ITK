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

/*
******** nrrdCommentAdd()
**
** Adds a given string to the list of comments
** Leading spaces (' ') and comment chars ('#') are not included.
**
** This function does NOT use biff.
*/
int
nrrdCommentAdd(Nrrd *nrrd, const char *_str) {
  /* static const char me[]="nrrdCommentAdd";*/
  char *str;
  int i;
  
  if (!(nrrd && _str)) {
    /*
    sprintf(err, "%s: got NULL pointer", me);
    biffMaybeAdd(NRRD, err, useBiff);
    */
    return 1;
  }
  _str += strspn(_str, " #");
  if (!strlen(_str)) {
    /* we don't bother adding comments with no length */
    return 0;
  }
  if (!strcmp(_str, _nrrdFormatURLLine0)
      || !strcmp(_str, _nrrdFormatURLLine1)) {
    /* sneaky hack: don't store the format URL comment lines */
    return 0;
  }
  str = airStrdup(_str);
  if (!str) {
    /*
    sprintf(err, "%s: couldn't strdup given string", me);
    biffMaybeAdd(NRRD, err, useBiff);
    */
    return 1;
  }
  /* clean out carraige returns that would screw up reader */
  airOneLinify(str);
  i = airArrayLenIncr(nrrd->cmtArr, 1);
  if (!nrrd->cmtArr->data) {
    /*
    sprintf(err, "%s: couldn't lengthen comment array", me);
    biffMaybeAdd(NRRD, err, useBiff);
    */
    return 1;
  }
  nrrd->cmt[i] = str;
  return 0;
}

/*
******** nrrdCommentClear()
**
** blows away comments, but does not blow away the comment airArray
*/
void
nrrdCommentClear(Nrrd *nrrd) {

  if (nrrd) {
    airArrayLenSet(nrrd->cmtArr, 0);
  }
}

/*
******** nrrdCommentCopy()
**
** copies comments from one nrrd to another
** Existing comments in nout are blown away
**
** This does NOT use biff.
*/
int
nrrdCommentCopy(Nrrd *nout, const Nrrd *nin) {
  /* static const char me[]="nrrdCommentCopy"; */
  int numc, i, E;

  if (!(nout && nin)) {
    /*
    sprintf(err, "%s: got NULL pointer", me);
    biffMaybeAdd(NRRD, err, useBiff);
    */
    return 1;
  }
  if (nout == nin) {
    /* can't satisfy semantics of copying with nout==nin */
    return 2;
  }
  nrrdCommentClear(nout);
  numc = nin->cmtArr->len;
  E = 0;
  for (i=0; i<numc; i++) {
    if (!E) E |= nrrdCommentAdd(nout, nin->cmt[i]);
  }
  if (E) {
    /*
    sprintf(err, "%s: couldn't add all comments", me);
    biffMaybeAdd(NRRD, err, useBiff);
    */
    return 3;
  }
  return 0;
}

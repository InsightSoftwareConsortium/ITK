/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2012, 2011, 2010, 2009  University of Chicago
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
  unsigned int ii;

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
  ii = airArrayLenIncr(nrrd->cmtArr, 1);
  if (!nrrd->cmtArr->data) {
    /*
    sprintf(err, "%s: couldn't lengthen comment array", me);
    biffMaybeAdd(NRRD, err, useBiff);
    */
    return 1;
  }
  nrrd->cmt[ii] = str;
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
  int E;
  unsigned int numc, ii;

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
  for (ii=0; ii<numc; ii++) {
    if (!E) E |= nrrdCommentAdd(nout, nin->cmt[ii]);
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

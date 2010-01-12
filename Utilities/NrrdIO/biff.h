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

#ifndef BIFF_HAS_BEEN_INCLUDED
#define BIFF_HAS_BEEN_INCLUDED

/* ---- BEGIN non-NrrdIO */

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "NrrdIO.h"

#if defined(_WIN32) && !defined(__CYGWIN__) && !defined(TEEM_STATIC)
#  if defined(TEEM_BUILD) || defined(biff_EXPORTS) || defined(teem_EXPORTS)
#    define BIFF_EXPORT extern __declspec(dllexport)
#  else
#    define BIFF_EXPORT extern __declspec(dllimport)
#  endif
#else /* TEEM_STATIC || UNIX */
#  define BIFF_EXPORT extern
#endif
/* ---- END non-NrrdIO */


#ifdef __cplusplus
extern "C" {
#endif

/*
** biffMsg struct
**
** externally usable thing for holding error messages
*/
typedef struct {
  char *key;                   /* string for identifying the general source
                                  of the error message; set once, at time 
                                  of biffMsg creation */
  char **err;                  /* array of error strings; the err array itself
                                  is NOT null-terminated */
  unsigned int errNum;         /* length of "err" == # strings stored */
  airArray *errArr;            /* air array for err and num */
} biffMsg;

/* biffmsg.c */
BIFF_EXPORT biffMsg *biffMsgNew(const char *key);
BIFF_EXPORT biffMsg *biffMsgNix(biffMsg *msg);
BIFF_EXPORT void biffMsgAdd(biffMsg *msg, const char *err);
BIFF_EXPORT void biffMsgAddVL(biffMsg *msg, const char *errfmt, va_list args);
BIFF_EXPORT void biffMsgAddf(biffMsg *msg, const char *errfmt, ...)
#ifdef __GNUC__
__attribute__ ((format(printf,2,3)))
#endif
;
BIFF_EXPORT void biffMsgClear(biffMsg *msg);
BIFF_EXPORT unsigned int biffMsgLineLenMax(const biffMsg *msg);
BIFF_EXPORT void biffMsgMove(biffMsg *dest, biffMsg *src,
                             const char *err);
BIFF_EXPORT void biffMsgMoveVL(biffMsg *dest, biffMsg *src,
                               const char *errfmt, va_list args);
BIFF_EXPORT void biffMsgMovef(biffMsg *dest, biffMsg *src,
                                const char *errfmt, ...)
#ifdef __GNUC__
__attribute__ ((format(printf,3,4)))
#endif
;
BIFF_EXPORT unsigned int biffMsgStrlen(const biffMsg *msg);
BIFF_EXPORT char *biffMsgStrAlloc(const biffMsg *msg);
BIFF_EXPORT void biffMsgStrSet(char *ret, const biffMsg *msg);
BIFF_EXPORT char *biffMsgStrGet(const biffMsg *msg);
BIFF_EXPORT biffMsg *biffMsgNoop;

/* biffbiff.c */
BIFF_EXPORT void biffAdd(const char *key, const char *err);
BIFF_EXPORT void biffAddVL(const char *key, const char *errfmt, va_list args);
BIFF_EXPORT void biffAddf(const char *key, const char *errfmt, ...)
#ifdef __GNUC__
  __attribute__ ((format(printf,2,3)))
#endif
;
BIFF_EXPORT void biffMaybeAdd(const char *key, const char *err, int useBiff);
BIFF_EXPORT void biffMaybeAddf(int useBiff, const char *key,
                               const char *errfmt, ... )
#ifdef __GNUC__
__attribute__ ((format(printf,3,4)))
#endif
;
BIFF_EXPORT char *biffGet(const char *key);
BIFF_EXPORT int biffGetStrlen(const char *key);
BIFF_EXPORT void biffSetStr(char *str, const char *key);
BIFF_EXPORT int biffCheck(const char *key);
BIFF_EXPORT void biffDone(const char *key);
BIFF_EXPORT void biffMove(const char *destKey, const char *err,
                          const char *srcKey);
BIFF_EXPORT void biffMoveVL(const char *destKey, const char *srcKey,
                            const char *errfmt, va_list args);
BIFF_EXPORT void biffMovef(const char *destKey, const char *srcKey,
                            const char *errfmt, ...)
#ifdef __GNUC__
__attribute__ ((format(printf,3,4)))
#endif
;
BIFF_EXPORT char *biffGetDone(const char *key);
BIFF_EXPORT void biffSetStrDone(char *str, const char *key);

#ifdef __cplusplus
}
#endif

#endif /* BIFF_HAS_BEEN_INCLUDED */

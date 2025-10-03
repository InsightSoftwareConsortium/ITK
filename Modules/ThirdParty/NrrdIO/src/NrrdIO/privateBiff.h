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

#ifdef __cplusplus
extern "C" {
#endif

/*
** This private header was created because the following two "VL" functions are used
** only within in the biff sources. They take a va_list, which is unusual, and
** (currently) used for no other public functions in Teem.
**
** Furthermore, pre-1.13 release it became apparent that nothing else in Teem (outside
** of biff) was using any biffMsg anything, so these were also all moved to here,
** though out of laziness no _ prefix was added (as is expected of "private" text
** symbols in the library)
*/

/* biffmsg.c */
extern void _biffMsgAddVL(biffMsg *msg, const char *errfmt, va_list args);
extern void _biffMsgMoveVL(biffMsg *dest, biffMsg *src, const char *errfmt,
                           va_list args);

extern biffMsg *biffMsgNew(const char *key);
extern biffMsg *biffMsgNix(biffMsg *msg);
extern void biffMsgAdd(biffMsg *msg, const char *err);
extern void biffMsgClear(biffMsg *msg);
extern void biffMsgMove(biffMsg *dest, biffMsg *src, const char *err);
extern unsigned int biffMsgErrNum(const biffMsg *msg);
extern unsigned int biffMsgStrlen(const biffMsg *msg);
extern void biffMsgStrSet(char *ret, const biffMsg *msg);

#ifdef __cplusplus
}
#endif

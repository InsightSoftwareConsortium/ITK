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

#ifdef __cplusplus
extern "C" {
#endif

/* NOTE: these SN_,ASP1_ string macros copy-pasta'd to other private<Lib>.h files:
   SN_INCR: safely increments STR arg to point '\0'-termination of STR
   SN_COPY: safely copies SRC to DST, then increments DST
   SN_PRINTF: snprintf's into DST then increments DST
   ASP1_X: short version of AIR_STRLEN_X + 1 */
#define SN_INCR(STR, SIZE)                                                              \
  do {                                                                                  \
    size_t tmp_str_len_##STR = strlen(STR);                                             \
    STR += tmp_str_len_##STR;                                                           \
    SIZE -= tmp_str_len_##STR;                                                          \
  } while (0)
#define SN_COPY(DST, DST_SIZE, SRC)                                                     \
  do {                                                                                  \
    airStrcpy((DST), (DST_SIZE), (SRC));                                                \
    SN_INCR(DST, DST_SIZE);                                                             \
  } while (0)
#define ASP1_S (AIR_STRLEN_SMALL + 1)
#define ASP1_M (AIR_STRLEN_MED + 1)
#define ASP1_L (AIR_STRLEN_LARGE + 1)
#define ASP1_H (AIR_STRLEN_HUGE + 1)

typedef unsigned int uint; /* uint is just more concise */

/*
 * This private header was created because the biff__MsgAddVL and biff__MsgMoveVL
 * functions are only used within in the biff sources. They take a va_list, which is
 * unusual, and (currently) used for no other public functions in Teem.
 *
 * Furthermore, pre-1.13 release it became apparent that nothing else in Teem (outside of
 * biff) was using any biffMsg anything, so these were also all moved into here.
 * For TeemV2, the _ prefix was belatedly added to the biffMsg symbols (as is expected of
 * "private" text symbols in the library), and some were removed because they were only
 * used once in biffbiff.c (so their body could be effectively inlined at their usage)
 */

/*
** biffMsg struct
**
** externally usable thing for holding error messages
*/
typedef struct {
  char *key;           /* string for identifying the general source
                          of the error message; set once, at time
                          of biffMsg creation */
  char **err;          /* array of error strings; the err array itself
                          is NOT null-terminated */
  unsigned int errNum; /* length of "err" == # strings stored */
  airArray *errArr;    /* air array for err and num */
} biffMsg;

/* biffmsg.c */
extern const biffMsg *const biff__MsgNoop;
extern void biff__MsgAddVL(biffMsg *msg, const char *errfmt, va_list args);
extern void biff__MsgMoveVL(biffMsg *dest, biffMsg *src, const char *errfmt,
                            va_list args);

extern biffMsg *biff__MsgNew(const char *key);
extern biffMsg *biff__MsgNix(biffMsg *msg);
extern void biff__MsgAdd(biffMsg *msg, const char *err);
extern void biff__MsgMove(biffMsg *dest, biffMsg *src, const char *err);

#ifdef __cplusplus
}
#endif

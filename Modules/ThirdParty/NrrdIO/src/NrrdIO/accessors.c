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
#include "float.h"

/*
** making these typedefs here allows us to use one token for both
** constructing function names, and for specifying argument types
*/
typedef signed char CH;
typedef unsigned char UC;
typedef signed short SH;
typedef unsigned short US;
/* Microsoft apparently uses 'IN' as a keyword, so we changed 'IN' to 'JN'. */
typedef signed int JN;
typedef unsigned int UI;
typedef airLLong LL;
/* ui64 to double conversion is not implemented, sorry */
#if defined(_MSC_VER) && _MSC_VER < 1300
typedef airLLong UL;
#else
typedef airULLong UL;
#endif
typedef float FL;
typedef double DB;

#define MAP(F, A)                                                                       \
  F(A, CH)                                                                              \
  F(A, UC)                                                                              \
  F(A, SH)                                                                              \
  F(A, US)                                                                              \
  F(A, JN)                                                                              \
  F(A, UI)                                                                              \
  F(A, LL)                                                                              \
  F(A, UL)                                                                              \
  F(A, FL)                                                                              \
  F(A, DB)

/*
 * nload<TA><TB>(const void *_v)
 *
 * Load value from TB*, convert to TA:
 * assign `*_v` to `const TB*`, dereference, cast value to TA, return it.
 *
 * calling a cast function pointer is undefined behavior (UB), so TeemV2 avoids this:
 * #define LOAD_DEFN(TA, TB) static TA nload##TA##TB(TB *v) { return (TA)(*v); }
 * #define LOAD_LIST(TA, TB) (TA (*)(const void *)) nload##TA##TB,
 *                           ^^^^^^^^^^^^^^^^^^^^^^ casting function pointer
 */
#define LOAD_DEFN(TA, TB)                                                               \
  static TA nload##TA##TB(const void *_v) {                                             \
    const TB *v = _v;                                                                   \
    return (TA)(*v);                                                                    \
  }
#define LOAD_LIST(TA, TB) nload##TA##TB,

MAP(LOAD_DEFN, UI)
MAP(LOAD_DEFN, JN)
MAP(LOAD_DEFN, FL)
MAP(LOAD_DEFN, DB)

/* clang-format off */
unsigned int (*const nrrdUILoad[NRRD_TYPE_MAX + 1])(const void *) = {NULL, MAP(LOAD_LIST, UI) NULL};
         int (*const  nrrdILoad[NRRD_TYPE_MAX + 1])(const void *) = {NULL, MAP(LOAD_LIST, JN) NULL};
       float (*const  nrrdFLoad[NRRD_TYPE_MAX + 1])(const void *) = {NULL, MAP(LOAD_LIST, FL) NULL};
      double (*const  nrrdDLoad[NRRD_TYPE_MAX + 1])(const void *) = {NULL, MAP(LOAD_LIST, DB) NULL};
/* clang-format on */
#undef LOAD_DEFN
#undef LOAD_LIST

/*
 * nstore<TA><TB>(void *_v, <TA> j)
 *
 * Stores TA `j` in (after casting to `TB*`) `*_v`, implicitly casting value to TB.
 * Returns the result of the assignment, cast back to TA, which may not be the same as
 * the value that was passed in.
 *
 * TeemV2 seeks to avoid UB by not casting function pointers, so no longer doing:
 * #define STORE_DEF(TA, TB) \
 *    static TA nstore##TA##TB(TB *v, TA j) { return (TA)(*v = (TB)j); }
 * #define STORE_LIST(TA, TB) (TA (*)(void *, TA)) nstore##TA##TB,
 *                            ^^^^^^^^^^^^^^^^^^^ casting function pointer
 */
#define STORE_DEF(TA, TB)                                                               \
  static TA nstore##TA##TB(void *_v, TA j) {                                            \
    TB *v = _v;                                                                         \
    return (TA)(*v = (TB)j);                                                            \
  }
#define STORE_LIST(TA, TB) nstore##TA##TB,

MAP(STORE_DEF, UI)
MAP(STORE_DEF, JN)
MAP(STORE_DEF, FL)
MAP(STORE_DEF, DB)

/* clang-format off */
unsigned int (*const nrrdUIStore[NRRD_TYPE_MAX + 1])(void *, unsigned int) = {NULL, MAP(STORE_LIST, UI) NULL};
         int (*const  nrrdIStore[NRRD_TYPE_MAX + 1])(void *, int)          = {NULL, MAP(STORE_LIST, JN) NULL};
       float (*const  nrrdFStore[NRRD_TYPE_MAX + 1])(void *, float)        = {NULL, MAP(STORE_LIST, FL) NULL};
      double (*const  nrrdDStore[NRRD_TYPE_MAX + 1])(void *, double)       = {NULL, MAP(STORE_LIST, DB) NULL};
/* clang-format on */
#undef STORE_DEF
#undef STORE_LIST

/*
 * nlookup<TA><TB>(const void *_v, size_t I)
 *
 * After assigning _v to TB *v, looks up TB v[I], and returns it cast to TA.
 *
 * pre-TeemV2 UB code:
 * #define LOOKUP_DEF(TA, TB)
 *   static TA nlookup##TA##TB(TB *v, size_t I) { return (TA)v[I]; }
 * #define LOOKUP_LIST(TA, TB) (TA (*)(const void *, size_t)) nlookup##TA##TB,
 */
#define LOOKUP_DEF(TA, TB)                                                              \
  static TA nlookup##TA##TB(const void *_v, size_t I) {                                 \
    const TB *v = _v;                                                                   \
    return (TA)v[I];                                                                    \
  }
#define LOOKUP_LIST(TA, TB) nlookup##TA##TB,

MAP(LOOKUP_DEF, UI)
MAP(LOOKUP_DEF, JN)
MAP(LOOKUP_DEF, FL)
MAP(LOOKUP_DEF, DB)

/* clang-format off */
unsigned int (*const nrrdUILookup[NRRD_TYPE_MAX + 1])(const void *, size_t) = {NULL, MAP(LOOKUP_LIST, UI) NULL};
         int (*const  nrrdILookup[NRRD_TYPE_MAX + 1])(const void *, size_t) = {NULL, MAP(LOOKUP_LIST, JN) NULL};
       float (*const  nrrdFLookup[NRRD_TYPE_MAX + 1])(const void *, size_t) = {NULL, MAP(LOOKUP_LIST, FL) NULL};
      double (*const  nrrdDLookup[NRRD_TYPE_MAX + 1])(const void *, size_t) = {NULL, MAP(LOOKUP_LIST, DB) NULL};
/* clang-format on */
#undef LOOKUP_DEF
#undef LOOKUP_LIST

/*
 * ninsert<TA><TB>(void *_v, size_t I, <TA> j)
 *
 * Given TA j, casts to TB, and stores it in (after casting to TB*) _v[i].
 * Returns the result of the assignment, cast back to TA, which may not be the same as
 * the value that was passed in.
 * pre-TeemV2 UB code:
 * #define INSERT_DEF(TA, TB)
 *   static TA ninsert##TA##TB(TB *v, size_t I, TA j) { return (TA)(v[I] = (TB)j); }
 * #define INSERT_LIST(TA, TB) (TA (*)(void *, size_t, TA)) ninsert##TA##TB,
 */
#define INSERT_DEF(TA, TB)                                                              \
  static TA ninsert##TA##TB(void *_v, size_t I, TA j) {                                 \
    TB *v = _v;                                                                         \
    return (TA)(v[I] = (TB)j);                                                          \
  }
#define INSERT_LIST(TA, TB) ninsert##TA##TB,

MAP(INSERT_DEF, UI)
MAP(INSERT_DEF, JN)
MAP(INSERT_DEF, FL)
MAP(INSERT_DEF, DB)

/* clang-format off */
unsigned int (*const nrrdUIInsert[NRRD_TYPE_MAX + 1])(void *, size_t, unsigned int) = {NULL, MAP(INSERT_LIST, UI) NULL};
         int (*const  nrrdIInsert[NRRD_TYPE_MAX + 1])(void *, size_t, int)          = {NULL, MAP(INSERT_LIST, JN) NULL};
       float (*const  nrrdFInsert[NRRD_TYPE_MAX + 1])(void *, size_t, float)        = {NULL, MAP(INSERT_LIST, FL) NULL};
      double (*const  nrrdDInsert[NRRD_TYPE_MAX + 1])(void *, size_t, double)       = {NULL, MAP(INSERT_LIST, DB) NULL};
/* clang-format on */
#undef INSERT_DEF
#undef INSERT_LIST

/*
 ******** nrrdSprint
 *
 * Dereferences void *_v (after casting to specific type)
 * and snprintf()s that value into given string s,
 * returns the result of snprintf()
 */
/* clang-format off */
static int
nsprintCH(char *s, size_t sz, const void *_v) {
     const CH *v = _v; return snprintf(s, sz, "%d", *v); }
static int
nsprintUC(char *s, size_t sz, const void *_v) {
     const UC *v = _v; return snprintf(s, sz, "%u", *v); }
static int
nsprintSH(char *s, size_t sz, const void *_v) {
    const SH *v = _v; return snprintf(s, sz, "%d", *v); }
static int
nsprintUS(char *s, size_t sz, const void *_v) {
     const US *v = _v; return snprintf(s, sz, "%u", *v); }
static int
nsprintIN(char *s, size_t sz, const void *_v) {
     const JN *v = _v; return snprintf(s, sz, "%d", *v); }
static int
nsprintUI(char *s, size_t sz, const void *_v) {
     const UI *v = _v; return snprintf(s, sz, "%u", *v); }
static int
nsprintLL(char *s, size_t sz, const void *_v) {
     const LL *v = _v; return snprintf(s, sz, AIR_LLONG_FMT, *v); }
static int
nsprintUL(char *s, size_t sz, const void *_v) {
     const UL *v = _v; return snprintf(s, sz, AIR_ULLONG_FMT, *v); }
static int
nsprintFL(char *s, size_t sz, const void *_v) {
  const FL *v = _v;
  /* having %.8g instead of %.9g was a roughly 20-year old bug,
     but now using %g with TeemV2 airSinglePrintf */
  return airSinglePrintf(NULL, s, sz, "%g", *v);
}
static int
nsprintDB(char *s, size_t sz, const void *_v) {
  const DB *v = _v;
  /* not with %.17g thanks to TeemV2 airSinglePrintf */
  return airSinglePrintf(NULL, s, sz, "%lg", *v);
}
int (*const nrrdSprint[NRRD_TYPE_MAX + 1])(char *, size_t, const void *)
  = {NULL,
     nsprintCH,
     nsprintUC,
     nsprintSH,
     nsprintUS,
     nsprintIN,
     nsprintUI,
     nsprintLL,
     nsprintUL,
     nsprintFL,
     nsprintDB,
     NULL};
/* clang-format on */

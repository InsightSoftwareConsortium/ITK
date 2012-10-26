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
#include "float.h"

/*
** making these typedefs here allows us to used one token for both
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
#if _MSC_VER < 1300
typedef airLLong UL;
#else
typedef airULLong UL;
#endif
typedef float FL;
typedef double DB;

#define MAP(F, A) \
F(A, CH) \
F(A, UC) \
F(A, SH) \
F(A, US) \
F(A, JN) \
F(A, UI) \
F(A, LL) \
F(A, UL) \
F(A, FL) \
F(A, DB)

/*
** _nrrdLoad<TA><TB>(<TB> *v)
**
** Dereferences v as TB*, casts it to TA, returns it.
*/
#define LOAD_DEF(TA, TB)                    \
static TA                                   \
_nrrdLoad##TA##TB(TB *v) {                  \
  return (TA)(*v);                          \
}
#define LOAD_LIST(TA, TB)                   \
  (TA (*)(const void *))_nrrdLoad##TA##TB,

MAP(LOAD_DEF, UI)
MAP(LOAD_DEF, JN)
MAP(LOAD_DEF, FL)
MAP(LOAD_DEF, DB)

unsigned int (*
nrrdUILoad[NRRD_TYPE_MAX+1])(const void*) = {
  NULL, MAP(LOAD_LIST, UI) NULL
};
int (*
nrrdILoad[NRRD_TYPE_MAX+1])(const void*) = {
  NULL, MAP(LOAD_LIST, JN) NULL
};
float (*
nrrdFLoad[NRRD_TYPE_MAX+1])(const void*) = {
  NULL, MAP(LOAD_LIST, FL) NULL
};
double (*
nrrdDLoad[NRRD_TYPE_MAX+1])(const void*) = {
  NULL, MAP(LOAD_LIST, DB) NULL
};


/*
** _nrrdStore<TA><TB>(<TB> *v, <TA> j)
**
** Takes a TA j, and stores it in *v, thereby implicitly casting it to TB.
** Returns the result of the assignment, which may not be the same as
** the value that was passed in.
*/
#define STORE_DEF(TA, TB)                   \
static TA                                   \
_nrrdStore##TA##TB(TB *v, TA j) {           \
  return (TA)(*v = (TB)j);                  \
}
#define STORE_LIST(TA, TB)                  \
  (TA (*)(void *, TA))_nrrdStore##TA##TB,

MAP(STORE_DEF, UI)
MAP(STORE_DEF, JN)
MAP(STORE_DEF, FL)
MAP(STORE_DEF, DB)

unsigned int (*
nrrdUIStore[NRRD_TYPE_MAX+1])(void *, unsigned int) = {
  NULL, MAP(STORE_LIST, UI) NULL
};
int (*
nrrdIStore[NRRD_TYPE_MAX+1])(void *, int) = {
  NULL, MAP(STORE_LIST, JN) NULL
};
float (*
nrrdFStore[NRRD_TYPE_MAX+1])(void *, float) = {
  NULL, MAP(STORE_LIST, FL) NULL
};
double (*
nrrdDStore[NRRD_TYPE_MAX+1])(void *, double) = {
  NULL, MAP(STORE_LIST, DB) NULL
};


/*
** _nrrdLookup<TA><TB>(<TB> *v, size_t I)
**
** Looks up element I of TB array v, and returns it cast to a TA.
*/
#define LOOKUP_DEF(TA, TB)                    \
static TA                                     \
_nrrdLookup##TA##TB(TB *v, size_t I) {        \
  return (TA)v[I];                            \
}
#define LOOKUP_LIST(TA, TB)                   \
  (TA (*)(const void*, size_t))_nrrdLookup##TA##TB,

MAP(LOOKUP_DEF, UI)
MAP(LOOKUP_DEF, JN)
MAP(LOOKUP_DEF, FL)
MAP(LOOKUP_DEF, DB)

unsigned int (*
nrrdUILookup[NRRD_TYPE_MAX+1])(const void *, size_t) = {
  NULL, MAP(LOOKUP_LIST, UI) NULL
};
int (*
nrrdILookup[NRRD_TYPE_MAX+1])(const void *, size_t) = {
  NULL, MAP(LOOKUP_LIST, JN) NULL
};
float (*
nrrdFLookup[NRRD_TYPE_MAX+1])(const void *, size_t) = {
  NULL, MAP(LOOKUP_LIST, FL) NULL
};
double (*
nrrdDLookup[NRRD_TYPE_MAX+1])(const void *, size_t) = {
  NULL, MAP(LOOKUP_LIST, DB) NULL
};


/*
** _nrrdInsert<TA><TB>(<TB> *v, size_t I, <TA> j)
**
** Given TA j, stores it in v[i] (implicitly casting to TB).
** Returns the result of the assignment, which may not be the same as
** the value that was passed in.
*/
#define INSERT_DEF(TA, TB)                         \
static TA                                          \
_nrrdInsert##TA##TB(TB *v, size_t I, TA j) {       \
  return (TA)(v[I] = (TB)j);                       \
}
#define INSERT_LIST(TA, TB)                        \
  (TA (*)(void*, size_t, TA))_nrrdInsert##TA##TB,

MAP(INSERT_DEF, UI)
MAP(INSERT_DEF, JN)
MAP(INSERT_DEF, FL)
MAP(INSERT_DEF, DB)

unsigned int (*
nrrdUIInsert[NRRD_TYPE_MAX+1])(void *, size_t, unsigned int) = {
  NULL, MAP(INSERT_LIST, UI) NULL
};
int (*
nrrdIInsert[NRRD_TYPE_MAX+1])(void *, size_t, int) = {
  NULL, MAP(INSERT_LIST, JN) NULL
};
float (*
nrrdFInsert[NRRD_TYPE_MAX+1])(void *, size_t, float) = {
  NULL, MAP(INSERT_LIST, FL) NULL
};
double (*
nrrdDInsert[NRRD_TYPE_MAX+1])(void *, size_t, double) = {
  NULL, MAP(INSERT_LIST, DB) NULL
};

/*
******** nrrdSprint
**
** Dereferences pointer v and sprintf()s that value into given string s,
** returns the result of sprintf()
**
** There is obviously no provision for ensuring that the sprint'ing
** doesn't overflow the buffer, which is unfortunate...
*/
static int _nrrdSprintCH(char *s, const CH *v) { return sprintf(s, "%d", *v); }
static int _nrrdSprintUC(char *s, const UC *v) { return sprintf(s, "%u", *v); }
static int _nrrdSprintSH(char *s, const SH *v) { return sprintf(s, "%d", *v); }
static int _nrrdSprintUS(char *s, const US *v) { return sprintf(s, "%u", *v); }
static int _nrrdSprintIN(char *s, const JN *v) { return sprintf(s, "%d", *v); }
static int _nrrdSprintUI(char *s, const UI *v) { return sprintf(s, "%u", *v); }
static int _nrrdSprintLL(char *s, const LL *v) {
  return sprintf(s, AIR_LLONG_FMT, *v);
}
static int _nrrdSprintUL(char *s, const UL *v) {
  return sprintf(s, AIR_ULLONG_FMT, *v);
}
/* HEY: sizeof(float) and sizeof(double) assumed here, since we're
   basing "8" and "17" on 6 == FLT_DIG and 15 == DBL_DIG, which are
   digits of precision for floats and doubles, respectively */
static int _nrrdSprintFL(char *s, const FL *v) {
  return airSinglePrintf(NULL, s, "%.8g", (double)(*v)); }
static int _nrrdSprintDB(char *s, const DB *v) {
  return airSinglePrintf(NULL, s, "%.17g", *v); }
int (*
nrrdSprint[NRRD_TYPE_MAX+1])(char *, const void *) = {
  NULL,
  (int (*)(char *, const void *))_nrrdSprintCH,
  (int (*)(char *, const void *))_nrrdSprintUC,
  (int (*)(char *, const void *))_nrrdSprintSH,
  (int (*)(char *, const void *))_nrrdSprintUS,
  (int (*)(char *, const void *))_nrrdSprintIN,
  (int (*)(char *, const void *))_nrrdSprintUI,
  (int (*)(char *, const void *))_nrrdSprintLL,
  (int (*)(char *, const void *))_nrrdSprintUL,
  (int (*)(char *, const void *))_nrrdSprintFL,
  (int (*)(char *, const void *))_nrrdSprintDB,
  NULL};


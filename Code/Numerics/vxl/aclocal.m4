dnl ----------------------------------------------------------------------------
dnl  AC_VXL_UNISTD
dnl ---------------------------------------------------------------------------

AC_DEFUN(AC_VXL_UNISTD,[
echo "checking <unistd.h>..."

# first get preprocessed unistd.h :
cat > check_vxl_unistd.c <<EOF
#include <unistd.h>
EOF
eval "$ac_cpp check_vxl_unistd.c" 2>&5 > check_vxl_unistd.i;

# caveat: sometimes __useconds_t is defined, hence the space
if (egrep "typedef.* useconds_t;" check_vxl_unistd.i >/dev/null 2>&1); then
  VXL_UNISTD_HAS_USECONDS_T="1";
  echo "... for useconds_t... yes"
else
  VXL_UNISTD_HAS_USECONDS_T="0";
  echo "... for useconds_t... no"
fi; export VXL_UNISTD_HAS_USECONDS_T;

# caveat: sometimes __intptr_t is defined, hence the space
if (egrep "typedef.* intptr_t;" check_vxl_unistd.i >/dev/null 2>&1); then
  VXL_UNISTD_HAS_INTPTR_T="1";
  echo "... for intptr_t... yes"
else
  VXL_UNISTD_HAS_INTPTR_T="0";
  echo "... for intptr_t... no"
fi; export VXL_UNISTD_HAS_INTPTR_T;

echo "... if usleep() returns void"
AC_TRY_COMPILE(
[#include <unistd.h>
],[{ int x = usleep(0); }],[VXL_UNISTD_USLEEP_IS_VOID="0";],[VXL_UNISTD_USLEEP_IS_VOID="1";])
export VXL_UNISTD_USLEEP_IS_VOID;

rm -f check_vxl_unistd.c check_vxl_unistd.i
AC_CHECK_FUNC([ualarm],[VXL_UNISTD_HAS_UALARM=1],[VXL_UNISTD_HAS_UALARM=0])
AC_CHECK_FUNC([usleep],[VXL_UNISTD_HAS_USLEEP=1],[VXL_UNISTD_HAS_USLEEP=0])
AC_CHECK_FUNC([lchown],[VXL_UNISTD_HAS_LCHOWN=1],[VXL_UNISTD_HAS_LCHOWN=0])
AC_CHECK_FUNC([pread],[VXL_UNISTD_HAS_PREAD=1],[VXL_UNISTD_HAS_PREAD=0])
AC_CHECK_FUNC([pwrite],[VXL_UNISTD_HAS_PWRITE=1],[VXL_UNISTD_HAS_PWRITE=0])
AC_CHECK_FUNC([tell],[VXL_UNISTD_HAS_TELL=1],[VXL_UNISTD_HAS_TELL=0])

])

dnl ----------------------------------------------------------------------------
dnl  Usage: AC_VXL_WORDS

AC_DEFUN(AC_VXL_WORDS,[
AC_MSG_CHECKING( [for machine word sizes] )
cat > check_vxl_words.cc <<EOF
#include <stdio.h>
#include <limits.h>
#ifndef CHAR_BIT
# define CHAR_BIT 0
#endif

// this is a silly workaround. on most machines, the configure
// script will cat the 2-character sequence \" as a 2-character
// sequence. however, julia@robots.ox.ac.uk "expands it" to a
// single quote character first. the obvious solution, which is
// to add extra backslashes will fix it for julia, but break it
// for other machines. so to print a quote, we use its ascii
// value.
// note that the backslashes in the macro 'macro' are expanded
// by configure, but we dont care about that.
#define QUOTE 34

#define macro(NAME, n, cand) \
  if (CHAR_BIT==8 && sizeof(cand)==n) \
    printf(#NAME "=%c" #cand "%c;\n", QUOTE, QUOTE); \
  else \
    printf(#NAME "=%c" "void" "%c;\n", QUOTE, QUOTE); \
  printf("export " #NAME ";\n");

int main(int, char **) {
  macro(VXL_INT_8, 1, char);
  macro(VXL_INT_16, 2, short);
  macro(VXL_INT_32, 4, int);
  macro(VXL_INT_64, 8, long long);
  macro(VXL_IEEE_32, 4, float);
  macro(VXL_IEEE_64, 8, double);
//  macro(VXL_IEEE_96, 12, long double);  // x86
//  macro(VXL_IEEE_128, 16, long double); // sparc, mips
  return 0;
}
EOF
if eval "$CXX ./check_vxl_words.cc -o ./check_vxl_words.exe" 2>&5; then
  eval `./check_vxl_words.exe` 2>&5
  AC_MSG_RESULT( ok )
else
  AC_MSG_RESULT( error )
fi
rm -f ./check_vxl_words.*
])

dnl ----------------------------------------------------------------------------
dnl  Usage: AC_VXL_HAS_QSORT : do we have a qsort() function?
dnl
dnl ---------------------------------------------------------------------------

AC_DEFUN(AC_VXL_HAS_QSORT,[
AC_CACHE_CHECK(whether we have a working qsort,ac_vxl_has_qsort,[
AC_LANG_SAVE
AC_LANG_C #PLUSPLUS
AC_TRY_COMPILE(
[
/* This is not a C++ header, strictly speaking. */
/* Actually, it is normative but deprecated, strictly speaking :) */
#include <stdlib.h>
int f(const void *a,const void *b) { return 1; }
/* configure provides its own main(), so putting one here 
   makes the test fail even if qsort() is available */
int not_main(void) { int a[5]; qsort(a, 5, sizeof(int), f); return 0; }
],,ac_vxl_has_qsort=yes,ac_vxl_has_qsort=no)
AC_LANG_RESTORE
 ])
if test $ac_vxl_has_qsort = yes ; then
  VXL_STDLIB_HAS_QSORT="1";
else
  VXL_STDLIB_HAS_QSORT="0";
fi;
export VXL_STDLIB_HAS_QSORT
])
dnl


dnl ----------------------------------------------------------------------------
dnl Usage AC_TWO_ARG_TIMEOFDAY : check for one or two argument gettimeofday
dnl                              also sets TIME_WITH_SYS_TIME and HAVE_SYS_TIME_H
dnl
dnl ---------------------------------------------------------------------------


AC_DEFUN(AC_TWO_ARG_TIMEOFDAY,[
# these must go first or the msg_result will get clobbered.
AC_HEADER_TIME
AC_STRUCT_TM

AC_CACHE_CHECK( whether gettimeofday takes two arguments,ac_twoarg_timeofday,
[AC_LANG_SAVE
AC_LANG_CPLUSPLUS
AC_TRY_RUN([
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
int main()
{
  struct timeval real0;
  struct timezone tz;
  gettimeofday(&real0, &tz);
  return 0;
}],[
ac_twoarg_timeofday=yes
],[
ac_twoarg_timeofday=no
])
AC_LANG_RESTORE])

])


dnl ------------------------------------------------------------
AC_DEFUN(AC_VXL_MATH_HAS_FINITE,[
AC_MSG_CHECKING([whether <math.h> provides finite()])
AC_LANG_CPLUSPLUS
AC_TRY_COMPILE([
#include <math.h>
int vxl_finite(double x) { return finite(x); }
],,[
VXL_MATH_HAS_FINITE=1
AC_MSG_RESULT(yes)
],[
VXL_MATH_HAS_FINITE=0
AC_MSG_RESULT(no)
])
AC_LANG_RESTORE
export VXL_MATH_HAS_FINITE
])



dnl ------------------------------------------------------------
AC_DEFUN(AC_VXL_IEEEFP_HAS_FINITE,[
AC_MSG_CHECKING([whether <ieeefp.h> provides finite()])
AC_LANG_CPLUSPLUS
AC_TRY_COMPILE([
#include <ieeefp.h>
int vxl_finite(double x) { return finite(x); }
],,[
VXL_IEEEFP_HAS_FINITE=1
AC_MSG_RESULT(yes)
],[
VXL_IEEEFP_HAS_FINITE=0
AC_MSG_RESULT(no)
])
AC_LANG_RESTORE
export VXL_IEEEFP_HAS_FINITE
])



dnl ------------------------------------------------------------
AC_DEFUN(AC_VXL_STDLIB_RAND48,[
AC_LANG_CPLUSPLUS

AC_MSG_CHECKING([whether <stdlib.h> provides lrand48()])
AC_TRY_COMPILE([
#include <stdlib.h>
int vxl_lrand48() { return lrand48(); }
],,[
VXL_STDLIB_HAS_LRAND48=1
AC_MSG_RESULT(yes)
],[
VXL_STDLIB_HAS_LRAND48=0
AC_MSG_RESULT(no)
])
export VXL_STDLIB_HAS_LRAND48

AC_MSG_CHECKING([whether <stdlib.h> provides drand48()])
AC_TRY_COMPILE([
#include <stdlib.h>
double vxl_drand48() { return drand48(); }
],,[
VXL_STDLIB_HAS_DRAND48=1
AC_MSG_RESULT(yes)
],[
VXL_STDLIB_HAS_DRAND48=0
AC_MSG_RESULT(no)
])
export VXL_STDLIB_HAS_DRAND48

AC_LANG_RESTORE
])

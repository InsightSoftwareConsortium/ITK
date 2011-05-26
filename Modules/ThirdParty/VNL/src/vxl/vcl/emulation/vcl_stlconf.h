#define VCL_EMULATION_STLCONF_H_INCLUDED

// TargetJr and the IUE use abbreviated STL class names:
#define  __STL_USE_ABBREVS 1

#if defined(__sgi) && !defined(__GNUC__)
#   if (_COMPILER_VERSION >= 700)
#     define __SGI_CC_7
#   else
#     define __SGI_CC_6
#   endif
#endif

#ifdef __GNUC__
# if (__GNUC__ > 2)
#  include "egcs-stlconf.h"
# elif (__GNUC__ == 2) && (__GNUC_MINOR__ == 8)
#  include "gcc-281-stlconf.h"
# elif (__GNUC__ == 2) && (__GNUC_MINOR__ == 95)
#  include "gcc-295-stlconf.h"
# elif (__GNUC__ == 2) && (__GNUC_MINOR__ > 8)
#  include "egcs-stlconf.h"
# else
#  include "gcc-272-stlconf.h"
# endif
#else
# ifdef WIN32
#  include "win32-vc50-stlconf.h"
# else
#  if defined (__SGI_CC_6) || defined (__SGI_CC_7)
#   include "sgi-CC-stlconf.h"
#  else
#   if defined (__SUNPRO_CC)
#    if __SUNPRO_CC < 0x500
#      include "sun-CC4.1-stlconf.h"
#    else
#      include "sun-CC5.0-stlconf.h"
#    endif
#   else
#    error "Please create an appropriate stlconf.h file"
#   endif
#  endif
# endif
#endif

// EGCS doesn't like definition of default types, viz:
//   template <class A = default> class vector;
//   template <class A = default> class vector { ... };
// This macro is used to say "define if not previously defined, like
//   template <__DFL_TYPE_PARAM_STLDECL(A,a)> class vector { ... };
#ifndef __DFL_TYPE_PARAM_STLDECL
#define __DFL_TYPE_PARAM_STLDECL(A,a) __DFL_TYPE_PARAM(A,a)
#endif

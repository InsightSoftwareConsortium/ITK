/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Copyright (c) 1997
 * Silicon Graphics
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Copyright (c) 1997
 * Moscow Center for SPARC Technology
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Moscow Center for SPARC Technology makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */


#ifndef vcl_emulation_bool_h
#define vcl_emulation_bool_h

// include compiler settings
#include "vcl_stlconf.h"

// some final tweaking ( should not be in stlconf.h to avoid
// clobbering by configure

#if ! defined ( __STL_NAMESPACES )
# define __STL_NO_NAMESPACES 1
#endif

#if ! defined ( __STL_USE_EXCEPTIONS )
# undef  __STL_NO_EXCEPTIONS
# define __STL_NO_EXCEPTIONS 1
#endif

#if defined ( __STL_NO_EXCEPTIONS )
# undef __STL_USE_EXCEPTIONS
#endif

#if defined  (__STL_WIN32THREADS) && !defined (_NOTHREADS) \
 && !defined(__STL_USE_MALLOC) && !defined (__STL_USE_NEWALLOC) && !defined (__STL_BOOL_KEYWORD)
# define __STL_WINDOWS_H_INCLUDED
# define NOMINMAX
# define Arg rpc_Arg
#ifndef WIN32_LEAN_AND_MEAN
 #define WIN32_LEAN_AND_MEAN
#endif
#   include <windows.h>
#   undef Arg
#   undef min
#   undef max
//  This must precede bool.h
#endif

#ifndef bool
        // <awf> : target handles bool anyway
# if defined(__STL_YVALS_H)
#  include <yvals.h>
# else
#  if ! defined(__STL_BOOL_KEYWORD)
#   if defined (__STL_RESERVED_BOOL_KEYWORD)
#    define bool int
#   else
        typedef int bool;
#   endif
#    define true 1
#    define false 0
#  endif /* __STL_BOOL_KEYWORD */
# endif
#endif

#undef __STL_BOOL_KEYWORD
#undef __STL_RESERVED_BOOL_KEYWORD
#undef __STL_YVALS_H
#undef __STL_LOOP_INLINE_PROBLEMS
#undef __STL_TYPENAME
#undef __STL_EXPLICIT
#undef __AUTO_CONFIGURED
#undef __STL_FULL_SPEC_SYNTAX

#endif // vcl_emulation_bool_h

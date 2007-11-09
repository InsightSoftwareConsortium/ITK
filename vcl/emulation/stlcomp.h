/*
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

#ifndef __STLCOMP_H
# define __STLCOMP_H

//==========================================================
// Features selection

// Uncomment that to disable exception handling
// #  define __STL_NO_EXCEPTIONS 1

// Uncomment that to disable std namespace usage
// #  define __STL_NO_NAMESPACES 1

// Comment that to not include defalloc.h ( no defaults changed )
#  define    __STL_USE_DEFALLOC   1

// Uncomment that to to use new-based allocator as default
// #  define    __STL_USE_NEWALLOC   1

// Uncomment this to use malloc-based allocator as default
// #  define __STL_USE_MALLOC 1

// Uncomment this to disable using std by default
// #  define __STL_NO_USING_STD 1

// define __STL_USE_ABBREVS if your linker has trouble with long
// external symbols
// # define  __STL_USE_ABBREVS 1

// set this to force checked allocators
// #  define __STL_DEBUG_ALLOC 1

// Mostly correct guess
#  define __STL_UINT32_T unsigned long

//==========================================================

//==========================================================
// default values of autoconf  flags
//==========================================================

// the values choosen here as defaults try to give
// maximum functionality on the most conservative settings

// Uncomment this if your compiler supports "bool"
// #  define  __STL_BOOL_KEYWORD 1

// Uncomment this if your compiler has "bool" keyword reserved
// #  define  __STL_RESERVED_BOOL_KEYWORD 1

// Comment this if your compiler doesn't support that
#  define __STL_DEFAULT_TEMPLATE_PARAM 1
// Uncomment this if your compiler support only complete types as
// default parameters
// #  define __STL_DEFAULT_TYPE_PARAM 1

// Comment this if your compiler lacks static data
// members template declarations
// Uncomment next line if your compiler supports __attribute__((weak))
#  define __STL_STATIC_TEMPLATE_DATA 1
// #  define __STL_WEAK_ATTRIBUTE 1

// Uncomment this if your C library has lrand48() function
// #  define __STL_RAND48 1
// Uncomment this if your compiler can't inline while(), for()
// #  define __STL_LOOP_INLINE_PROBLEMS 1

// Uncomment this if your compiler supports namespaces
// #  define __STL_NAMESPACES 1

// Uncomment this if your compiler supports typename
// #  define __STL_TYPENAME 1

// Uncomment this if your compiler supports mutable
// #  define __STL_MUTABLE 1

// Uncomment if const_cast<> is available
// #  define __STL_NEW_STYLE_CASTS 1

// Uncomment this if your compiler supports explicit constructors
// #  define __STL_EXPLICIT 1

// Uncomment this if your compiler supports exceptions
// #  define __STL_EXCEPTIONS 1

// Uncomment this if your compiler supports exception specifications
// with reduced overhead ( e.g. inlines them, not vice versa)
// #  define __STL_EXCEPTION_SPEC

// Uncomment if long long is available
// #  define __STL_LONG_LONG 1

// Uncomment this for wchar_t functinality
// #  define __STL_WCHAR_T  1

// Uncomment if needed full  specialization syntax : template <> struct ....
// #  define __STL_FULL_SPEC_SYNTAX  1

// Uncomment if bad_alloc defined in <new>
// #  define __STL_BAD_ALLOC_DEFINED  1

// Uncomment if member templates available
// #  define __STL_MEMBER_TEMPLATES   1

// Uncomment if member templates available
// #  define __STL_FRIEND_TEMPLATES   1

// Uncomment if available
// #  define __STL_CLASS_PARTIAL_SPECIALIZATION 1

// Uncomment if available
// #  define __STL_FUNC_PARTIAL_ORDERING 1

// Uncomment if available
// #  define __STL_AUTOMATIC_TYPE_TRAITS 1

// Uncomment if getting errors compiling mem_fun* adaptors
// #  define __STL_MEMBER_POINTER_PARAM_BUG 1

// All these settings don't affect performance/functionality
// Comment them if your compiler has no problems.
#  define __STL_BASE_MATCH_BUG          1
// #  define __STL_NESTED_TYPE_PARAM_BUG   1
#  define __STL_UNUSED_REQUIRED_BUG     1
#  define __STL_UNINITIALIZABLE_PRIVATE  1
#  define __STL_BASE_TYPEDEF_OUTSIDE_BUG 1
#  define __STL_CONST_CONSTRUCTOR_BUG    1
// if your compiler have serious problems with typedefs, try this one
// #  define __STL_BASE_TYPEDEF_BUG          1
//==========================================================

//==========================================================
// per-version compiler features recognition
//==========================================================

// reporting of incompatibility
#  define __GIVE_UP_WITH_STL(message) void give_up() \
   { upgrade_the_compiler_to_use_STL;}


# if defined(__sgi) && !defined(__GNUC__)
#   if defined(_BOOL) || ! ((_MIPS_ISA < 2) || defined (_ABIO32))
#     define __STL_BOOL_KEYWORD
#   endif
#   if defined(_TYPENAME)
#     define __STL_TYPENAME
#   endif
#   ifdef _PARTIAL_SPECIALIZATION_OF_CLASS_TEMPLATES
#     define __STL_CLASS_PARTIAL_SPECIALIZATION
#   endif
#   ifdef _MEMBER_TEMPLATES
#     define __STL_MEMBER_TEMPLATES
#   endif
#   ifdef __EXCEPTIONS
#     define __STL_USE_EXCEPTIONS
#   endif
#   if !defined(_NOTHREADS) && !defined(_PTHREADS)
#     define __STL_SGI_THREADS
#   endif
# endif


// AIX xlC, is there more specific define ?
#if defined(_AIX)
#  define __STL_RESERVED_BOOL_KEYWORD 1
#  undef  __STL_DEFAULT_TEMPLATE_PARAM
#  undef  __STL_DEFAULT_TYPE_PARAM
#  undef  __STL_NAMESPACES
#  undef  __STL_UNINITIALIZABLE_PRIVATE
#  define __STL_UNINITIALIZABLE_PRIVATE 1
#  define __STL_BASE_TYPEDEF_OUTSIDE_BUG 1
#  undef  __STL_CONST_CONSTRUCTOR_BUG
#  define __STL_CONST_CONSTRUCTOR_BUG 1
#endif

// Microsoft Visual C++ 4.0, 4.1, 4.2, 5.0
# if defined(_MSC_VER)
// comment this one to try experimantal allocator
// #  define __STL_USE_NEWALLOC  1
#  undef  __STL_BOOL_KEYWORD
#  undef  __STL_UNINITIALIZABLE_PRIVATE
#  undef  __STL_BASE_MATCH_BUG
#  undef  __STL_DEFAULT_TEMPLATE_PARAM
#   ifdef _CPPUNWIND
#     define __STL_USE_EXCEPTIONS
#   endif
#   if defined ( _MT )
#     define __STL_WIN32THREADS
#   endif
#  if ( _MSC_VER>=1000 )
#   define __STL_NAMESPACES             1
#   define __STL_NEW_STYLE_CASTS        1
#   undef  __STL_CONST_CONSTRUCTOR_BUG
#   define __STL_CONST_CONSTRUCTOR_BUG  1
#   define __STL_LONG_DOUBLE            1
#   if ( _MSC_VER<=1010 )
// "bool" is reserved in MSVC 4.1 while <yvals.h> absent, so :
#   define __STL_RESERVED_BOOL_KEYWORD 1
#   define __STL_USE_ABBREVS           1
#   else
#    define __STL_YVALS_H 1
#    define __STL_BAD_ALLOC_DEFINED 1
#   endif
#  endif
#  if (_MSC_VER >= 1100)  // MSVC 5.0
#    define __STL_DEFAULT_TEMPLATE_PARAM 1
#    define __STL_TYPENAME      1
#    define __STL_EXPLICIT      1
#    define __STL_MUTABLE       1
#  endif
# endif

// Borland C++ ( 5.x )
# if defined ( __BORLANDC__ )
#  undef  __STL_UNINITIALIZABLE_PRIVATE
#  undef  __STL_DEFAULT_TEMPLATE_PARAM
#  if ( __BORLANDC__ < 0x500 )
#   undef  __STL_BOOL_KEYWORD
#   undef  __STL_NAMESPACES
#   undef  __STL_DEFAULT_TEMPLATE_PARAM
#   undef  __STL_NESTED_TYPE_PARAM_BUG
#   undef  __STL_BASE_MATCH_BUG
#   define __STL_NESTED_TYPE_PARAM_BUG 1
#   define __STL_BASE_MATCH_BUG        1
#  else
#   define __STL_BOOL_KEYWORD 1
#   define __STL_DEFAULT_TYPE_PARAM 1
#   define __STL_NAMESPACES 1
#   define __STL_EXPLICIT   1
#   define __STL_TYPENAME   1
#   define __STL_USE_EXCEPTIONS 1
#   define __STL_NEW_STYLE_CASTS
#   define __STL_LONG_DOUBLE 1
#   define __STL_MUTABLE 1
#   define __STL_WCHAR_T 1
#   define __STL_NEW_HEADER_NAMES 1
#   undef  __STL_CONST_CONSTRUCTOR_BUG
#   define __STL_CONST_CONSTRUCTOR_BUG 1
#  endif
#  undef  __STL_LOOP_INLINE_PROBLEMS
#  define __STL_LOOP_INLINE_PROBLEMS 1
// empty exception spec make things worse in BC, so:
#  undef __STL_EXCEPTION_SPEC
# endif

# if defined(__SUNPRO_CC)
#  if ( __SUNPRO_CC <= 0x420 )
   // SUNPro C++ 4.1 and above
#   undef  __STL_BOOL_KEYWORD
#   undef  __STL_DEFAULT_TEMPLATE_PARAM
#   undef  __STL_NAMESPACES
#   define __STL_USE_EXCEPTIONS     1
#   undef  __STL_EXCEPTION_SPEC
#   define __STL_EXCEPTION_SPEC 1
#   undef  __STL_UNINITIALIZABLE_PRIVATE
#   define __STL_UNINITIALIZABLE_PRIVATE 1
#   define __STL_LONG_LONG  1
#   define __STL_WCHAR_T  1
   // SUNPro C++ prior to 4.1
#   if ( __SUNPRO_CC < 0x410 )
   // hard times ;(
#   define __STL_BASE_MATCH_BUG          1
#   define __STL_BASE_TYPEDEF_BUG        1
#     if ( __SUNPRO_CC < 0x401 )
        __GIVE_UP_WITH_STL(SUNPRO_401)
#     endif
#   else
#    if ( __SUNPRO_CC >= 0x420 )
#     define __STL_FULL_SPEC_SYNTAX 1
#    endif
#   endif

#  endif
# endif

// g++ 2.7.x and above
# if defined (__GNUC__ )
#  undef   __STL_UNINITIALIZABLE_PRIVATE
#  define  __STL_BOOL_KEYWORD 1
// cygnus have a lot of version, let's assume the best.
// no specific definitions known except this one
#  if defined (__CYGWIN32__)
#   define __CYGNUS_GCC__
#  endif

#  if ! ( __GNUC__ > 2 || __GNUC_MINOR__ > 7 || defined (__CYGNUS_GCC__) )
// Will it work with 2.6 ? I doubt it.
#   if ( __GNUC_MINOR__ < 7 )
    __GIVE_UP_WITH_STL(GCC_272);
#   endif
#   undef  __STL_NAMESPACES
#   undef  __STL_DEFAULT_TEMPLATE_PARAM
#   define __STL_DEFAULT_TYPE_PARAM 1
#   undef  __STL_STATIC_TEMPLATE_DATA
#   define __STL_NESTED_TYPE_PARAM_BUG   1
#   undef  __STL_STATIC_TEMPLATE_DATA
#   define __STL_BASE_MATCH_BUG       1
//  unused operators are required (forward)
#   undef  __STL_EXPLICIT
#   define __STL_EXPLICIT 1
#   undef  __STL_UNINITIALIZABLE_PRIVATE
#   define __STL_UNINITIALIZABLE_PRIVATE 1
#   undef  __STL_CONST_CONSTRUCTOR_BUG
#   undef  __STL_LONG_LONG
#   undef  __STL_WCHAR_T
#   define __STL_LONG_LONG  1
#   define __STL_WCHAR_T  1
#   define __STL_MUTABLE 1
#   define __STL_NEW_STYLE_CASTS 1
// default for gcc-2.7.2 is no exceptions, let's follow it
#  endif /* __GNUC__ > 2 */

// cygnus gcc may be as advanced as that
#  if defined ( __CYGNUS_GCC__ )
#   undef  __STL_DEFAULT_TEMPLATE_PARAM
#   define __STL_DEFAULT_TEMPLATE_PARAM 1
#   undef  __STL_STATIC_TEMPLATE_DATA
#   define __STL_STATIC_TEMPLATE_DATA   1
#   undef  __STL_NAMESPACES
#   define __STL_EXPLICIT   1
#   define __STL_TYPENAME   1
#  endif

// static template data members workaround strategy for gcc tries
// to use weak symbols.
// if you don't want to use that, #define __STL_WEAK_ATTRIBUTE=0 ( you'll
// have to put "#define __PUT_STATIC_DATA_MEMBERS_HERE" line in one of your
// compilation unit ( or CFLAGS for it ) _before_ including any STL header ).
#  if !(defined (__STL_STATIC_TEMPLATE_DATA) || defined (__STL_WEAK_ATTRIBUTE ))
// systems using GNU ld or format that supports weak symbols
// may use "weak" attribute
// Linux & Solaris ( x86 & SPARC ) are being auto-recognized here
#   if defined(__STL_GNU_LD) || defined(__ELF__) || \
    (( defined (__SVR4) || defined ( __svr4__ )) && \
     ( defined (sun) || defined ( __sun__ )))
#    define __STL_WEAK_ATTRIBUTE 1
#   endif
#  endif /* __STL_WEAK_ATTRIBUTE */

# endif /* __GNUC__ */


# if defined (__WATCOM_CPLUSPLUS__)
#  if (__WATCOM_CPLUSPLUS__ >= 1100 )
// Can define if you enable /xs compiler option
//#   if 0
//#    define __STL_EXCEPTIONS 1
//#    define __STL_EXCEPTION_SPEC 1
//#   else
//#    undef  __STL_EXCEPTIONS
//#    undef  __STL_EXCEPTION_SPEC
//#   endif
#   define __STL_NESTED_TYPE_PARAM_BUG 1
#   define __STL_BOOL_KEYWORD 1
#   undef  __STL_DEFAULT_TEMPLATE_PARAM
#   define __STL_STATIC_TEMPLATE_DATA 1
#   define __STL_EXPLICIT 1
#   undef  __STL_BASE_MATCH_BUG
#   undef  __STL_BASE_TYPEDEF_BUG
#   define __STL_BASE_TYPEDEF_OUTSIDE_BUG 1
#   undef  __STL_UNINITIALIZABLE_PRIVATE
#   define __STL_CONST_CONSTRUCTOR_BUG 1
#   define __STL_NEW_HEADER_NAMES 1
#   define __STL_LONG_DOUBLE 1
#   define __STL_MUTABLE 1
#   define __STL_NEW_STYLE_CASTS 1
#   undef  __STL_UNUSED_REQUIRED_BUG
#  endif
# endif /* __WATCOM_CPLUSPLUS__ */

# undef __GIVE_UP_WITH_STL

#endif // __STLCOMP_H

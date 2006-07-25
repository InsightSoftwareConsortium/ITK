#ifndef vcl_compiler_h_
#define vcl_compiler_h_
//:
// \file
// \brief Uniform macro definition scheme for finding out about the compiler
//
// It's much better to determine the compiler automatically here than to depend
// on command-line flags being set.

// Be careful when modifying this file. In general, you need to make
// sure that exactly one of the preprocessor flags is defined. For
// example, if the compiler is GCC 3.4.2, then VCL_GCC should be
// defined, VCL_GCC_3 should be defined, and VCL_GCC_34 should be
// defined. Others, like VCL_GCC_33 *should not* be defined.
//
// Note that this is most commonly implemented using a cascade of if
// statements. Be careful to add your statements to the correct place
// in the cascade list.
//
// Naming scheme:
// If you have a compiler name XYZ, then
//     #define VCL_XYZ
// Each each major release, define a release number
//     #define VCL_XYZ_4
// Avoid using the marketing name for the release number, because it's
// harder to follow. For example, Microsoft Visual C++ .NET 2003 is
// better called Visual C++ 7.
// For each minor version, define the appropriate minor version number
//     #define VCL_XYZ_40
// If necessary, define the patchlevel too:
//     #define VCL_XYZ_401
//
// Make sure that if the minor version is defined, then the release
// number and the compiler name are also defined.
//
// Add the corresponding test to tests/test_platform to make sure.

#if defined(__sgi) && !defined(__GNUC__)
# ifndef _COMPILER_VERSION
#  define VCL_SGI_CC_6
# else
#  if (_COMPILER_VERSION >= 700)
#   define VCL_SGI_CC_7
#  else
#   define VCL_SGI_CC_6
#  endif
#  if   (_COMPILER_VERSION >= 730)
#   define VCL_SGI_CC_730
// 1116  Non-void function "std::__malloc_alloc_template<0>::_S_oom_malloc" (declared
//    at line 163) should return a value.
// 3505  setjmp not marked as unknown_control_flow because it is not declared as a
// 3438 ceil not marked as no_side_effects because it is not declared as a function
#   pragma set woff 3438
#   pragma set woff 1116
#   pragma set woff 3505
#  elif (_COMPILER_VERSION >= 720)
#   define VCL_SGI_CC_720
#  endif
#  define VCL_SGI_CC
# endif
#endif

#if defined(__SUNPRO_CC)
# define VCL_SUNPRO_CC
# if (__SUNPRO_CC>=0x500)
#  if (__SUNPRO_CC>=0x560)
#   define VCL_SUNPRO_CC_56
#   undef VCL_SUNPRO_CC_50
#  else
#   define VCL_SUNPRO_CC_50
#   undef VCL_SUNPRO_CC_56
#  endif
#  define VCL_SUNPRO_CC_5
# else
#  undef VCL_SUNPRO_CC_5
# endif
# ifdef INSTANTIATE_TEMPLATES
#  define _RWSTD_COMPILE_INSTANTIATE
# endif
#endif

#if defined(__GNUC__) && !defined(__ICC) // icc 8.0 defines __GNUC__
# define VCL_GCC
# if (__GNUC__<=1)
#  error "forget it."
# elif (__GNUC__==2)
#  if (__GNUC_MINOR__>=100)
#   error "I need some help here."
#  elif (__GNUC_MINOR__>=95)
#   define VCL_GCC_295
#  elif (__GNUC_MINOR__>8)
#   define VCL_EGCS
#  elif (__GNUC_MINOR__>7)
#   define VCL_GCC_28
#  elif (__GNUC_MINOR__>6)
#   define VCL_GCC_27
#  endif
#  if (__GNUC_MINOR__>7)
#   define VCL_GCC_EGCS // so this is the union of EGCS, GCC_28 and GCC_295
#  endif
# elif (__GNUC__==3)
#  define VCL_GCC_3
#  if (__GNUC_MINOR__ > 3 )
#   define VCL_GCC_34
#  elif (__GNUC_MINOR__ > 2 )
#   define VCL_GCC_33
#  elif (__GNUC_MINOR__ > 1 )
#   define VCL_GCC_32
#  elif (__GNUC_MINOR__ > 0 )
#   define VCL_GCC_31
#  else
#   define VCL_GCC_30
#  endif
# elif (__GNUC__==4)
#  define VCL_GCC_4
#  if (__GNUC_MINOR__ > 0 )
#   define VCL_GCC_41
#  else
#   define VCL_GCC_40
#  endif
# else
#  error "Dunno about this gcc"
# endif
#endif

#if defined(_WIN32) || defined(WIN32)
# define VCL_WIN32
# if defined(_MSC_VER)
#  define VCL_VC
#  if _MSC_VER >= 1300
#   define VCL_VC_DOTNET 1 // VC is at least version >= 7.0
#  endif
#  if _MSC_VER >= 1400     // .NET 2005 = Version 8.x
#   define _CRT_SECURE_NO_DEPRECATE 1
#   define VCL_VC_8
#   if _MSC_VER >= 1400
#    define VCL_VC_80 1    // version 8.0
#    define VCL_VC80       // (deprecated)
#   endif
#  elif _MSC_VER >= 1300   // .NET 2003 = Version 7.x
#   define VCL_VC_7
#   if _MSC_VER >= 1310
#    define VCL_VC_71      // Version 7.1
#    define VCL_VC71 1     // (deprecated)
#   else
#    define VCL_VC_70      // Version 7.0
#    define VCL_VC70 1     // (deprecated)
#   endif
#  elif _MSC_VER >= 1200   // pre- .NET, Version 6.x
#   define VCL_VC_6
#   define VCL_VC_60       // Version 6.0
#   define VCL_VC60 1      // (deprecated)
#  else
#   define VCL_VC_5
#   define VCL_VC_50       // Version 5.0
#   define VCL_VC50 1      // (deprecated)
#  endif
# elif defined(__BORLANDC__)
#  define VCL_BORLAND
#  if __BORLANDC__ >= 0x0500
#   define VCL_BORLAND_5
#  endif
#  if __BORLANDC__ >= 0x0570
#   define VCL_BORLAND_57
#  elif __BORLANDC__ >= 0x0560
#   define VCL_BORLAND_56
#  elif __BORLANDC__ >= 0x0550
#   define VCL_BORLAND_55
#  endif
# endif
#endif

// win32 or vc++ ?
// awf hack alert:
#ifdef VCL_VC
#  ifdef VCL_VC_60
#    pragma warning(disable:4786 4660 4661)
#    pragma warning(disable:4786 4660 4355 4390)
#  elif VCL_VC_DOTNET
// 4786: 'identifier' : identifier was truncated to 'number' characters in the debug information
// 4018: signed/unsigned mismatch
// 4146: unary minus operator applied to unsigned type, result still unsigned
// 4267: conversion related to size_t
// 4355: 'this' : used in base member initializer list
#    pragma warning(disable:4786 4355)
#    pragma warning(disable:4018 4146 4267)
#  endif
#endif

#if defined(__KCC) // KAI compiler
# define VCL_KAI
#endif

#if defined(__ICC) ||defined(__ECC) // Intel compiler?
# define VCL_ICC
#  if __ICC >= 800
#   define VCL_ICC_8
#   if __ICC >= 810
#    define VCL_ICC_81
#   elif __ICC >= 800
#    define VCL_ICC_80
#   else
#    #error "Err.. ICC 8.x starts with ICC 8.0..."
#   endif
#  endif
#endif

#if defined(como4301) // Comeau C/C++ 4.3.0.1
# define VCL_COMO
#endif


#if defined(__MWERKS__)
// [sic]
# define VCL_METRO_WERKS
#endif

// include header files generated by configure.
#include <vcl_config_manual.h>
#include <vcl_config_compiler.h>
#include <vcl_config_headers.h>

// This *needs* to come after vcl_config_headers.h
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)
# if defined(VCL_GCC_3) || defined(VCL_GCC_4)
#  define GNU_LIBSTDCXX_V3 1
# elif !defined(GNU_LIBSTDCXX_V3) && defined(VCL_GCC_295) && VCL_CXX_HAS_HEADER_ISTREAM
// One difference between v2 and v3 is that the former has
// no <istream> header file whereas v3 has the lot.
#  define GNU_LIBSTDCXX_V3 1
# endif
#endif

// -------------------- default template parameters
#if VCL_CAN_DO_COMPLETE_DEFAULT_TYPE_PARAMETER
# define VCL_DFL_TYPE_PARAM_STLDECL(A, a) class A = a
#else
# define VCL_DFL_TYPE_PARAM_STLDECL(A, a) class A
#endif

#if VCL_CAN_DO_TEMPLATE_DEFAULT_TYPE_PARAMETER
# define VCL_DFL_TMPL_PARAM_STLDECL(A, a) class A = a
#else
# define VCL_DFL_TMPL_PARAM_STLDECL(A, a) class A
#endif

#define VCL_DFL_TMPL_ARG(classname) , classname

#if VCL_USE_NATIVE_STL
# define VCL_SUNPRO_ALLOCATOR_HACK(T) T VCL_SUNPRO_CLASS_SCOPE_HACK(std::allocator<T >)
#else
# define VCL_SUNPRO_ALLOCATOR_HACK(T) T // FIXME
#endif

//-------------------- template instantiation

// if the compiler doesn't understand "export", we just leave it out.
// gcc and SunPro 5.0 understand it, but they ignore it noisily.
#if !VCL_HAS_EXPORT||defined(VCL_EGCS)||defined(VCL_GCC_295)||defined(VCL_GCC_3)||defined(VCL_GCC_4)||defined(VCL_SUNPRO_CC_5)
# define export /* ignore */
#endif

#if VCL_NEEDS_INLINE_INSTANTIATION
# define VCL_INSTANTIATE_INLINE(symbol) template symbol
#else
# define VCL_INSTANTIATE_INLINE(symbol) /* */
#endif

// work-around to get template instantiation to work correctly with SunPro
// check flag to turn on inlining
#undef IUEi_STL_INLINE
#if defined(INLINE_EXPLICIT_FLAG) && defined(VCL_SUNPRO_CC) && defined(INSTANTIATE_TEMPLATES)
# define IUEi_STL_INLINE
#else
# define IUEi_STL_INLINE inline
#endif

//--------------------------------------------------------------------------------

// work-around to deal with some cases where some compilers (and the standard)
// requires an explicit typename qualifier. MSVC6.0 on the other had cannot cope
// with a typename in those places.
// VCL_DISAPPEARING_TYPENAME should only be used where either a hardcoded use
// or abscence of "typename" does not work over all platforms.

#if defined(VCL_VC_60) || !VCL_HAS_TYPENAME
# define VCL_DISAPPEARING_TYPENAME /* */
#else
# define VCL_DISAPPEARING_TYPENAME typename
#endif

//--------------------------------------------------------------------------------

#if VCL_FOR_SCOPE_HACK
# undef for
# define for if (false) { } else for
typedef int saw_VCL_FOR_SCOPE_HACK;
#endif

// fix to instantiate template functions
#define VCL_INSTANTIATE_NONINLINE(fn_decl) template fn_decl

// -------------------- handy macros

//: VCL_COMMA
//
// Handy for passing things with commas in them to CPP macros.  e.g.
// DO_MACRO(pair<A,B>) can be replaced by DO_MACRO(pair<A VCL_COMMA B>).
#define VCL_COMMA ,


//: VCL_VOID_RETURN
//
// VCL_VOID_RETURN is used as a return type where void is expected,
// as in return VCL_VOID_RETURN;
#define VCL_VOID_RETURN /*empty*/

//----------------------------------------------------------------------------
// Macros for safe-bool idiom.
#ifdef VCL_BORLAND
# define VCL_SAFE_BOOL_TRUE true
# define VCL_SAFE_BOOL_DEFINE typedef bool safe_bool
#else
# define VCL_SAFE_BOOL_TRUE (&safe_bool_dummy::dummy)
# define VCL_SAFE_BOOL_DEFINE \
   struct safe_bool_dummy { void dummy() {} }; \
   typedef void (safe_bool_dummy::* safe_bool)()
#endif

#endif // vcl_compiler_h_

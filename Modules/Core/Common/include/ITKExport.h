/*
 The rules to follow are

Use ModuleName_EXPORT for non-templated classes.
Use ITK_TEMPLATE_EXPORT for templated classes.
Use ITK_FORWARD_EXPORT for forward declarations.


This supports the all the combinations of
  macOS / Linux / Windows
     BUILD_SHARED_LIBS ON and OFF
     Explicit and implicit template instantiation
     CMAKE_CXX_VISIBILITY_PRESET set to hidden -> -fvisibility=hidden
     CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS:BOOL=ON
*/


#ifndef ITK_TEMPLATE_EXPORT_H
#define ITK_TEMPLATE_EXPORT_H

/* Below nearly identical to what is produced
 * by cmake generate_export_header function
 * with the exception of the manually set
 * ITK_TEMPLATE_EXPLICIT_EXPORTS variable that
 * is not linked with a library name.
 */

#ifdef ITK_STATIC
#  define ITK_TEMPLATE_EXPORT
#  define ITK_TEMPLATE_NO_EXPORT
#else
#  ifndef ITK_TEMPLATE_EXPORT
     /* DEFINE ITK_TEMPLATE_EXPLICIT_EXPORTS in a .cxx file BEFORE
      * including any header files to change the ITK_TEMPLATE_EXPORT
      * for windows from dllimport to dllexport when using explicit
      * instantiations.
      */
#    ifdef ITK_TEMPLATE_EXPLICIT_EXPORTS
        /* We are building this library */
#      define ITK_TEMPLATE_EXPORT __attribute__((visibility("default")))
#    else
        /* We are using this library */
#      define ITK_TEMPLATE_EXPORT __attribute__((visibility("default")))
#    endif
#  endif

#  ifndef ITK_TEMPLATE_NO_EXPORT
#    define ITK_TEMPLATE_NO_EXPORT __attribute__((visibility("hidden")))
#  endif
#endif

#ifndef ITK_TEMPLATE_DEPRECATED
#  define ITK_TEMPLATE_DEPRECATED __attribute__ ((__deprecated__))
#endif

#ifndef ITK_TEMPLATE_DEPRECATED_EXPORT
#  define ITK_TEMPLATE_DEPRECATED_EXPORT ITK_TEMPLATE_EXPORT ITK_TEMPLATE_DEPRECATED
#endif

#ifndef ITK_TEMPLATE_DEPRECATED_NO_EXPORT
#  define ITK_TEMPLATE_DEPRECATED_NO_EXPORT ITK_TEMPLATE_NO_EXPORT ITK_TEMPLATE_DEPRECATED
#endif

#if 0 /* DEFINE_NO_DEPRECATED */
#  ifndef ITK_TEMPLATE_NO_DEPRECATED
#    define ITK_TEMPLATE_NO_DEPRECATED
#  endif
#endif

/* ------- Items below are customization not included with generate_export_header ------ */
// Setup symbol exports
#ifdef ITK_TEMPLATE_VISIBILITY_DEFAULT
  #define ITK_FORCE_EXPORT_MACRO(moduleName) __attribute__ ((visibility ("default")))
#else
  #define ITK_FORCE_EXPORT_MACRO(moduleName) moduleName ## _EXPORT
#endif

#ifndef ITK_FORWARD_EXPORT
  // If build with shared libraries, on MacOS, if USE_COMPILER_HIDDEN_VISIBILITY is ON
  #if defined(__APPLE__)\
   && defined(ITK_TEMPLATE_VISIBILITY_DEFAULT)\
   && defined(ITK_BUILD_SHARED_LIBS)\
   && defined(USE_COMPILER_HIDDEN_VISIBILITY)
    #define ITK_FORWARD_EXPORT __attribute__ ((visibility ("default")))
  #else
    #define ITK_FORWARD_EXPORT
  #endif
#endif

#endif

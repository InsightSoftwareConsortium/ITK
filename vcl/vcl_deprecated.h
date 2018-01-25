#ifndef vcl_deprecated_h_
#define vcl_deprecated_h_

//:
// \file
// \brief  Defines macros used for marking deprecated functions
// \author Amitha Perera
// \date   27 May 2001
//
// To mark a function as deprecated, use the macro VXL_DEPRECATED_MACRO as
// the first line of that function:
// \code
//    void a::f() {
//       VXL_DEPRECATED_MACRO( "a::f()" );
//       ...
//    }
// \endcode
//
// If VXL_WARN_DEPRECATED was not defined at compile time, nothing
// happens. If it was defined, then executing the function will result
// in runtime warnings. The default behaviour is to warn every time
// the function is called. Additionally, if VXL_WARN_DEPRECATED_ONCE
// was defined, the warning will only be issued on the first call. If
// VXL_WARN_DEPRECATED_ABORT was defined, the function will abort()
// when called.
//
// Since the C++ language does not have support for deprecation, the
// surest way to find out _where_ the deprecated function is called is
// to define VXL_WARN_DEPRECATED_ABORT and then do a stack trace using
// a debugger!

#ifdef VXL_WARN_DEPRECATED
  #ifdef VXL_WARN_DEPRECATED_ABORT
    void vcl_deprecated_abort( const char* func_name );
    #define VXL_DEPRECATED_MACRO(f) vcl_deprecated_abort( f )
  #else
    void vcl_deprecated_warn( const char* func_name );
    #ifdef VXL_WARN_DEPRECATED_ONCE
      #define VXL_DEPRECATED_MACRO(f) \
        do { \
            static bool vcl_deprecated_flag = true; \
            if( vcl_deprecated_flag ) { \
                vcl_deprecated_warn( f ); \
                vcl_deprecated_flag=false; \
            } \
        } while (0)
    #else
      #define VXL_DEPRECATED_MACRO(f) vcl_deprecated_warn( f )
    #endif
  #endif
#else
  #define VXL_DEPRECATED_MACRO(f) /* suppress deprecation warning */
#endif

#endif

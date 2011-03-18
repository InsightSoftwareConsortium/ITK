// This is vcl/vcl_cctype.h
#ifndef vcl_cctype_h_
#define vcl_cctype_h_
//:
// \file
// \author fsm

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CCTYPE
# include <ctype.h>
# define vcl_generic_cctype_STD /* */
# include "generic/vcl_cctype.h"
#elif defined(VCL_VC)
  // VC doesn't put the cctype in STD
# define vcl_generic_cctype_STD /* */
#include <cctype>
# include "generic/vcl_cctype.h"
#else
# include "iso/vcl_cctype.h"
#endif

// Sometimes they are not functions, but macros (this is non-standard).
//  isspace
//  isprint
//  iscntrl
//  isupper
//  islower
//  isalpha
//  isdigit
//  ispunct
//  isxdigit
//  isalnum
//  isgraph
#ifdef isspace
# undef  vcl_isspace
# define vcl_isspace isspace
#endif

#ifdef isprint
# undef  vcl_isprint
# define vcl_isprint isprint
#endif

#ifdef iscntrl
# undef  vcl_iscntrl
# define vcl_iscntrl iscntrl
#endif

#ifdef isupper
# undef  vcl_isupper
# define vcl_isupper isupper
#endif

#ifdef toupper
# undef  vcl_toupper
# define vcl_toupper toupper
#endif

#ifdef islower
# undef  vcl_islower
# define vcl_islower islower
#endif

#ifdef tolower
# undef  vcl_tolower
# define vcl_tolower tolower
#endif

#ifdef isalpha
# undef  vcl_isalpha
# define vcl_isalpha isalpha
#endif

#ifdef isdigit
# undef  vcl_isdigit
# define vcl_isdigit isdigit
#endif

#ifdef ispunct
# undef  vcl_ispunct
# define vcl_ispunct ispunct
#endif

#ifdef isxdigit
# undef  vcl_isxdigit
# define vcl_isxdigit isxdigit
#endif

#ifdef isalnum
# undef  vcl_isalnum
# define vcl_isalnum isalnum
#endif

#ifdef isgraph
# undef  vcl_isgraph
# define vcl_isgraph isgraph
#endif

#endif // vcl_cctype_h_

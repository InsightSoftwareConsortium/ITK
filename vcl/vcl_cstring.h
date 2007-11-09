#ifndef vcl_cstring_h_
#define vcl_cstring_h_

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CSTRING
# include <string.h>
# define vcl_generic_cstring_STD /* */
# include "generic/vcl_cstring.h"
#elif defined(VCL_VC60)
# include <cstring>
# define vcl_generic_cstring_STD /* */
# include "generic/vcl_cstring.h"
#elif defined(VCL_METRO_WERKS)
# include <cstring>
# define vcl_generic_cstring_STD /* */
# include "generic/vcl_cstring.h"
#else
# include "iso/vcl_cstring.h"
#endif

// Sometimes they are not functions, but macros (this is non-standard).
//  memchr
//  memcmp
//  memcpy
//  memmove
//  memset
//  strcat
//  strchr
//  strcmp
//  strcoll
//  strcpy
//  strcspn
//  strerror
//  strlen
//  strncat
//  strncmp
//  strncpy
//  strpbrk
//  strrchr
//  strspn
//  strstr
//  strtok
//  strxfrm
#ifdef memchr
# undef vcl_memchr
# define vcl_memchr memchr
#endif
#ifdef memcmp
# undef vcl_memcmp
# define vcl_memcmp memcmp
#endif
#ifdef memcpy
# undef vcl_memcpy
# define vcl_memcpy memcpy
#endif
#ifdef memmove
# undef vcl_memmove
# define vcl_memmove memmove
#endif
#ifdef memset
# undef vcl_memset
# define vcl_memset memset
#endif
#ifdef strcat
# undef vcl_strcat
# define vcl_strcat strcat
#endif
#ifdef strchr
# undef vcl_strchr
# define vcl_strchr strchr
#endif
#ifdef strcmp
# undef vcl_strcmp
# define vcl_strcmp strcmp
#endif
#ifdef strcoll
# undef vcl_strcoll
# define vcl_strcoll strcoll
#endif
#ifdef strcpy
# undef vcl_strcpy
# define vcl_strcpy strcpy
#endif
#ifdef strcspn
# undef vcl_strcspn
# define vcl_strcspn strcspn
#endif
#ifdef strerror
# undef vcl_strerror
# define vcl_strerror strerror
#endif
#ifdef strlen
# undef vcl_strlen
# define vcl_strlen strlen
#endif
#ifdef strncat
# undef vcl_strncat
# define vcl_strncat strncat
#endif
#ifdef strncmp
# undef vcl_strncmp
# define vcl_strncmp strncmp
#endif
#ifdef strncpy
# undef vcl_strncpy
# define vcl_strncpy strncpy
#endif
#ifdef strpbrk
# undef vcl_strpbrk
# define vcl_strpbrk strpbrk
#endif
#ifdef strrchr
# undef vcl_strrchr
# define vcl_strrchr strrchr
#endif
#ifdef strspn
# undef vcl_strspn
# define vcl_strspn strspn
#endif
#ifdef strstr
# undef vcl_strstr
# define vcl_strstr strstr
#endif
#ifdef strtok
# undef vcl_strtok
# define vcl_strtok strtok
#endif
#ifdef strxfrm
# undef vcl_strxfrm
# define vcl_strxfrm strxfrm
#endif

#endif // vcl_cstring_h_

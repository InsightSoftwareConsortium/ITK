#ifndef CHARLS_CONFIG
#define CHARLS_CONFIG



#ifdef _DEBUG
#include <assert.h>
#define ASSERT(t) assert(t)
#else
#  ifndef ASSERT
#    define ASSERT(t) { }
#  endif
#endif

#if defined(_WIN32)
#ifndef CHARLS_IMEXPORT
#  define CHARLS_IMEXPORT __declspec(dllexport)
#endif

#ifdef _MSC_VER
#pragma warning (disable:4512)
#endif

#endif

// Typedef used by Charls for the default integral type.
// charls will work correct with 64 or 32 bit.
typedef long LONG;

enum constants
{
  LONG_BITCOUNT = sizeof(LONG)*8
};


typedef unsigned char BYTE;
typedef unsigned short USHORT;

#undef  NEAR

#ifndef inlinehint
#  ifdef _MSC_VER
#    ifdef _DEBUG
#      define inlinehint
#    else
#      define inlinehint __forceinline
#    endif
#  elif defined(__GNUC__) && (__GNUC__ > 3 || __GNUC__ == 3 && __GNUC_MINOR__ > 0)
#    define inlinehint inline
#  else
#    define inlinehint inline
#  endif
#endif
// must try __attribute__((always_inline)) for GCC!

#if defined(i386) || defined(__i386__) || defined(_M_IX86) || defined(__amd64__) || defined(_M_X64)
#define ARCH_HAS_UNALIGNED_MEM_ACCESS /* TODO define this symbol for more architectures */
#endif


#endif

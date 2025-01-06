
//-------------------------------------

#ifdef VXL_UNISTD_USLEEP_IS_VOID
// VXL_UNISTD_USLEEP_IS_VOID is set to 1 if this test fails
#include <unistd.h>

int main() { int x = usleep(0); return x*0; }
#endif // VXL_UNISTD_USLEEP_IS_VOID
//-------------------------------------
#ifdef VCL_NUMERIC_LIMITS_HAS_INFINITY
// Does vcl_numeric_limits<float>::has_infinity == 1?

// Several versions of gcc (3.0, 3.1, and 3.2) come with a
// numeric_limits that reports that they have no infinity.
#include <limits>
int main() {
  return std::numeric_limits<double>::has_infinity &&
         std::numeric_limits<float>::has_infinity ? 0 : 1;
}
#endif // VCL_NUMERIC_LIMITS_HAS_INFINITY

//-------------------------------------

#ifdef VXL_HAS_TYPE_OF_SIZE
// This is used to check if (1) a type exists, (2) is has the required
// size in bytes, and (3) it is functional. The last requirement is
// driven by MSCV 6 which has __int64, but it is not fully
// functional. (It can't be cast to a double, for example.)

// CHAR_BIT is the number of bits per char.
#include <climits>
#ifndef CHAR_BIT
# define CHAR_BIT 8
#endif

#include "config.h"

#if INTEGRAL_TYPE
double cast( THE_TYPE a, unsigned THE_TYPE b, signed THE_TYPE c )
{
  return double( a ) + double( b ) + double( c );
}
#else // INTEGRAL_TYPE
double cast( THE_TYPE a )
{
  return double( a );
}
#endif // INTEGRAL_TYPE

// These declarations conflict unless the sizes match.
extern int (*verify_size)[sizeof(THE_TYPE) * CHAR_BIT];
extern int (*verify_size)[THE_SIZE];

int main()
{
  return 0;
}

#endif // VXL_HAS_TYPE_OF_SIZE

//-------------------------------------
#ifdef VCL_HAS_LFS

// Return 1 if compiler has #define-switchable Large File Support
#define _LARGEFILE_SOURCE
#define _FILE_OFFSET_BITS 64

#include <fstream>
#include <iostream>

int main(int argc, char * argv[])
{
 if( sizeof(std::streamoff)==8 )
   return 0 ;
 else
   return 1;
}

#endif // VCL_HAS_LFS

//-------------------------------------

#ifdef VXL_HAS_DBGHELP_H

// This is a Windows header, and needs windows.h included first to make it compile properly.
#include <Windows.h>
#include <DbgHelp.h>

int main() { MINIDUMP_EXCEPTION_INFORMATION dummy; return 0; }
#endif // VXL_HAS_DBGHELP_H

//-------------------------------------

//-------------------------------------
#ifdef VXL_HAS_WIN_WCHAR_T

#ifdef _WCHAR_T_DEFINED
#include <cwchar>
int main()
{
  wchar_t buf [10];
  buf[0] = L'1';
  buf[1] = L'\0';
  return 0;
}
#else
  int main() { return 1; }
#endif

#endif

//-------------------------------------

#ifdef VXL_HAS_MM_MALLOC
#include <emmintrin.h>
int main()
{
  void* x = _mm_malloc(4*sizeof(float),16);
  _mm_free(x);
  return 0;
}
#endif

//-------------------------------------

#ifdef VXL_HAS_ALIGNED_MALLOC
#include <malloc.h>
int main()
{
  void* x = _aligned_malloc(4*sizeof(float),16);
  _aligned_free(x);
  return 0;
}
#endif

//-------------------------------------

#ifdef VXL_HAS_MINGW_ALIGNED_MALLOC
#include <malloc.h>
int main()
{
  void* x = __mingw_aligned_malloc(4*sizeof(float),16);
  __mingw_aligned_free(x);
  return 0;
}
#endif

//-------------------------------------

#ifdef VXL_HAS_POSIX_MEMALIGN
#include <cstdlib>
int main()
{
  void* x = memalign(16,4*sizeof(float));
  free(x);
  return 0;
}
#endif

//-------------------------------------

#if defined(VXL_HAS_SSE2_HARDWARE_SUPPORT) || defined(VXL_SSE2_HARDWARE_SUPPORT_POSSIBLE)
#include <emmintrin.h>
int main()
{
  //try to do some sse2 calculations
  double d_a[]  = { 6.75, 3.42 };
  double d_b[]  = { 2.3, 9.2 };
  double res[2] = {0.0};

  __m128d z;
  z = _mm_mul_pd(_mm_loadu_pd(d_a),_mm_loadu_pd(d_b));

  _mm_storeu_pd(res,z);

  return 0;
}
#endif

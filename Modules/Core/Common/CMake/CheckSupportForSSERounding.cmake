# check if the platform has sse functions
include(CheckCXXSourceCompiles)

include(CheckIncludeFiles)

check_include_files("emmintrin.h" HAVE_EMMINTRIN_H)

check_cxx_source_compiles("
#include <emmintrin.h> // sse 2 intrinsics
int main()
{
float x = 0.5;
_mm_cvtss_si64( _mm_set_ss(x) );
_mm_cvtsd_si64( _mm_set_sd(x) );
return 0;
}
"
ITK_COMPILER_SUPPORTS_SSE2_64
)

check_cxx_source_compiles("
#include <emmintrin.h> // sse 2 intrinsics
int main()
{
float x = 0.5;
_mm_cvtss_si32( _mm_set_ss(x) );
_mm_cvtsd_si32( _mm_set_sd(x) );
return 0;
}
"
ITK_COMPILER_SUPPORTS_SSE2_32
)

check_cxx_source_compiles("
#include <emmintrin.h> // sse 2 intrinsics
int main()
{
#if !defined( __SSE__ )
#  error
#endif

#if !defined( __SSE2__ )
#  error
#endif

#if !defined( __SSE_MATH__ )
#  error
#endif

#if !defined( __SSE2_MATH__ )
#  error
#endif

  return 0;
}
"
ITK_COMPILER_DOES_NOT_NEED_MSSE2_FLAG
)

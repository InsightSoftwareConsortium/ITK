#ifndef vnl_sse_h_
#define vnl_sse_h_
//:
// \file
// \author Kieran O'Mahony
// \date Sep 2007
// \brief Support for Streaming SIMD Extensions to speed up vector arithmetic
// \verbatim
//  Modifications
//   2009-03-30 Peter Vanroose - Added arg_min() & arg_max() and reimplemented min() & max()
// \endverbatim

#include <vcl_compiler.h> // for macro decisions based on compiler type
#include <vxl_config.h>   // for checking supported integer data types
#include <vcl_cfloat.h>   // for DBL_MAX and FLT_MAX

#include <vnl/vnl_config.h> // is SSE enabled
#include <vnl/vnl_alloc.h>  // is SSE enabled
#include "vnl/vnl_export.h"

// some caveats...
// - Due to the way vnl_matrix is represented in memory cannot guarantee 16-byte alignment,
//   therefore have to use slower unaligned loading intrinsics for matrices.
// - The GCC 3.4 intrinsics seem to be horrendously slow...

// - On Mac OS X, in order to support Universal Binaries, we do not consider it a hard
//   error if VNL_CONFIG_ENABLE_SSE2 is true for PowerPC builds. PowerPC obviously does
//   not support SSE2, so we simply redefine it to false.
#if VNL_CONFIG_ENABLE_SSE2
# if defined(__APPLE__) && (defined(__ppc__) || defined(__ppc64__))
#   undef VNL_CONFIG_ENABLE_SSE2
#   define VNL_CONFIG_ENABLE_SSE2 0
# elif !VXL_HAS_EMMINTRIN_H
#   error "Required file emmintrin.h for SSE2 not found"
# else
#   include <emmintrin.h> // sse 2 intrinsics
# endif
#endif


// Try and use compiler instructions for forcing inlining if possible
// Also instruction for aligning stack memory is compiler dependent
#if defined(VCL_GCC)
// With attribute always_inline, gcc can give an error if a function
// cannot be inlined, so it is disabled.  Problem seen on 64 bit
// platforms with std::vector<vnl_rational>.
# define VNL_SSE_FORCE_INLINE /* __attribute__((always_inline)) */ inline
# define VNL_SSE_STACK_ALIGNED(x)  __attribute__((aligned(x))) VNL_EXPORT
#elif defined VCL_VC
# define VNL_SSE_FORCE_INLINE __forceinline
# define VNL_SSE_STACK_ALIGNED(x)  __declspec(align(x)) VNL_EXPORT
#else
# define VNL_SSE_FORCE_INLINE inline
# define VNL_SSE_STACK_ALIGNED(x)
# define VNL_SSE_STACK_STORE(pf) _mm_storeu_##pf // no stack alignment so use unaligned store (slower!)
#endif


// SSE operates faster with 16 byte aligned memory addresses.
// Check what memory alignment function is supported
#if VNL_CONFIG_ENABLE_SSE2 && VXL_HAS_MM_MALLOC
# define VNL_SSE_ALLOC(n,s,a) _mm_malloc(n*s,a)
# define VNL_SSE_FREE(v,n,s) _mm_free(v)
#elif VNL_CONFIG_ENABLE_SSE2 && VXL_HAS_ALIGNED_MALLOC
# include <malloc.h>
# define VNL_SSE_ALLOC(n,s,a) _aligned_malloc(n*s,a)
# define VNL_SSE_FREE(v,n,s) _aligned_free(v)
#elif VNL_CONFIG_ENABLE_SSE2 && VXL_HAS_MINGW_ALIGNED_MALLOC
# include <malloc.h>
# define VNL_SSE_ALLOC(n,s,a) __mingw_aligned_malloc(n*s,a)
# define VNL_SSE_FREE(v,n,s) __mingw_aligned_free(v)
#elif VNL_CONFIG_ENABLE_SSE2 && VXL_HAS_POSIX_MEMALIGN
# include <vcl_cstdlib.h>
# define VNL_SSE_ALLOC(n,s,a) memalign(a,n*s)
# define VNL_SSE_FREE(v,n,s) std::free(v)
#else // sse2 disabled or could not get memory alignment support, use slower unaligned based intrinsics
# define VNL_SSE_HEAP_STORE(pf) _mm_storeu_##pf
# define VNL_SSE_HEAP_LOAD(pf) _mm_loadu_##pf
# if VNL_CONFIG_THREAD_SAFE
#   define VNL_SSE_ALLOC(n,s,a) new char[n*s]
#   define VNL_SSE_FREE(v,n,s) delete [] static_cast<char*>(v)
# else
#   define VNL_SSE_ALLOC(n,s,a) vnl_alloc::allocate((n == 0) ? 8 : (n * s));
#   define VNL_SSE_FREE(v,n,s) if (v) vnl_alloc::deallocate(v, (n == 0) ? 8 : (n * s));
# endif
#endif


// Stack memory can be aligned -> use SSE aligned store
#ifndef VNL_SSE_STACK_STORE
# define VNL_SSE_STACK_STORE(pf) _mm_store_##pf
#endif

// Heap memory can be aligned -> use SSE aligned load & store
#ifndef VNL_SSE_HEAP_STORE
# define VNL_SSE_HEAP_STORE(pf) _mm_store_##pf
# define VNL_SSE_HEAP_LOAD(pf) _mm_load_##pf
#endif

//: Custom memory allocation function to force 16 byte alignment of data
VNL_SSE_FORCE_INLINE void* vnl_sse_alloc(std::size_t n, unsigned size)
{
  return VNL_SSE_ALLOC(n,size,16);
}

//: Custom memory deallocation function to free 16 byte aligned of data
VNL_SSE_FORCE_INLINE void vnl_sse_dealloc(void* mem, std::size_t n, unsigned size)
{
  // Variables n and size are not used in all versions of the VNL_SSE_FREE macro.
  // Cast to void here to avoid unused variable warnings.
  (void)n,(void)size;
  VNL_SSE_FREE(mem,n,size);
}

// avoid inlining when debugging
#ifndef NDEBUG
#undef VNL_SSE_FORCE_INLINE
#define VNL_SSE_FORCE_INLINE
#endif

#if VNL_CONFIG_ENABLE_SSE2
class VNL_EXPORT vnl_sse_supplement
{
public:
  // SSE2 does not have a native _mm_min_epi32 or _mm_max_epi32 (le sigh-- SSE4.1
  // provides these).  So, these are substitutes written in SSE2 based off the
  // SSEPlus library.
  static VNL_SSE_FORCE_INLINE __m128i vnl_mm_min_epi32(__m128i a, __m128i b)
  {
    __m128i mask  = _mm_cmplt_epi32(a, b);
    a = _mm_and_si128(a, mask);
    b = _mm_andnot_si128(mask, b);
    a = _mm_or_si128(a, b);
    return a;
  }

  static VNL_SSE_FORCE_INLINE __m128i vnl_mm_max_epi32(__m128i a, __m128i b)
  {
    __m128i mask  = _mm_cmpgt_epi32(a, b);
    a = _mm_and_si128(a, mask);
    b = _mm_andnot_si128(mask, b);
    a = _mm_or_si128(a, b);
    return a;
  }
};
// If SSE4.1 is available, these can be replaced by their native
// implementations.
#define VNL_MM_MIN_EPI32 vnl_sse_supplement::vnl_mm_min_epi32
#define VNL_MM_MAX_EPI32 vnl_sse_supplement::vnl_mm_max_epi32
#endif // VNL_CONFIG_ENABLE_SSE2

//: Bog standard (no sse) implementation for non sse enabled hardware and any type which doesn't have a template specialisation.
template <class T>
class VNL_EXPORT vnl_sse
{
 public:
  static VNL_SSE_FORCE_INLINE void element_product(const T* x, const T* y, T* r, unsigned n)
  {
    for (unsigned i = 0; i < n; ++i)
      r[i] = x[i] * y[i];
  }

  static VNL_SSE_FORCE_INLINE T dot_product(const T* x, const T* y, unsigned n)
  {
    T sum(0);
    for (unsigned i = 0; i < n; ++i)
      sum += x[i] * y[i];
    return sum;
  }

  static VNL_SSE_FORCE_INLINE T euclid_dist_sq(const T* x, const T* y, unsigned n)
  {
    // IMS: Unable to optimise this any further for MSVC compiler
    T sum(0);
    --x;
    --y;
    while (n!=0)
    {
      const T diff = x[n] - y[n];
      sum += diff*diff;
      --n;
    }
    return sum;
  }

  static VNL_SSE_FORCE_INLINE void vector_x_matrix(const T* v, const T* m, T* r, unsigned rows, unsigned cols)
  {
    for (unsigned int j=0; j<cols; ++j) {
      T som(0);
      for (unsigned int i=0; i<rows; ++i)
        som += (m+i*cols)[j] * v[i];
      r[j] = som;
    }
  }

  static VNL_SSE_FORCE_INLINE void matrix_x_vector(const T* m, const T* v, T* r, unsigned rows, unsigned cols)
  {
    for (unsigned int i=0; i<rows; ++i) {
      T som(0);
      for (unsigned int j=0; j<cols; ++j)
        som += (m+i*cols)[j] * v[j];
      r[i] = som;
    }
  }

  static VNL_SSE_FORCE_INLINE T sum(const T* v, unsigned n)
  {
    T tot(0);
    for (unsigned i = 0; i < n; ++i)
      tot += *v++;
    return tot;
  }

  static VNL_SSE_FORCE_INLINE T max(const T* v, unsigned n)
  {
    if (n==0) return T(0); // the maximum of an empty set is undefined
    T tmp = *v;
    while (--n > 0)
      if (*++v > tmp)
        tmp = *v;
    return tmp;
  }

  static VNL_SSE_FORCE_INLINE T min(const T* v, unsigned n)
  {
    if (n==0) return T(0); // the minimum of an empty set is undefined
    T tmp = *v;
    while (--n > 0)
      if (*++v < tmp)
        tmp = *v;
    return tmp;
  }

  static VNL_SSE_FORCE_INLINE unsigned arg_max(const T* v, unsigned n)
  {
    if (n==0) return unsigned(-1); // the maximum of an empty set is undefined
    T tmp = *v;
    unsigned idx = 0;
    for (unsigned i=1; i<n; ++i)
      if (*++v > tmp)
        tmp = *v, idx = i;
    return idx;
  }

  static VNL_SSE_FORCE_INLINE unsigned arg_min(const T* v, unsigned n)
  {
    if (n==0) return unsigned(-1); // the minimum of an empty set is undefined
    T tmp = *v;
    unsigned idx = 0;
    for (unsigned i=1; i<n; ++i)
      if (*++v < tmp)
        tmp = *v, idx = i;
    return idx;
  }
};

#if VNL_CONFIG_ENABLE_SSE2

//: SSE2 implementation for double precision floating point (64 bit)
template <>
class VNL_EXPORT vnl_sse<double>
{
 public:
  static VNL_SSE_FORCE_INLINE void element_product(const double* x, const double* y, double* r, unsigned n)
  {
    switch (n%4)
    {
      // do scalar (single value) load, multiply and store for end elements
      case 3: --n; _mm_store_sd(r+n,_mm_mul_sd(_mm_load_sd(x+n),_mm_load_sd(y+n)));
      case 2: --n; _mm_store_sd(r+n,_mm_mul_sd(_mm_load_sd(x+n),_mm_load_sd(y+n)));
      case 1: --n; _mm_store_sd(r+n,_mm_mul_sd(_mm_load_sd(x+n),_mm_load_sd(y+n)));
      case 0: ;
    }

    // load, multiply and store two doubles at a time
    // loop unroll to handle 4
    if (std::ptrdiff_t(x)%16 || std::ptrdiff_t(y)%16  || std::ptrdiff_t(r)%16)
          // unaligned case
      for (int i = n-4; i >= 0; i-=4)
      {
        _mm_storeu_pd(r+i,_mm_mul_pd(_mm_loadu_pd(x+i),_mm_loadu_pd(y+i)));
        _mm_storeu_pd(r+i+2,_mm_mul_pd(_mm_loadu_pd(x+i+2),_mm_loadu_pd(y+i+2)));
      }
    else  // aligned case
      for (int i = n-4; i >= 0; i-=4)
      {
        VNL_SSE_HEAP_STORE(pd)(r+i,_mm_mul_pd(VNL_SSE_HEAP_LOAD(pd)(x+i),VNL_SSE_HEAP_LOAD(pd)(y+i)));
        VNL_SSE_HEAP_STORE(pd)(r+i+2,_mm_mul_pd(VNL_SSE_HEAP_LOAD(pd)(x+i+2),VNL_SSE_HEAP_LOAD(pd)(y+i+2)));
      }
  }

  static VNL_SSE_FORCE_INLINE double dot_product(const double* x, const double* y, unsigned n)
  {
    double ret;
    __m128d sum;
    if (n%2)
    {
      // handle single element at end of odd sized vectors
      n--; sum = _mm_mul_sd(_mm_load_sd(x+n),_mm_load_sd(y+n));
    }
    else
      sum = _mm_setzero_pd();

    if (std::ptrdiff_t(x)%16 || std::ptrdiff_t(y)%16)
         // unaligned case
      for (int i = n-2; i >= 0; i-=2)
        sum = _mm_add_pd(_mm_mul_pd(_mm_loadu_pd(x+i), _mm_loadu_pd(y+i)),sum);
    else // aligned case
      for (int i = n-2; i >= 0; i-=2)
        sum = _mm_add_pd(_mm_mul_pd(VNL_SSE_HEAP_LOAD(pd)(x+i), VNL_SSE_HEAP_LOAD(pd)(y+i)),sum);

    // sum will contain 2 accumulated values, need to add them together
    sum = _mm_add_sd(sum,_mm_unpackhi_pd(sum,_mm_setzero_pd()));
    _mm_store_sd(&ret,sum);
    return ret;
  }

  static VNL_SSE_FORCE_INLINE double euclid_dist_sq(const double* x, const double* y, unsigned n)
  {
    double ret;
    __m128d sum,a;

    if (n%2)
    {
      // handle single element at end of odd sized vectors
        n--; a = _mm_sub_sd(_mm_load_sd(x+n),_mm_load_sd(y+n));
        sum = _mm_mul_sd(a,a);
    }
    else
      sum = _mm_setzero_pd();

    if (std::ptrdiff_t(x)%16 || std::ptrdiff_t(y)%16)
         // unaligned case
      for ( int i = n-2; i >= 0; i-=2 )
      {
        a = _mm_sub_pd(_mm_loadu_pd(x+i),_mm_loadu_pd(y+i));
        sum = _mm_add_pd(_mm_mul_pd(a,a),sum);
      }
    else // aligned case
      for ( int i = n-2; i >= 0; i-=2 )
      {
        a = _mm_sub_pd(VNL_SSE_HEAP_LOAD(pd)(x+i),VNL_SSE_HEAP_LOAD(pd)(y+i));
        sum = _mm_add_pd(_mm_mul_pd(a,a),sum);
      }

    // sum will contain 2 accumulated values, need to add them together
    sum = _mm_add_sd(sum,_mm_unpackhi_pd(sum,_mm_setzero_pd()));
    _mm_store_sd(&ret,sum);
    return ret;
  }

  static VNL_SSE_FORCE_INLINE void vector_x_matrix(const double* v, const double* m, double* r, unsigned rows, unsigned cols)
  {
    __m128d accum, x,y,z,w;

    // calculate if there are any left-over rows/columns
    unsigned r_left = rows%4;
    unsigned r_nice = rows - r_left;
    unsigned c_left = cols%2;
    unsigned c_nice = cols - c_left;

    // handle 2 matrix columns at a time
    for (unsigned j = 0; j < c_nice; j+=2)
    {
      // handle 4 matrix rows at a time
      accum = _mm_setzero_pd();
      unsigned i = 0;
      while ( i < r_nice )
      {
        // load vector data so that
        // y = (v0,v1) , w = (v2,v3)
        y = VNL_SSE_HEAP_LOAD(pd)(v+i);
        w = VNL_SSE_HEAP_LOAD(pd)(v+i+2);

        _mm_prefetch((const char*)(v+i+4), _MM_HINT_NTA);

        // after shuffling
        // x = (v0, v0)
        // y = (v1, v1)
        // z = (v2, v2)
        // w = (v3, v3)
        x = _mm_shuffle_pd(y,y,_MM_SHUFFLE2(0,0));
        y = _mm_shuffle_pd(y,y,_MM_SHUFFLE2(1,1));
        z = _mm_shuffle_pd(w,w,_MM_SHUFFLE2(0,0));
        w = _mm_shuffle_pd(w,w,_MM_SHUFFLE2(1,1));

        // multiply the two matrix columns
        // i.e.  x = ( v0 * m00, v0 * m01)
        //       y = ( v1 * m10, v1 * m11)
        //       z = ( v2 * m20, v2 * m21)
        //       w = ( v3 * m30, v3 * m31)
        x = _mm_mul_pd(x,_mm_loadu_pd(i++*cols+m+j));
        y = _mm_mul_pd(y,_mm_loadu_pd(i++*cols+m+j));
        z = _mm_mul_pd(z,_mm_loadu_pd(i++*cols+m+j));
        w = _mm_mul_pd(w,_mm_loadu_pd(i++*cols+m+j));

        // now sum both columns
        accum = _mm_add_pd(x,accum);
        accum = _mm_add_pd(y,accum);
        accum = _mm_add_pd(z,accum);
        accum = _mm_add_pd(w,accum);

        // accum is now ( v0 * m00 + v1 * m10 + v2 * m20 + v3 * m30,
        //                v0 * m01 + v1 * m11 + v2 * m21 + v3 * m31 )
      }

      // handle left-over rows
      switch (r_left)
      {
        case 3: accum = _mm_add_pd(_mm_mul_pd(_mm_load1_pd(v+i),_mm_loadu_pd(m+i*cols+j)), accum); i++;
        case 2: accum = _mm_add_pd(_mm_mul_pd(_mm_load1_pd(v+i),_mm_loadu_pd(m+i*cols+j)), accum); i++;
        case 1: accum = _mm_add_pd(_mm_mul_pd(_mm_load1_pd(v+i),_mm_loadu_pd(m+i*cols+j)), accum);
        case 0: ;
      }

      // store the 2 values of the result vector
      // use stream to avoid polluting the cache
      _mm_stream_pd(r+j,accum);
    }

    // handle the left over columns
    if ( c_left )
    {
      accum = _mm_setzero_pd();
      for (unsigned int i=0; i<rows; ++i)
        accum = _mm_add_sd(_mm_mul_sd(_mm_load_sd(m+i*cols+cols-1),_mm_load_sd(v+i)),accum);
      _mm_store_sd(r+cols-1, accum);
    }
  }

  static VNL_SSE_FORCE_INLINE void matrix_x_vector(const double* m, const double* v, double* r, unsigned rows, unsigned cols)
  {
    __m128d accum, x,y,mxy1,mxy2;

    // calculate if there are any left-over rows/columns
    unsigned r_left = rows%2;
    unsigned r_nice = rows - r_left;
    unsigned c_left = cols%2;
    unsigned c_nice = cols - c_left;

    // handle 2 matrix rows at a time
    for (unsigned i = 0; i < r_nice; i+=2)
    {
      // handle 2 matrix columns at a time
      accum = _mm_setzero_pd();
      const double *r1 = m+i*cols, *r2 = m+(i+1)*cols;
      unsigned j = 0;
      for (; j < c_nice; j+=2)
      {
        // load  vector data so that
        //  y = (v0, v1)
        y = VNL_SSE_HEAP_LOAD(pd)(v+j);

        // shuffle so that
        //  x = (v0,v0)   y = (v1,v1)
        x = _mm_shuffle_pd(y,y,_MM_SHUFFLE2(0,0));
        y = _mm_shuffle_pd(y,y,_MM_SHUFFLE2(1,1));

        // load the matrix data so that
        // mxy1 = (m00,m01),  mxy2 = (m10,m11)
        mxy1 = _mm_loadu_pd(r1+j);
        mxy2 = _mm_loadu_pd(r2+j);

        // unpack matrix data to achieve
        //  (v0,v0) * (m00,m10)
        //  (v1,v1) * (m01,m11)
        x = _mm_mul_pd(x,_mm_unpacklo_pd(mxy1,mxy2));
        y = _mm_mul_pd(y,_mm_unpackhi_pd(mxy1,mxy2));

        // now sum the results
        accum = _mm_add_pd(x,accum);
        accum = _mm_add_pd(y,accum);

        // accum is now ( v0 * m00 + v1 * m01,
        //                v0 * m11 + v1 * m11 )
      }
      // handle the left over columns
      if (c_left)
        accum = _mm_add_pd(_mm_mul_pd(_mm_load1_pd(v+j),_mm_set_pd(*(r2+j),*(r1+j))), accum);

      // store the 2 values of the result vector
      // use stream to avoid polluting the cache
      _mm_stream_pd(r+i,accum);
    }

    // handle the left over rows
    if ( r_left )
    {
      accum = _mm_setzero_pd();
      const double* p = m+(rows-1)*cols;
      for (unsigned int j=0; j<cols; ++j)
        accum = _mm_add_sd(_mm_mul_sd(_mm_load_sd(p+j),_mm_load_sd(v+j)),accum);
      _mm_store_sd(r+rows-1, accum);
    }
  }

  static VNL_SSE_FORCE_INLINE double sum(const double* x, unsigned n)
  {
    double ret;
    // decision logic for odd sized vectors
    __m128d sum = n%2 ? _mm_load_sd(x+--n) : _mm_setzero_pd();

    // sum two elements at a time, sum will contain two running totals
    for (int i = n-2; i >= 0; i-=2)
      sum = _mm_add_pd(VNL_SSE_HEAP_LOAD(pd)(x+i),sum);

    // sum will contain 2 accumulated values, need to add them together
    sum = _mm_add_sd(sum,_mm_unpackhi_pd(sum,_mm_setzero_pd()));
    _mm_store_sd(&ret,sum);
    return ret;
  }

  static VNL_SSE_FORCE_INLINE unsigned arg_max(const double* x, unsigned n)
  {
    if (n == 1)
      return 0;

    __m128d min_double = _mm_set1_pd(- DBL_MAX);
    __m128d max = min_double;
    __m128d input;
    __m128i input_idx = _mm_set_epi32(1, 1, 0, 0);
    __m128i input_idx_increment = _mm_set1_epi32(2);
    __m128i arg_max = _mm_set1_epi32(0);
    union IsMaxMask
      {
      __m128d m128d;
      __m128i m128i;
      };
    IsMaxMask is_max;

    const int n16 = (n/2) * 2;
    // handle two elements at a time, max will contain two max values
    for (int i=0; i<n16; i+=2)
      {
      input = VNL_SSE_HEAP_LOAD(pd)(x+i);
      max = _mm_max_pd(input, max);
      is_max.m128d = _mm_cmpeq_pd(max, input);
      arg_max = VNL_MM_MAX_EPI32(arg_max, _mm_and_si128(is_max.m128i, input_idx));
      input_idx = _mm_add_epi32(input_idx, input_idx_increment);
      }

    // decision logic for odd sized vectors
    if (n%2)
      {
      input_idx = _mm_set1_epi32(--n);
      input = _mm_load1_pd(x+n);
      max = _mm_max_sd(max, input);
      is_max.m128d = _mm_cmpeq_pd(max, input);
      arg_max = VNL_MM_MAX_EPI32(arg_max, _mm_and_si128(is_max.m128i, input_idx));
      }

    // need to store the index of the max value
    is_max.m128d = max;
    max = _mm_max_sd(_mm_unpackhi_pd(max, min_double), max);
    max = _mm_unpacklo_pd(max, max);
    is_max.m128d = _mm_cmpeq_pd(is_max.m128d, max);
    arg_max = _mm_and_si128(is_max.m128i, arg_max);
    arg_max = VNL_MM_MAX_EPI32(arg_max, _mm_unpackhi_epi32(arg_max, _mm_set1_epi32(0)));
    unsigned ret = _mm_cvtsi128_si32(arg_max);
    return ret;
  }

  static VNL_SSE_FORCE_INLINE unsigned arg_min(const double* x, unsigned n)
  {
    if (n == 1)
      return 0;

    __m128d max_double = _mm_set1_pd(DBL_MAX);
    __m128d min = max_double;
    __m128d input;
    __m128i input_idx = _mm_set_epi32(1, 1, 0, 0);
    __m128i input_idx_increment = _mm_set1_epi32(2);
    __m128i arg_min = _mm_set1_epi32(0);
    union IsMinMask
      {
      __m128d m128d;
      __m128i m128i;
      };
    IsMinMask is_min;

    const int n16 = (n/2) * 2;
    // handle two elements at a time, max will contain two max values
    for (int i=0; i<n16; i+=2)
      {
      input = VNL_SSE_HEAP_LOAD(pd)(x+i);
      min = _mm_min_pd(input, min);
      is_min.m128d = _mm_cmpeq_pd(min, input);
      arg_min = VNL_MM_MAX_EPI32(arg_min, _mm_and_si128(is_min.m128i, input_idx));
      input_idx = _mm_add_epi32(input_idx, input_idx_increment);
      }

    // decision logic for odd sized vectors
    if (n%2)
      {
      input_idx = _mm_set1_epi32(--n);
      input = _mm_load1_pd(x+n);
      min = _mm_min_sd(min, input);
      is_min.m128d = _mm_cmpeq_pd(min, input);
      arg_min = VNL_MM_MAX_EPI32(arg_min, _mm_and_si128(is_min.m128i, input_idx));
      }

    // need to store the index of the min value
    is_min.m128d = min;
    min = _mm_min_sd(_mm_unpackhi_pd(min, max_double), min);
    min = _mm_unpacklo_pd(min, min);
    is_min.m128d = _mm_cmpeq_pd(is_min.m128d, min);
    arg_min = _mm_and_si128(is_min.m128i, arg_min);
    arg_min = VNL_MM_MAX_EPI32(arg_min, _mm_unpackhi_epi32(arg_min, _mm_set1_epi32(0)));
    unsigned ret = _mm_cvtsi128_si32(arg_min);
    return ret;
  }

  static VNL_SSE_FORCE_INLINE double max(const double* x, unsigned n)
  {
    double ret;
    __m128d min_double = _mm_set1_pd(- DBL_MAX);
    __m128d max = min_double;

    // decision logic for odd sized vectors
    if (n%2)
      max = _mm_max_sd(max,_mm_load_sd(x+--n));

    // handle two elements at a time, max will contain two max values
    for (int i=n-2; i>=0; i-=2)
      max = _mm_max_pd(VNL_SSE_HEAP_LOAD(pd)(x+i), max);

    // need to store max's two values
    max = _mm_max_sd(max,_mm_unpackhi_pd(max,min_double));
    _mm_store_sd(&ret,max);
    return ret;
  }

  static VNL_SSE_FORCE_INLINE double min(const double* x, unsigned n)
  {
    double ret;
    __m128d max_double = _mm_set1_pd(DBL_MAX);
    __m128d min = max_double;

    // hand last element if odd sized vector
    if (n%2)
      min = _mm_min_sd(min,_mm_load_sd(x+--n));

    // handle two elements at a time, min will contain two min values
    for (int i=n-2; i>=0; i-=2)
      min = _mm_min_pd(VNL_SSE_HEAP_LOAD(pd)(x+i), min);

    // need to store min's two values
    min = _mm_min_sd(min,_mm_unpackhi_pd(min,max_double));
    _mm_store_sd(&ret,min);
    return ret;
  }
};

//: SSE2 implementation for single precision floating point (32 bit)
template <>
class VNL_EXPORT vnl_sse<float>
{
 public:
  static VNL_SSE_FORCE_INLINE void element_product(const float* x, const float* y, float* r, unsigned n)
  {
    switch (n%4)
    {
      // do scalar (single value) load, multiply and store for end elements
      case 3: --n; _mm_store_ss(r+n,_mm_mul_ss(_mm_load_ss(x+n),_mm_load_ss(y+n)));
      case 2: --n; _mm_store_ss(r+n,_mm_mul_ss(_mm_load_ss(x+n),_mm_load_ss(y+n)));
      case 1: --n; _mm_store_ss(r+n,_mm_mul_ss(_mm_load_ss(x+n),_mm_load_ss(y+n)));
      case 0: ;
    }

    // load, multiply and store four floats at a time
    for (int i = n-4; i >= 0; i-=4)
      VNL_SSE_HEAP_STORE(ps)(r+i,_mm_mul_ps(VNL_SSE_HEAP_LOAD(ps)(x+i),VNL_SSE_HEAP_LOAD(ps)(y+i)));
  }

  static VNL_SSE_FORCE_INLINE float dot_product(const float* x, const float* y, unsigned n)
  {
    float ret;
    __m128 sum = _mm_setzero_ps();
    switch (n%4)
    {
      // handle elements at end of vectors with sizes not divisable by 4
      case 3: n--; sum = _mm_mul_ss(_mm_load_ss(x+n), _mm_load_ss(y+n));
      case 2: n--; sum = _mm_add_ss(_mm_mul_ss(_mm_load_ss(x+n), _mm_load_ss(y+n)),sum);
      case 1: n--; sum = _mm_add_ss(_mm_mul_ss(_mm_load_ss(x+n), _mm_load_ss(y+n)),sum);
      case 0: ;
    }

    for (int i = n-4; i >= 0; i-=4)
      sum = _mm_add_ps(_mm_mul_ps(VNL_SSE_HEAP_LOAD(ps)(x+i), VNL_SSE_HEAP_LOAD(ps)(y+i)),sum);

    // sum will contain 4 accumulated values, need to add them together
    sum = _mm_add_ps(sum,_mm_movehl_ps(_mm_setzero_ps(),sum));
    sum = _mm_add_ss(sum,_mm_shuffle_ps(sum,sum,_MM_SHUFFLE(3,2,1,1)));

    _mm_store_ss(&ret,sum);
    return ret;
  }

  static VNL_SSE_FORCE_INLINE float euclid_dist_sq(const float* x, const float* y, unsigned n)
  {
    float ret;
    __m128 sum,a;
    sum = a = _mm_setzero_ps();
    switch (n%4)
    {
      // handle elements at end of vectors with sizes not divisable by 4
      case 3: --n; a = _mm_sub_ss(_mm_load_ss(x+n),_mm_load_ss(y+n));
      case 2: --n; a = _mm_shuffle_ps(_mm_sub_ss(_mm_load_ss(x+n),_mm_load_ss(y+n)), a ,_MM_SHUFFLE(1,0,0,1));
      case 1: --n; a = _mm_move_ss(a,_mm_sub_ss(_mm_load_ss(x+n),_mm_load_ss(y+n)));
              sum = _mm_mul_ps(a,a);
      case 0: ;
    }

    for ( int i = n-4; i >= 0; i-=4 )
    {
      a = _mm_sub_ps(VNL_SSE_HEAP_LOAD(ps)(x+i),VNL_SSE_HEAP_LOAD(ps)(y+i));
      sum = _mm_add_ps(_mm_mul_ps(a,a),sum);
    }

    // sum will contain 4 accumulated values, need to add them together
    sum = _mm_add_ps(sum,_mm_movehl_ps(_mm_setzero_ps(),sum));
    sum = _mm_add_ss(sum,_mm_shuffle_ps(sum,sum,_MM_SHUFFLE(3,2,1,1)));

    _mm_store_ss(&ret,sum);
    return ret;
  }

  static VNL_SSE_FORCE_INLINE void vector_x_matrix(const float* v, const float* m, float* r, unsigned rows, unsigned cols)
  {
    __m128 accum, x,y,z,w;

    // calculate if there are any left-over rows/columns
    unsigned r_left = rows%4;
    unsigned r_nice = rows - r_left;
    unsigned c_left = cols%4;
    unsigned c_nice = cols - c_left;

    // handle 4 matrix columns at a time
    for (unsigned j = 0; j < c_nice; j+=4)
    {
      // handle 4 matrix rows at a time
      accum = _mm_setzero_ps();
      unsigned i = 0;
      while ( i < r_nice )
      {
        // load vector data so that
        // w = (v0,v1,v2,v3)
        w = VNL_SSE_HEAP_LOAD(ps)(v+i);

        // after shuffling
        // x = (v0, v0, v0, v0)
        // y = (v1, v1, v1, v1)
        // z = (v2, v2, v2, v2)
        // w = (v3, v3, v3, v3)
        x = _mm_shuffle_ps(w,w,_MM_SHUFFLE(0,0,0,0));
        y = _mm_shuffle_ps(w,w,_MM_SHUFFLE(1,1,1,1));
        z = _mm_shuffle_ps(w,w,_MM_SHUFFLE(2,2,2,2));
        w = _mm_shuffle_ps(w,w,_MM_SHUFFLE(3,3,3,3));

        // multiply the four matrix columns
        // i.e.  x = ( v0 * m00, v0 * m01, v0 * m02, v0 * m03)
        //       y = ( v1 * m10, v1 * m11, v1 * m12, v1 * m13)
        //       z = ( v2 * m20, v2 * m21, v2 * m22, v2 * m23)
        //       w = ( v3 * m30, v3 * m31, v3 * m32, v3 * m33)
        x = _mm_mul_ps(x,_mm_loadu_ps(m+i++*cols+j));
        y = _mm_mul_ps(y,_mm_loadu_ps(m+i++*cols+j));
        z = _mm_mul_ps(z,_mm_loadu_ps(m+i++*cols+j));
        w = _mm_mul_ps(w,_mm_loadu_ps(m+i++*cols+j));

        // now sum the four columns
        accum = _mm_add_ps(x,accum);
        accum = _mm_add_ps(y,accum);
        accum = _mm_add_ps(z,accum);
        accum = _mm_add_ps(w,accum);

        // accum is now ( v0 * m00 + v1 * m10 + v2 * m20 + v3 * m30,
        //                v0 * m01 + v1 * m11 + v2 * m21 + v3 * m31,
        //                v0 * m02 + v1 * m12 + v2 * m22 + v3 * m32,
        //                v0 * m03 + v1 * m13 + v2 * m23 + v3 * m33 )
      }

      // handle left-over rows
      switch (r_left)
      {
        case 3: accum = _mm_add_ps(_mm_mul_ps(_mm_load1_ps(v+i),_mm_loadu_ps(m+i*cols+j)), accum); i++;
        case 2: accum = _mm_add_ps(_mm_mul_ps(_mm_load1_ps(v+i),_mm_loadu_ps(m+i*cols+j)), accum); i++;
        case 1: accum = _mm_add_ps(_mm_mul_ps(_mm_load1_ps(v+i),_mm_loadu_ps(m+i*cols+j)), accum);
        case 0: ;
      }

      // store the 4 values of the result vector
      // use stream to avoid polluting the cache
      _mm_stream_ps(r+j,accum);
    }

    // handle the left over columns
    for (; c_left > 0; --c_left) {
      accum = _mm_setzero_ps();
      for (unsigned int i=0; i<rows; ++i)
        accum = _mm_add_ss(_mm_mul_ss(_mm_load_ss(m+i*cols+cols-c_left), _mm_load_ss(v+i)),accum);
      _mm_store_ss(r+cols-c_left,accum);
    }
  }

  static VNL_SSE_FORCE_INLINE void matrix_x_vector(const float* m, const float* v, float* r, unsigned rows, unsigned cols)
  {
    __m128 accum, x,y,z,w,mxy1,mxy2,mzw1,mzw2, mr1,mr2,mr3,mr4;

    // calculate if there are any left-over rows/columns
    unsigned r_left = rows%4;
    unsigned r_nice = rows - r_left;
    unsigned c_left = cols%4;
    unsigned c_nice = cols - c_left;

    // handle 4 matrix rows at a time
    for (unsigned i = 0; i < r_nice; i+=4)
    {
      // handle 4 matrix columns at a time
      accum = _mm_setzero_ps();
      const float *r1 = m+i*cols, *r2 = m+(i+1)*cols,
                  *r3 = m+(i+2)*cols, *r4 = m+(i+3)*cols;
      unsigned j = 0;
      for (; j < c_nice; j+=4)
      {
        // load  vector data so that
        //  w = (v0, v1, v2, v3)
        w = VNL_SSE_HEAP_LOAD(ps)(v+j);

        // after shuffling
        // x = (v0, v0, v0, v0)
        // y = (v1, v1, v1, v1)
        // z = (v2, v2, v2, v2)
        // w = (v3, v3, v3, v3)
        x = _mm_shuffle_ps(w,w,_MM_SHUFFLE(0,0,0,0));
        y = _mm_shuffle_ps(w,w,_MM_SHUFFLE(1,1,1,1));
        z = _mm_shuffle_ps(w,w,_MM_SHUFFLE(2,2,2,2));
        w = _mm_shuffle_ps(w,w,_MM_SHUFFLE(3,3,3,3));

        // load form first two rows of the matrix
        // i.e. mr1 = (m00, m01, m02, m03)
        //      mr2 = (m10, m11, m12, m13)
        mr1 = _mm_loadu_ps(r1+j);
        mr2 = _mm_loadu_ps(r2+j);

        // unpack into xy and zw parts
        // i.e mxy1 = (m00, m10, m01, m11)
        //     mzw1 = (m02, m12, m03, m13)
        mxy1 = _mm_unpacklo_ps(mr1,mr2);
        mzw1 = _mm_unpackhi_ps(mr1,mr2);

        // similarly for the next two rows
        mr3 = _mm_loadu_ps(r3+j);
        mr4 = _mm_loadu_ps(r4+j);

        // unpack into xy and zw parts
        // i.e mxy2 = (m20, m30, m21, m31)
        //     mxy2 = (m22, m32, m23, m33)
        mxy2 = _mm_unpacklo_ps(mr3,mr4);
        mzw2 = _mm_unpackhi_ps(mr3,mr4);

        // move around matrix data and multiply so that
        // x = (v0,v0,v0,v0) * (m00,m10,m20,m30)
        // y = (v1,v1,v1,v1) * (m01,m11,m21,m31)
        // z = (v2,v2,v2,v2) * (m02,m12,m22,m32)
        // w = (v3,v3,v3,v3) * (m03,m13,m23,m33)
#if 1
        __m128 mx = _mm_movelh_ps(mxy1,mxy2);
        x = _mm_mul_ps(x, mx);
        __m128 my = _mm_movehl_ps(mxy2,mxy1);
        y = _mm_mul_ps(y, my);
        __m128 mz = _mm_movelh_ps(mzw1,mzw2);
        z = _mm_mul_ps(z, mz);
        __m128 mw = _mm_movehl_ps(mzw2,mzw1);
        w = _mm_mul_ps(w,mw);
#else
        x = _mm_mul_ps(x,_mm_movelh_ps(mxy1,mxy2));
        y = _mm_mul_ps(y,_mm_movehl_ps(mxy1,mxy2));
        z = _mm_mul_ps(z,_mm_movelh_ps(mzw1,mzw2));
        w = _mm_mul_ps(w,_mm_movehl_ps(mzw1,mzw2));
#endif // 0

        // now sum the four results
        accum = _mm_add_ps(x,accum);
        accum = _mm_add_ps(y,accum);
        accum = _mm_add_ps(z,accum);
        accum = _mm_add_ps(w,accum);

        // accum is now ( v0 * m00 + v1 * m01 + v2 * m02 + v3 * m03,
        //               v0 * m10 + v1 * m11 + v2 * m12 + v3 * m13,
        //               v0 * m20 + v1 * m21 + v2 * m22 + v3 * m23,
        //               v0 * m30 + v1 * m31 + v2 * m32 + v3 * m33 )
      }

      // handle the left over columns
      switch (c_left)
      {
        case 3: accum = _mm_add_ps(_mm_mul_ps(_mm_load1_ps(v+j),_mm_set_ps(*(r4+j),*(r3+j),*(r2+j),*(r1+j))), accum); j++;
        case 2: accum = _mm_add_ps(_mm_mul_ps(_mm_load1_ps(v+j),_mm_set_ps(*(r4+j),*(r3+j),*(r2+j),*(r1+j))), accum); j++;
        case 1: accum = _mm_add_ps(_mm_mul_ps(_mm_load1_ps(v+j),_mm_set_ps(*(r4+j),*(r3+j),*(r2+j),*(r1+j))), accum);
        case 0: ;
      }
      // store the 4 values of the result vector
      // use stream to avoid polluting the cache
      _mm_stream_ps(r+i,accum);
    }

    // handle the left over rows
    for (; r_left > 0; --r_left) {
      accum = _mm_setzero_ps();
      const float* p = m+(rows-r_left)*cols;
      for (unsigned int j=0; j<cols; ++j)
        accum = _mm_add_ss(_mm_mul_ss(_mm_load_ss(p+j), _mm_load_ss(v+j)),accum);
      _mm_store_ss(r+rows-r_left,accum);
    }
  }

  static VNL_SSE_FORCE_INLINE float sum(const float* x, unsigned n)
  {
    float ret;
    __m128 sum = _mm_setzero_ps();
    switch (n%4)
    { // handle vector sizes which aren't divisible by 4
      case 3: sum = _mm_load_ss(x+--n);
      case 2: sum = _mm_shuffle_ps(_mm_load_ss(x+--n), sum ,_MM_SHUFFLE(1,0,0,1));
      case 1: sum = _mm_move_ss(sum,_mm_load_ss(x+--n));
      case 0: ;
    }

    // sum four elements at a time, sum will contain four running totals
    for (int i = n-4; i >= 0; i-=4)
      sum = _mm_add_ps(VNL_SSE_HEAP_LOAD(ps)(x+i),sum);

    // sum will contain 4 accumulated values, need to add them together
    sum = _mm_add_ps(sum,_mm_movehl_ps(_mm_setzero_ps(),sum));
    sum = _mm_add_ss(sum,_mm_shuffle_ps(sum,sum,_MM_SHUFFLE(3,2,1,1)));
    _mm_store_ss(&ret,sum);
    return ret;
  }

  static VNL_SSE_FORCE_INLINE float max(const float* x, unsigned n)
  {
    float ret;
    __m128 min_float = _mm_set1_ps(- FLT_MAX);
    __m128 max = min_float;
    switch (n%4)
    { // handle vector sizes which aren't divisible by 4
      case 3: max = _mm_load_ss(x+--n);
      case 2: max = _mm_shuffle_ps(_mm_load_ss(x+--n), max ,_MM_SHUFFLE(1,0,0,1));
      case 1: max = _mm_move_ss(max,_mm_load_ss(x+--n));
      case 0: ;
    }

    // handle four elements at a time, max will contain four max values
    for (int i = n-4; i >= 0; i-=4)
      max = _mm_max_ps(VNL_SSE_HEAP_LOAD(ps)(x+i), max);

    // need compare max's four values
    max = _mm_max_ps(max,_mm_movehl_ps(min_float,max));
    max = _mm_max_ss(max,_mm_shuffle_ps(max,max,_MM_SHUFFLE(3,2,1,1)));
    _mm_store_ss(&ret,max);

    return ret;
  }

  static VNL_SSE_FORCE_INLINE float min(const float* x, unsigned n)
  {
    float ret;
    __m128 max_float = _mm_set1_ps(FLT_MAX);
    __m128 min = max_float;

    switch (n%4)
    { // handle vector sizes which aren't divisible by 4
      case 3: min = _mm_min_ss(min,_mm_load_ss(x+--n));
      case 2: min = _mm_min_ss(min,_mm_load_ss(x+--n));
      case 1: min = _mm_min_ss(min,_mm_load_ss(x+--n));
      case 0: ;
    }

    // handle four elements at a time, min will contain four min values
    for (int i = n-4; i >= 0; i-=4)
      min = _mm_min_ps(VNL_SSE_HEAP_LOAD(ps)(x+i), min);


    // need compare min's four values
    min = _mm_min_ps(min,_mm_movehl_ps(max_float,min));
    min = _mm_min_ss(min,_mm_shuffle_ps(min,min,_MM_SHUFFLE(3,2,1,1)));
    _mm_store_ss(&ret,min);

    return ret;
  }

  static VNL_SSE_FORCE_INLINE unsigned arg_max(const float* x, unsigned n)
  {
    __m128  max = _mm_set1_ps(- FLT_MAX);
    __m128  input;
    __m128i input_idx = _mm_set_epi32(3, 2, 1, 0);
    __m128i input_idx_increment = _mm_set1_epi32(4);
    __m128i arg_max = _mm_set1_epi32(0);
    union IsMaxMask
      {
      __m128  m128;
      __m128i m128i;
      };
    IsMaxMask is_max;

    const int n16 = (n/4) * 4;
    // handle two elements at a time, max will contain two max values
    for (int i=0; i<n16; i+=4)
      {
      input = VNL_SSE_HEAP_LOAD(ps)(x+i);
      max = _mm_max_ps(input, max);
      is_max.m128 = _mm_cmpeq_ps(max, input);
      arg_max = VNL_MM_MAX_EPI32(arg_max, _mm_and_si128(is_max.m128i, input_idx));
      input_idx = _mm_add_epi32(input_idx, input_idx_increment);
      }

    // decision logic for vector sizes whach aren't divisible by 4
    int mod = n%4;
    n = n-mod;
    input_idx_increment = _mm_set1_epi32(1);
    while (mod != 0)
      {
      input_idx = _mm_set1_epi32(n);
      input = _mm_load1_ps(x+n);
      max = _mm_max_ps(max, input);
      is_max.m128 = _mm_cmpeq_ps(max, input);
      arg_max = VNL_MM_MAX_EPI32(arg_max, _mm_and_si128(is_max.m128i, input_idx));
      --mod;
      ++n;
      input_idx = _mm_add_epi32(input_idx, input_idx_increment);
      }

    // need to store the index of the max value
    is_max.m128 = max;
    max = _mm_max_ps(max, _mm_shuffle_ps(max, max, _MM_SHUFFLE(2,3,0,1)));
    max = _mm_max_ps(max, _mm_movehl_ps(max, max));
    max = _mm_shuffle_ps(max, max, _MM_SHUFFLE(0,0,0,0));
    is_max.m128 = _mm_cmpeq_ps(is_max.m128, max);
    arg_max = _mm_and_si128(is_max.m128i, arg_max);
    arg_max = VNL_MM_MAX_EPI32(arg_max, _mm_unpackhi_epi32(arg_max, _mm_set1_epi32(0)));
    arg_max = VNL_MM_MAX_EPI32(arg_max, _mm_srli_si128(arg_max, 4));
    unsigned ret = _mm_cvtsi128_si32(arg_max);
    return ret;
  }

  static VNL_SSE_FORCE_INLINE unsigned arg_min(const float* x, unsigned n)
  {
    __m128  min = _mm_set1_ps(FLT_MAX);
    __m128  input;
    __m128i input_idx = _mm_set_epi32(3, 2, 1, 0);
    __m128i input_idx_increment = _mm_set1_epi32(4);
    __m128i arg_min = _mm_set1_epi32(0);
    union IsMinMask
      {
      __m128  m128;
      __m128i m128i;
      };
    IsMinMask is_min;

    const int n16 = (n/4) * 4;
    // handle two elements at a time, max will contain two max values
    for (int i=0; i<n16; i+=4)
      {
      input = VNL_SSE_HEAP_LOAD(ps)(x+i);
      min = _mm_min_ps(input, min);
      is_min.m128 = _mm_cmpeq_ps(min, input);
      arg_min = VNL_MM_MAX_EPI32(arg_min, _mm_and_si128(is_min.m128i, input_idx));
      input_idx = _mm_add_epi32(input_idx, input_idx_increment);
      }

    //// decision logic for vector sizes whach aren't divisible by 4
    int mod = n%4;
    n = n-mod;
    input_idx_increment = _mm_set1_epi32(1);
    while (mod != 0)
      {
      input_idx = _mm_set1_epi32(n);
      input = _mm_load1_ps(x+n);
      min = _mm_min_ps(min, input);
      is_min.m128 = _mm_cmpeq_ps(min, input);
      arg_min = VNL_MM_MAX_EPI32(arg_min, _mm_and_si128(is_min.m128i, input_idx));
      --mod;
      ++n;
      input_idx = _mm_add_epi32(input_idx, input_idx_increment);
      }

    // need to store the index of the max value
    is_min.m128 = min;
    min = _mm_min_ps(min, _mm_shuffle_ps(min, min, _MM_SHUFFLE(2,3,0,1)));
    min = _mm_min_ps(min, _mm_movehl_ps(min, min));
    min = _mm_shuffle_ps(min, min, _MM_SHUFFLE(0,0,0,0));
    is_min.m128 = _mm_cmpeq_ps(is_min.m128, min);
    arg_min = _mm_and_si128(is_min.m128i, arg_min);
    arg_min = VNL_MM_MAX_EPI32(arg_min, _mm_unpackhi_epi32(arg_min, _mm_set1_epi32(0)));
    arg_min = VNL_MM_MAX_EPI32(arg_min, _mm_srli_si128(arg_min, 4));
    unsigned ret = _mm_cvtsi128_si32(arg_min);
    return ret;
  }
};

#endif // VNL_CONFIG_ENABLE_SSE2

#endif // vnl_sse_h_

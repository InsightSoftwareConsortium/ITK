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

#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif // for macro decisions based on compiler type
#include <vxl_config.h>   // for checking supported integer data types
#include <cfloat>// for DBL_MAX and FLT_MAX

#include <vnl/vnl_config.h> // is SSE enabled
#include "vnl_alloc.h"  // is SSE enabled
#include "vnl/vnl_export.h"

// some caveats...
// - Due to the way vnl_matrix is represented in memory cannot guarantee 16-byte alignment,
//   therefore have to use slower unaligned loading intrinsics for matrices.
// - The GCC 3.4 intrinsics seem to be horrendously slow...



// Try and use compiler instructions for forcing inlining if possible
// Also instruction for aligning stack memory is compiler dependent
#if defined(__GNUC__)
// With attribute always_inline, gcc can give an error if a function
// cannot be inlined, so it is disabled.  Problem seen on 64 bit
// platforms with std::vector<vnl_rational>.
# define VNL_SSE_FORCE_INLINE /* __attribute__((always_inline)) */ inline
# define VNL_SSE_STACK_ALIGNED(x)  __attribute__((aligned(x))) VNL_EXPORT
#elif defined _MSC_VER
# define VNL_SSE_FORCE_INLINE __forceinline
# define VNL_SSE_STACK_ALIGNED(x)  __declspec(align(x)) VNL_EXPORT
#else
# define VNL_SSE_FORCE_INLINE inline
# define VNL_SSE_STACK_ALIGNED(x)
# define VNL_SSE_STACK_STORE(pf) _mm_storeu_##pf // no stack alignment so use unaligned store (slower!)
#endif



# define VNL_SSE_HEAP_STORE(pf) _mm_storeu_##pf
# define VNL_SSE_HEAP_LOAD(pf) _mm_loadu_##pf
# if VNL_CONFIG_THREAD_SAFE
#   define VNL_SSE_ALLOC(n,s,a) new char[(n)*(s)]
#   define VNL_SSE_FREE(v,n,s) (delete [] static_cast<char*>(v))
# else
#   define VNL_SSE_ALLOC(n,s,a) vnl_alloc::allocate((n == 0) ? 8 : (n * s));
#   define VNL_SSE_FREE(v,n,s) if (v) vnl_alloc::deallocate(v, (n == 0) ? 8 : (n * s));
# endif


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


#endif // vnl_sse_h_

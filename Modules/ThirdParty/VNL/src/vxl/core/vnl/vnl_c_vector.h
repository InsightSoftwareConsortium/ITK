// This is core/vnl/vnl_c_vector.h
#ifndef vnl_c_vector_h_
#define vnl_c_vector_h_
//:
// \file
// \brief Math on blocks of memory
//
//    vnl_c_vector interfaces to low-level memory-block operations.
//
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   12 Feb 1998
//
// \verbatim
//  Modifications
//   1998-02-12 AWF              Initial version.
//   2001-03-26 LSB (Manchester) Tidied documentation
//   2009-03-30 Peter Vanroose   added arg_min() and arg_max()
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <iosfwd>
#include <cstddef>
#include <cmath>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <vnl/vnl_numeric_traits.h>
#include <vnl/vnl_export.h>

// avoid messing about with aux_* functions for gcc 2.7 -- fsm
template <class T, class S> VNL_EXPORT void vnl_c_vector_one_norm(T const *p, unsigned n, S *out);
template <class T, class S> VNL_EXPORT void vnl_c_vector_two_norm(T const *p, unsigned n, S *out);
template <class T, class S> VNL_EXPORT void vnl_c_vector_inf_norm(T const *p, unsigned n, S *out);
template <class T, class S> VNL_EXPORT void vnl_c_vector_two_norm_squared(T const *p, unsigned n, S *out);
template <class T, class S> VNL_EXPORT void vnl_c_vector_rms_norm(T const *p, unsigned n, S *out);

//: vnl_c_vector interfaces to lowlevel memory-block operations.
template <class T>
class VNL_EXPORT vnl_c_vector
{
 public:
  typedef typename vnl_numeric_traits<T>::abs_t abs_t;
  typedef typename vnl_numeric_traits<T>::real_t real_t;

  static T sum(T const* v, unsigned n);
  static inline abs_t squared_magnitude(T const *p, unsigned n)
  { abs_t val; vnl_c_vector_two_norm_squared(p, n, &val); return val; }
  static void normalize(T *, unsigned n);
  static void apply(T const *, unsigned, T (*f)(T), T* v_out);
  static void apply(T const *, unsigned, T (*f)(T const&), T* v_out);

  //: y[i]  = x[i]
  static void copy    (T const *x, T       *y, unsigned);

  //:  y[i]  = a*x[i]
  static void scale   (T const *x, T       *y, unsigned, T const &);

  //: z[i]  = x[i] + y[i];
  static void add     (T const *x, T const *y, T *z, unsigned);

  //: z[i]  = x[i] + y;
  static void add     (T const *x, T const& y, T *z, unsigned);

  //: z[i]  = x[i] - y[i]
  static void subtract(T const *x, T const *y, T *z, unsigned);

  //: z[i]  = x[i] - y[i]
  static void subtract(T const *x, T const& y, T *z, unsigned);

  //: z[i]  = x[i] * y[i]
  static void multiply(T const *x, T const *y, T *z, unsigned);

  //: z[i]  = x[i] * y[i]
  static void multiply(T const *x, T const& y, T *z, unsigned);

  //: z[i]  = x[i] / y[i]
  static void divide  (T const *x, T const *y, T *z, unsigned);

  //: z[i]  = x[i] / y[i]
  static void divide  (T const *x, T const& y, T *z, unsigned);

  //: y[i]  = -x[i]
  // Note that this is a no-op when T is an unsigned type.
  static void negate  (T const *x, T       *y, unsigned);

  //: y[i]  = 1/x[i]
  static void invert  (T const *x, T       *y, unsigned);

  //:  y[i] += a*x[i]
  static void saxpy   (T const &a, T const *x, T *y, unsigned);

  //: x[i]  = v
  static void fill    (T *x, unsigned, T const &v);


  static void reverse (T *x, unsigned);
  static T dot_product  (T const *, T const *, unsigned);

  //: conjugate second
  static T inner_product(T const *, T const *, unsigned);
  static void conjugate(T const *, T *, unsigned);

  static T max_value(T const *, unsigned);
  static T min_value(T const *, unsigned);
  static unsigned arg_max(T const *, unsigned);
  static unsigned arg_min(T const *, unsigned);

  static T mean(T const *p, unsigned n) { return T(sum(p,n)/abs_t(n)); }

  //: The standard deviation
  // This method uses the 1/(n-1) normalisation, assuming that your
  // data is a sample of a population.
  static inline real_t std(T const *p, unsigned n) {
    return std::sqrt(real_t(sum_sq_diff_means(p, n))/real_t(abs_t(n-1)));}

  //: The sum of squared differences from the mean
  static T sum_sq_diff_means(T const* v, unsigned n);

  //:  one_norm : sum of abs values
  static inline abs_t one_norm(T const *p, unsigned n)
  { abs_t val; vnl_c_vector_one_norm(p, n, &val); return val; }

  //: two_norm : sqrt of sum of squared abs values
  static inline abs_t two_norm(T const *p, unsigned n)
  { abs_t val; vnl_c_vector_two_norm(p, n, &val); return val; }

 //: inf_norm : max of abs values
  static inline abs_t inf_norm(T const *p, unsigned n)
  { abs_t val; vnl_c_vector_inf_norm(p, n, &val); return val; }

  //: two_nrm2 : sum of squared abs values
  static inline abs_t two_nrm2(T const *p, unsigned n)
  { abs_t val; vnl_c_vector_two_norm_squared(p, n, &val); return val; }

  //: rms_norm : sqrt of mean sum of squared abs values
  static inline abs_t rms_norm(T const *p, unsigned n)
  { abs_t val; vnl_c_vector_rms_norm(p, n, &val); return val; }

  //: Euclidean Distance between two vectors.
  // Sum of Differences squared.
  static T euclid_dist_sq(T const *, T const *, unsigned);

  //: Memory allocation
  static T** allocate_Tptr(const std::size_t n);
  static T*  allocate_T(const std::size_t n);
  static void deallocate(T**, const std::size_t n_when_allocated);
  static void deallocate(T*, const std::size_t n_when_allocated);
};

//: Input & output
// \relatesalso vnl_c_vector
template <class T> VNL_EXPORT
std::ostream& print_vector(std::ostream&, T const*, unsigned);

#endif // vnl_c_vector_h_

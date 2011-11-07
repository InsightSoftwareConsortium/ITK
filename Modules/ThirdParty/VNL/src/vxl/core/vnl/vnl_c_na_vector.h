// This is core/vnl/vnl_c_na_vector.h
#ifndef vnl_c_na_vector_h_
#define vnl_c_na_vector_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Math on blocks of memory
//
//    NA aware vnl_c_vector-like interfaces to lowlevel memory-block operations.
//
// \author Andrew W. Fitzgibbon, Ian Scott
// \date   3 Nov 2010
//
// \verbatim
//  Modifications
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vcl_iosfwd.h>
#include <vnl/vnl_numeric_traits.h>
#include <vcl_cmath.h> // for vcl_sqrt

// avoid messing about with aux_* functions for gcc 2.7 -- fsm
#if 0
template <class T, class S> void vnl_c_na_vector_inf_norm(T const *p, unsigned n, S *out);
template <class T, class S> void vnl_c_na_vector_rms_norm(T const *p, unsigned n, S *out);
#endif
template <class T, class S> void vnl_c_na_vector_one_norm(T const *p, unsigned n, S *out);
template <class T, class S> void vnl_c_na_vector_two_norm(T const *p, unsigned n, S *out);
template <class T, class S> void vnl_c_na_vector_two_norm_squared(T const *p, unsigned n, S *out);

//: vnl_c_na_vector interfaces to NA-aware lowlevel memory-block operations.
template <class T>
class vnl_c_na_vector
{
 public:
  typedef typename vnl_numeric_traits<T>::abs_t abs_t;
  typedef typename vnl_numeric_traits<T>::real_t real_t;

  static T sum(T const* v, unsigned n);
  static inline abs_t squared_magnitude(T const *p, unsigned n)
  { abs_t val; vnl_c_na_vector_two_norm_squared(p, n, &val); return val; }
#if 0
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
#endif

  static T mean(T const *p, unsigned n);

#if 0
  //: The standard deviation
  // This method uses the 1/(n-1) normalisation, assuming that your
  // data is a sample of a population.
  static inline real_t std(T const *p, unsigned n) {
    return vcl_sqrt(real_t(sum_sq_diff_means(p, n))/real_t(abs_t(n-1)));}

  //: The sum of squared differences from the mean
  static T sum_sq_diff_means(T const* v, unsigned n);
#endif

  //:  one_norm : sum of abs values
  static inline abs_t one_norm(T const *p, unsigned n)
  { abs_t val; vnl_c_na_vector_one_norm(p, n, &val); return val; }

  //: two_norm : sqrt of sum of squared abs values
  static inline abs_t two_norm(T const *p, unsigned n)
  { abs_t val; vnl_c_na_vector_two_norm(p, n, &val); return val; }

  //: two_nrm2 : sum of squared abs values
  static inline abs_t two_nrm2(T const *p, unsigned n)
  { abs_t val; vnl_c_na_vector_two_norm_squared(p, n, &val); return val; }

#if 0
 //: inf_norm : max of abs values
  static inline abs_t inf_norm(T const *p, unsigned n)
  { abs_t val; vnl_c_na_vector_inf_norm(p, n, &val); return val; }

  //: rms_norm : sqrt of mean sum of squared abs values
  static inline abs_t rms_norm(T const *p, unsigned n)
  { abs_t val; vnl_c_na_vector_rms_norm(p, n, &val); return val; }

  //: Euclidean Distance between two vectors.
  // Sum of Differences squared.
  static T euclid_dist_sq(T const *, T const *, unsigned);
#endif
};

//: Input & output
// \relatesalso vnl_c_na_vector
template <class T>
vcl_ostream& print_na_vector(vcl_ostream&, T const*, unsigned);

#endif // vnl_c_na_vector_h_

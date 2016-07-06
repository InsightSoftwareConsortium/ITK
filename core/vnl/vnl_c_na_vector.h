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

#include <iosfwd>
#include <cmath>
#include <vcl_compiler.h>
#include <vnl/vnl_numeric_traits.h>
#include "vnl/vnl_export.h"

// avoid messing about with aux_* functions for gcc 2.7 -- fsm
template <class T, class S> VNL_TEMPLATE_EXPORT void vnl_c_na_vector_one_norm(T const *p, unsigned n, S *out);
template <class T, class S> VNL_TEMPLATE_EXPORT void vnl_c_na_vector_two_norm(T const *p, unsigned n, S *out);
template <class T, class S> VNL_TEMPLATE_EXPORT void vnl_c_na_vector_two_norm_squared(T const *p, unsigned n, S *out);

//: vnl_c_na_vector interfaces to NA-aware lowlevel memory-block operations.
VCL_TEMPLATE_EXPORT template <class T>
class VNL_TEMPLATE_EXPORT vnl_c_na_vector
{
 public:
  typedef typename vnl_numeric_traits<T>::abs_t abs_t;
  typedef typename vnl_numeric_traits<T>::real_t real_t;

  static T sum(T const* v, unsigned n);
  static inline abs_t squared_magnitude(T const *p, unsigned n)
  { abs_t val; vnl_c_na_vector_two_norm_squared(p, n, &val); return val; }

  static T mean(T const *p, unsigned n);

  //:  one_norm : sum of abs values
  static inline abs_t one_norm(T const *p, unsigned n)
  { abs_t val; vnl_c_na_vector_one_norm(p, n, &val); return val; }

  //: two_norm : sqrt of sum of squared abs values
  static inline abs_t two_norm(T const *p, unsigned n)
  { abs_t val; vnl_c_na_vector_two_norm(p, n, &val); return val; }

  //: two_nrm2 : sum of squared abs values
  static inline abs_t two_nrm2(T const *p, unsigned n)
  { abs_t val; vnl_c_na_vector_two_norm_squared(p, n, &val); return val; }
};

//: Input & output
// \relatesalso vnl_c_na_vector
template <class T> VNL_TEMPLATE_EXPORT
std::ostream& print_na_vector(std::ostream&, T const*, unsigned);

#endif // vnl_c_na_vector_h_

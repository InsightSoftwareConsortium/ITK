// This is core/vnl/vnl_diag_matrix_fixed.txx
#ifndef vnl_diag_matrix_fixed_txx_
#define vnl_diag_matrix_fixed_txx_
//:
// \file

#include "vnl_diag_matrix_fixed.h"

#include <vcl_iostream.h>


//: Return inv(D) * b.
template <class T, unsigned int N>
vnl_vector_fixed<T,N> vnl_diag_matrix_fixed<T,N>::solve(vnl_vector_fixed<T,N> const& b) const
{
  vnl_vector_fixed<T,N> ret;
  for (unsigned i = 0; i < N; ++i)
    ret[i] = b[i] / diagonal_[i];
  return ret;
}

//: Return inv(D) * b.
template <class T, unsigned int N>
void vnl_diag_matrix_fixed<T,N>::solve(vnl_vector_fixed<T,N> const& b, vnl_vector_fixed<T,N>* out) const
{
  for (unsigned i = 0; i < N; ++i)
    (*out)[i] = b[i] / diagonal_[i];
}

//: Print in MATLAB diag([1 2 3]) form.
template <class T, unsigned int N>
vcl_ostream& operator<< (vcl_ostream& s, const vnl_diag_matrix_fixed<T,N>& D)
{
  s << "diag([ ";
  for (unsigned i=0; i<N; ++i)
    s << D(i,i) << ' ';
  return s << "])";
}

#if 0
//: Compares two matrices for component-wise equality within a small epsilon
template <class T, unsigned int N>
bool epsilon_equals (const vnl_diag_matrix_fixed<T>& m1, const vnl_diag_matrix_fixed<T>& m2,
                     double alt_epsilon)
{
  if (alt_epsilon < 0)
  {
    vcl_cerr << "Negative alt_epsilon passed to epsilon_equals: returning false\n";
    return false;
  }

  if (m1.rows() != m2.rows())
     return false;              // different sizes.

  double local_epsilon;
  if (alt_epsilon == 0)
    local_epsilon = comparison_epsilon<T>::epsilon;
  else
    local_epsilon = alt_epsilon;

  for (unsigned long i = 0; i < N; i++) {
#if 0
    T result = m1(i,i) - m2(i,i);
    if (result < 0)
      result = 0 - result;
    if (result > local_epsilon)
      return false;
#endif
    if (m1(i,i) - m2(i,i) > local_epsilon ||
        m2(i,i) - m1(i,i) > local_epsilon) // avoid using vcl_abs()
      return false;
  }
  return true;
}
#endif


#undef VNL_DIAG_MATRIX_FIXED_INSTANTIATE
#define VNL_DIAG_MATRIX_FIXED_INSTANTIATE(T , N ) \
template class vnl_diag_matrix_fixed<T , N >; \
template vcl_ostream& operator<< (vcl_ostream& s, vnl_diag_matrix_fixed<T , N > const &)

//template bool epsilon_equals (vnl_diag_matrix_fixed<T > const & , vnl_diag_matrix_fixed<T > const & , double)

#endif // vnl_diag_matrix_fixed_txx_

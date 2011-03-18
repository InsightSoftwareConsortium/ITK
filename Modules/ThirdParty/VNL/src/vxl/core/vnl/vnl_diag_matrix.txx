// This is core/vnl/vnl_diag_matrix.txx
#ifndef vnl_diag_matrix_txx_
#define vnl_diag_matrix_txx_
//:
// \file

#include "vnl_diag_matrix.h"

#include <vcl_iostream.h>


//: Return inv(D) * b.
template <class T>
vnl_vector<T> vnl_diag_matrix<T>::solve(vnl_vector<T> const& b) const
{
  unsigned len = diagonal_.size();
  vnl_vector<T> ret(len);
  for (unsigned i = 0; i < len; ++i)
    ret[i] = b[i] / diagonal_[i];
  return ret;
}

//: Return inv(D) * b.
template <class T>
void vnl_diag_matrix<T>::solve(vnl_vector<T> const& b, vnl_vector<T>* out) const
{
  unsigned len = diagonal_.size();
  for (unsigned i = 0; i < len; ++i)
    (*out)[i] = b[i] / diagonal_[i];
}

//: Print in MATLAB diag([1 2 3]) form.
template <class T>
vcl_ostream& operator<< (vcl_ostream& s, const vnl_diag_matrix<T>& D)
{
  s << "diag([ ";
  for (unsigned i=0; i<D.rows(); ++i)
    s << D(i,i) << ' ';
  return s << "])";
}

#if 0
//: Compares two matrices for component-wise equality within a small epsilon
template<class T>
bool epsilon_equals (const vnl_diag_matrix<T>& m1, const vnl_diag_matrix<T>& m2,
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

  for (unsigned long i = 0; i < m1.rows(); i++) {
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


#undef VNL_DIAG_MATRIX_INSTANTIATE
#define VNL_DIAG_MATRIX_INSTANTIATE(T) \
template class vnl_diag_matrix<T >; \
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator* (vnl_matrix<T > const &, vnl_diag_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator* (vnl_diag_matrix<T > const &, vnl_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator+ (vnl_matrix<T > const &, vnl_diag_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator+ (vnl_diag_matrix<T > const &, vnl_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator- (vnl_matrix<T > const &, vnl_diag_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator- (vnl_diag_matrix<T > const &, vnl_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_vector<T > operator* (const vnl_vector<T >&, vnl_diag_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_vector<T > operator* (vnl_diag_matrix<T > const &, const vnl_vector<T >&));\
template vcl_ostream& operator<< (vcl_ostream& s, vnl_diag_matrix<T > const &)

//template bool epsilon_equals (vnl_diag_matrix<T > const & , vnl_diag_matrix<T > const & , double)

#endif // vnl_diag_matrix_txx_

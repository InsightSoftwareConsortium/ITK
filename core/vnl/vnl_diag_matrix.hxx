// This is core/vnl/vnl_diag_matrix.hxx
#ifndef vnl_diag_matrix_hxx_
#define vnl_diag_matrix_hxx_
//:
// \file

#include <iostream>
#include "vnl_diag_matrix.h"

#include <vcl_compiler.h>


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
std::ostream& operator<< (std::ostream& s, const vnl_diag_matrix<T>& D)
{
  s << "diag([ ";
  for (unsigned i=0; i<D.rows(); ++i)
    s << D(i,i) << ' ';
  return s << "])";
}

#undef VNL_DIAG_MATRIX_INSTANTIATE
#define VNL_DIAG_MATRIX_INSTANTIATE(T) \
template class VNL_EXPORT vnl_diag_matrix<T >; \
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator* (vnl_matrix<T > const &, vnl_diag_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator* (vnl_diag_matrix<T > const &, vnl_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator+ (vnl_matrix<T > const &, vnl_diag_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator+ (vnl_diag_matrix<T > const &, vnl_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator- (vnl_matrix<T > const &, vnl_diag_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_matrix<T > operator- (vnl_diag_matrix<T > const &, vnl_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_vector<T > operator* (const vnl_vector<T >&, vnl_diag_matrix<T > const &));\
VCL_INSTANTIATE_INLINE(vnl_vector<T > operator* (vnl_diag_matrix<T > const &, const vnl_vector<T >&));\
template VNL_EXPORT std::ostream& operator<< (std::ostream& s, vnl_diag_matrix<T > const &)

//template bool epsilon_equals (vnl_diag_matrix<T > const & , vnl_diag_matrix<T > const & , double)

#endif // vnl_diag_matrix_hxx_

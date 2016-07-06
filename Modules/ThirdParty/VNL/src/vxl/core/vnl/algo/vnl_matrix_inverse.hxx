// This is core/vnl/algo/vnl_matrix_inverse.hxx
#ifndef vnl_matrix_inverse_hxx_
#define vnl_matrix_inverse_hxx_
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 22 Nov 96
//
//-----------------------------------------------------------------------------

#include "vnl_matrix_inverse.h"

#undef VNL_MATRIX_INVERSE_INSTANTIATE
#define VNL_MATRIX_INVERSE_INSTANTIATE(T) \
template struct VNL_ALGO_EXPORT vnl_matrix_inverse<T >;\
VCL_INSTANTIATE_INLINE( vnl_vector<T > operator*(vnl_matrix_inverse<T > const &, vnl_vector<T > const &) ); \
VCL_INSTANTIATE_INLINE( vnl_matrix<T > operator*(vnl_matrix_inverse<T > const &, vnl_matrix<T > const &) )

#endif // vnl_matrix_inverse_hxx_

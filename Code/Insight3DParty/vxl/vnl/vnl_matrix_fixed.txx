#include "vnl_matrix_fixed.h"

#ifndef __SUNPRO_CC
# undef VNL_MATRIX_FIXED_PAIR_INSTANTIATE
# ifdef WIN32
   // vc60 barfs at this -awf
#  define VNL_MATRIX_FIXED_PAIR_INSTANTIATE(T, M, N, O)
# else
#  define VNL_MATRIX_FIXED_PAIR_INSTANTIATE(T, M, N, O) \
   template vnl_matrix_fixed<T, M, O> operator*(const vnl_matrix_fixed<T, M, N>& a, const vnl_matrix_fixed<T, N, O>& b);
# endif

#else
# undef VNL_MATRIX_FIXED_PAIR_INSTANTIATE
# define VNL_MATRIX_FIXED_PAIR_INSTANTIATE(T, M, N, O)
#endif



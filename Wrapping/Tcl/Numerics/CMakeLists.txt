CABLE_CLASS_SET(ComplexScalar float double "long double")

CABLE_CLASS_SET(VScalar
  int
  long
  float
  double
  "long double"
  "signed char"
  "unsigned char"
  "unsigned int"
  "unsigned long"
  "vcl_complex<$ComplexScalar >"
)


ADD_LIBRARY(VXLNumericsTcl SHARED)
CABLE_WRAP_TCL(VXLNumericsTcl
  "vnl_matrix<$VScalar >;vnl_vector.h"
  "vnl_vector<$VScalar >;vnl_matrix.h"
)

LINK_LIBRARIES(
VXLNumerics
${CABLE_TCL_LIBRARY}
${TCL_LIBRARY}
)

#  vnl_c_vector<bool>
#  vnl_c_vector<double>
#  vnl_c_vector<double_complex>
#  vnl_c_vector<float>
#  vnl_c_vector<float_complex>
#  vnl_c_vector<int>
#  vnl_c_vector<long>
#  vnl_c_vector<long_double>
#  vnl_c_vector<long_double_complex>
#  vnl_c_vector<schar>
#  vnl_c_vector<uchar>
#  vnl_c_vector<uint>
#  vnl_c_vector<ulong>
#  vnl_diag_matrix<double>
#  vnl_diag_matrix<double_complex>
#  vnl_diag_matrix<float>
#  vnl_diag_matrix<float_complex>
#  vnl_diag_matrix<int>
#  vnl_diag_matrix<long_double>
#  vnl_file_matrix<double>
#  vnl_file_matrix<float>
#  vnl_file_vector<double>
#  vnl_fortran_copy<double>
#  vnl_fortran_copy<double_complex>
#  vnl_fortran_copy<float>
#  vnl_fortran_copy<float_complex>

#  vnl_matrix_exp<double>
#  vnl_matrix_fixed<double,2,2>
#  vnl_matrix_fixed<double,2,3>
#  vnl_matrix_fixed<double,2,6>
#  vnl_matrix_fixed<double,3,12>
#  vnl_matrix_fixed<double,3,3>
#  vnl_matrix_fixed<double,3,4>
#  vnl_matrix_fixed<double,4,3>
#  vnl_matrix_fixed<double,4,4>
#  vnl_matrix_fixed<float,3,3>
#  vnl_matrix_fixed_ref<double,2,2>
#  vnl_matrix_fixed_ref<double,2,3>
#  vnl_matrix_fixed_ref<double,3,12>
#  vnl_matrix_fixed_ref<double,3,3>
#  vnl_matrix_fixed_ref<double,3,4>
#  vnl_matrix_fixed_ref<double,4,3>
#  vnl_matrix_fixed_ref<double,4,4>
#  vnl_matrix_ref<double>
#  vnl_matrix_ref<float>
#  vnl_quaternion<double>
#  vnl_sparse_matrix<double>
#  vnl_sparse_matrix<float>
#
#  vnl_vector_ref<double>
#  vnl_vector_ref<int>


#  vcl_vector<vnl_amoeba_SimplexCorner>
#  vnl_adjugate<double>
#  vnl_fft1d<double>
#  vnl_fft1d<float>
#  vnl_fft2d
#  vnl_fft_1d<double>
#  vnl_fft_1d<float>
#  vnl_fft_2d<double>
#  vnl_fft_2d<float>
#  vnl_fft_base<1,double>
#  vnl_fft_base<1,float>
#  vnl_fft_base<2,double>
#  vnl_fft_base<2,float>
#  vnl_fft_prime_factors<double>
#  vnl_fft_prime_factors<float>
#  vnl_matrix_inverse<double>
#  vnl_orthogonal_complement<double>
#  vnl_orthogonal_complement<double_complex>
#  vnl_qr<double>
#  vnl_qr<double_complex>
#  vnl_qr<float>
#  vnl_qr<float_complex>
#  vnl_scatter_3x3<double>
#  vnl_scatter_3x3<float>
#  vnl_svd<double>
#  vnl_svd<double_complex>
#  vnl_svd<float>
#  vnl_svd<float_complex>
#  vnl_svd<long_double>
#  vnl_svd<long_double_complex>

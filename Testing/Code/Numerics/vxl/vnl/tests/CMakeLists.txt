SET(vnl_test_SRCS
test_fft1d.cxx
test_fft2d.cxx
test_math.cxx
test_vector.cxx
test_matrix.cxx
test_matrix_fixed.cxx
test_rnpoly_roots.cxx
test_sparse_matrix.cxx
test_sample.cxx
test_functions.cxx
test_numeric_limits.cxx
test_rpoly_roots.cxx
test_cpoly_roots.cxx
test_resize.cxx
test_complex.cxx
test_determinant.cxx
test_qr.cxx
test_qsvd.cxx
test_svd.cxx
test_real_eigensystem.cxx
test_complex_eigensystem.cxx
test_symmetric_eigensystem.cxx
test_generalized_eigensystem.cxx
test_matrix_exp.cxx
)

ADD_EXECUTABLE(vnlTests vnlTests.cxx ${vnl_test_SRCS})
TARGET_LINK_LIBRARIES(vnlTests VXLNumerics ITKCommon)
ADD_TEST(test_fft1d vnlTests test_fft1d )
ADD_TEST(test_fft2d vnlTests test_fft2d )
ADD_TEST(test_math vnlTests test_math )
ADD_TEST(test_vector vnlTests test_vector )
ADD_TEST(test_matrix vnlTests test_matrix )
ADD_TEST(test_matrix_fixed vnlTests test_matrix_fixed )
ADD_TEST(test_rnpoly_roots vnlTests test_rnpoly_roots )
ADD_TEST(test_sparse_matrix vnlTests test_sparse_matrix )
ADD_TEST(test_sample vnlTests test_sample )
ADD_TEST(test_functions vnlTests test_functions )
ADD_TEST(test_numeric_limits vnlTests test_numeric_limits )
ADD_TEST(test_rpoly_roots vnlTests test_rpoly_roots )
ADD_TEST(test_cpoly_roots vnlTests test_cpoly_roots )
ADD_TEST(test_resize vnlTests test_resize )
ADD_TEST(test_complex vnlTests test_complex )
ADD_TEST(test_determinant vnlTests test_determinant )
ADD_TEST(test_qr vnlTests test_qr )
ADD_TEST(test_qsvd vnlTests test_qsvd)
ADD_TEST(test_svd vnlTests test_svd )
ADD_TEST(test_real_eigensystem vnlTests  test_real_eigensystem )
ADD_TEST(test_complex_eigensystem vnlTests test_complex_eigensystem )
ADD_TEST(test_symmetric_eigensystem vnlTests test_symmetric_eigensystem )
ADD_TEST(test_generalized_eigensystem vnlTests   test_generalized_eigensystem)
ADD_TEST(test_matrix_exp vnlTests test_matrix_exp )


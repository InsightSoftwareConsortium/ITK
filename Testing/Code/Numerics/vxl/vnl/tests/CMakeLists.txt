SET(VNL_TESTS ${CXX_TEST_PATH}/vnlTests)
SET(VNL_TESTS2 ${CXX_TEST_PATH}/vnlTests2)

SET(vnl_test_SRCS
test_fft1d.cxx
test_fft2d.cxx
test_math.cxx
test_vector.cxx
test_matrix.cxx
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

SET(vnl_test2_SRCS
test_matrix_fixed.cxx
)

ADD_EXECUTABLE(vnlTests vnlTests.cxx ${vnl_test_SRCS})
TARGET_LINK_LIBRARIES(vnlTests VXLNumerics ITKIO ITKCommon)

ADD_TEST(test_fft1d ${VNL_TESTS} test_fft1d )
ADD_TEST(test_fft2d ${VNL_TESTS} test_fft2d )
ADD_TEST(test_math ${VNL_TESTS} test_math )
ADD_TEST(test_vector ${VNL_TESTS} test_vector )
ADD_TEST(test_matrix ${VNL_TESTS} test_matrix )
ADD_TEST(test_rnpoly_roots ${VNL_TESTS} test_rnpoly_roots )
ADD_TEST(test_sparse_matrix ${VNL_TESTS} test_sparse_matrix )
ADD_TEST(test_sample ${VNL_TESTS} test_sample )
ADD_TEST(test_functions ${VNL_TESTS} test_functions )
ADD_TEST(test_numeric_limits ${VNL_TESTS} test_numeric_limits )
ADD_TEST(test_rpoly_roots ${VNL_TESTS} test_rpoly_roots )
ADD_TEST(test_cpoly_roots ${VNL_TESTS} test_cpoly_roots )
ADD_TEST(test_resize ${VNL_TESTS} test_resize )
ADD_TEST(test_complex ${VNL_TESTS} test_complex )
ADD_TEST(test_determinant ${VNL_TESTS} test_determinant )
ADD_TEST(test_qr ${VNL_TESTS} test_qr )
ADD_TEST(test_qsvd ${VNL_TESTS} test_qsvd)
ADD_TEST(test_svd ${VNL_TESTS} test_svd )
ADD_TEST(test_real_eigensystem ${VNL_TESTS}  test_real_eigensystem )
ADD_TEST(test_complex_eigensystem ${VNL_TESTS} test_complex_eigensystem )
ADD_TEST(test_symmetric_eigensystem ${VNL_TESTS} test_symmetric_eigensystem )
ADD_TEST(test_generalized_eigensystem ${VNL_TESTS}   test_generalized_eigensystem)
ADD_TEST(test_matrix_exp ${VNL_TESTS} test_matrix_exp )

ADD_EXECUTABLE(vnlTests2 vnlTests2.cxx ${vnl_test2_SRCS})
TARGET_LINK_LIBRARIES(vnlTests2 VXLNumerics ITKIO ITKCommon)

ADD_TEST(test_matrix_fixed ${VNL_TESTS2} test_matrix_fixed )


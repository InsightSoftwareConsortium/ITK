SOURCE_FILES(vnl_test_SRCS
test_fft1d 
test_fft2d 
test_math 
test_vector 
test_matrix 
test_matrix_fixed
test_rnpoly_roots
test_sparse_matrix
test_sample 
test_functions
test_numeric_limits 
test_rpoly_roots
test_cpoly_roots 
test_resize 
test_complex 
test_determinant 
test_qr 
test_qsvd 
test_svd 
test_real_eigensystem 
test_complex_eigensystem 
test_symmetric_eigensystem 
test_generalized_eigensystem 
test_matrix_exp 
)

ADD_EXECUTABLE(vnlTests vnlTests.cxx vnl_test_SRCS)

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


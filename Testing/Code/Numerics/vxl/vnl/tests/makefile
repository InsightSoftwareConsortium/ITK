USES := vxl
USE_NETLIB := 1

TESTS += test_fft1d.cxx
TESTS += test_fft2d.cxx
TESTS += test_matlab.cxx
TESTS += test_math.cxx
TESTS += test_vector.cxx
TESTS += test_matrix.cxx
TESTS += test_matrix_fixed.cxx
TESTS += test_file_matrix.cxx
TESTS += test_rnpoly_roots.cxx
TESTS += test_sparse_matrix.cxx
TESTS += test_qsvd.cxx
TESTS += test_sample.cxx
TESTS += test_functions.cxx
TESTS += test_numeric_limits.cxx
TESTS += test_rpoly_roots.cxx
TESTS += test_cpoly_roots.cxx
TESTS += test_resize.cxx
TESTS += test_complex.cxx
TESTS += test_determinant.cxx
TESTS += test_qr.cxx
TESTS += test_svd.cxx
TESTS += test_real_eigensystem.cxx
TESTS += test_complex_eigensystem.cxx
TESTS += test_symmetric_eigensystem.cxx
TESTS += test_generalized_eigensystem.cxx
TESTS += test_matrix_exp.cxx
TESTS += test_transpose.cxx
TESTS += $(wildcard test_tensor.cxx)

IULIBS += -lvnl-algo -lvnl -lnetlib -lvpl # vpl_unlink used in test_matlab

#########################################
include ${IUEROOT}/config/static-tests.mk
#########################################

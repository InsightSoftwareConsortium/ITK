USES := vxl
LIBRARY := vnl-algo
VERSION := 1.0
IMMEDIATE_LIBS := -lvnl -lnetlib
DEFINES += -DBUILDING_VNL_ALGO_DLL

# matrix decompositions
MANPAGE_SOURCES += vnl_svd.txx
MANPAGE_SOURCES += vnl_matrix_inverse.txx
MANPAGE_SOURCES += vnl_qr.txx
MANPAGE_SOURCES += vnl_scatter_3x3.txx
SOURCES += vnl_cholesky.cxx
SOURCES += vnl_real_eigensystem.cxx
SOURCES += vnl_complex_eigensystem.cxx
SOURCES += vnl_symmetric_eigensystem.cxx
SOURCES += vnl_generalized_eigensystem.cxx
SOURCES += vnl_sparse_symmetric_eigensystem.cxx
SOURCES += $(wildcard vnl_jordan_normal_form.cxx)

# optimization
SOURCES += vnl_discrete_diff.cxx
SOURCES += vnl_levenberg_marquardt.cxx
SOURCES += vnl_conjugate_gradient.cxx
SOURCES += vnl_lbfgs.cxx
SOURCES += vnl_amoeba.cxx
SOURCES += vnl_powell.cxx
SOURCES += vnl_brent.cxx
SOURCES += vnl_lsqr.cxx

# equation solvers
SOURCES += vnl_rpoly_roots.cxx
SOURCES += vnl_cpoly_roots.cxx
SOURCES += vnl_rnpoly_solve.cxx

# fft
SOURCES += vnl_fftxd_prime_factors.cxx
MANPAGE_SOURCES += vnl_fft1d.txx
MANPAGE_SOURCES += vnl_fft2d.txx

# stuff
SOURCES += vnl_determinant.cxx
SOURCES += vnl_chi_squared.cxx
SOURCES += vnl_gaussian_kernel_1d.cxx
MANPAGE_SOURCES += vnl_adjugate.txx
MANPAGE_SOURCES += vnl_orthogonal_complement.txx

################################
include ${IUEROOT}/config/top.mk
################################

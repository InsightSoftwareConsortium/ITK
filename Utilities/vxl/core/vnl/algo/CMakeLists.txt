# vxl/vnl/algo/CMakeLists.txt

INCLUDE( ${MODULE_PATH}/FindNetlib.cmake )

# most of vnl_algo is simply a wrapper around netlib, so we must have netlib.
IF(NETLIB_FOUND)

# Direct netlib dependencies are:
# vnl_chi_squared                  dchscdf_
# vnl_cholesky                     dpoco_ dpodi_ dpofa_ dposl_
# vnl_complex_eigensystem          zgeev_
# vnl_conjugate_gradient           cg_
# vnl_fft                          dgpfa_ dsetgpfa_ gpfa_ setgpfa_
# vnl_generalized_eigensystem      rsg_
# vnl_generalized_schur            dgges_
# vnl_lbfgs                        lbfgs_ lb3_
# vnl_levenberg_marquardt          lmder1_ lmdif_
# vnl_lsqr                         lsqr_
# vnl_qr                           cqrdc_ cqrsl_ dqrdc_ dqrsl_ sqrdc_ sqrsl_ zqrdc_ zqrsl_
# vnl_real_eigensystem             rg_
# vnl_rpoly_roots                  rpoly_
# vnl_sparse_symmetric_eigensystem dnlaso_
# vnl_svd                          csvdc_ dsvdc_ ssvdc_ zsvdc_
# vnl_svd_economy                  csvdc_ dsvdc_ ssvdc_ zsvdc_
# vnl_symmetric_eigensystem        rs_

  INCLUDE_DIRECTORIES( ${NETLIB_INCLUDE_DIR} )

  SET( vnl_algo_sources
    vnl_algo_fwd.h
    vnl_netlib.h

    # matrix decompositions
    vnl_svd.txx vnl_svd.h
    vnl_svd_economy.txx vnl_svd_economy.h
    vnl_matrix_inverse.txx vnl_matrix_inverse.h
    vnl_qr.txx vnl_qr.h
    vnl_scatter_3x3.txx vnl_scatter_3x3.h
    vnl_cholesky.cxx vnl_cholesky.h
    vnl_real_eigensystem.cxx vnl_real_eigensystem.h
    vnl_complex_eigensystem.cxx vnl_complex_eigensystem.h
    vnl_symmetric_eigensystem.cxx vnl_symmetric_eigensystem.h
    vnl_generalized_eigensystem.cxx vnl_generalized_eigensystem.h
    vnl_sparse_symmetric_eigensystem.cxx vnl_sparse_symmetric_eigensystem.h
    vnl_generalized_schur.cxx vnl_generalized_schur.h

    # optimisation
    vnl_discrete_diff.cxx vnl_discrete_diff.h
    vnl_levenberg_marquardt.cxx vnl_levenberg_marquardt.h
    vnl_conjugate_gradient.cxx vnl_conjugate_gradient.h
    vnl_lbfgs.cxx vnl_lbfgs.h
    vnl_amoeba.cxx vnl_amoeba.h
    vnl_powell.cxx vnl_powell.h
    vnl_brent.cxx vnl_brent.h
    vnl_lsqr.cxx vnl_lsqr.h

    # equation solvers
    vnl_rpoly_roots.cxx vnl_rpoly_roots.h
    vnl_cpoly_roots.cxx vnl_cpoly_roots.h
    vnl_rnpoly_solve.cxx vnl_rnpoly_solve.h

    # fft
    vnl_fft.cxx vnl_fft.h
    vnl_fft_base.txx vnl_fft_base.h
    vnl_fft_1d.txx vnl_fft_1d.h
    vnl_fft_2d.txx vnl_fft_2d.h
    vnl_fft_prime_factors.txx vnl_fft_prime_factors.h

    # stuff
    vnl_convolve.txx vnl_convolve.h
    vnl_determinant.cxx vnl_determinant.txx vnl_determinant.h
    vnl_chi_squared.cxx vnl_chi_squared.h
    vnl_gaussian_kernel_1d.cxx vnl_gaussian_kernel_1d.h
    vnl_adjugate.txx vnl_adjugate.h
    vnl_orthogonal_complement.txx vnl_orthogonal_complement.h
  )

  AUX_SOURCE_DIRECTORY(Templates vnl_algo_sources)

  ADD_LIBRARY( vnl_algo ${vnl_algo_sources})
  TARGET_LINK_LIBRARIES( vnl_algo ${NETLIB_LIBRARIES} vnl )

ENDIF(NETLIB_FOUND)

# vxl/vnl/algo/CMakeLists.txt

project(vnl_algo)


include( ${VXL_CMAKE_DIR}/FindNetlib.cmake )

# most of vnl_algo is simply a wrapper around netlib, so we must have netlib.
if(NETLIB_FOUND)

# Direct netlib dependencies are:
# vnl_chi_squared                  dchscdf_
# vnl_cholesky                     dpoco_ dpodi_ dpofa_ dposl_
# vnl_ldl_cholesky                 dpoco_ dpodi_ dpofa_ dposl_
# vnl_complex_eigensystem          zgeev_
# vnl_complex_generalized_schur    zgges_
# vnl_conjugate_gradient           cg_
# vnl_fft                          dgpfa_ dsetgpfa_ gpfa_ setgpfa_
# vnl_generalized_eigensystem      rsg_
# vnl_generalized_schur            dgges_
# vnl_lbfgs                        lbfgs_ lb3_
# vnl_lbfgsb                       setulb_
# vnl_levenberg_marquardt          lmder1_ lmdif_
# vnl_lsqr                         lsqr_
# vnl_qr                           cqrdc_ cqrsl_ dqrdc_ dqrsl_ sqrdc_ sqrsl_ zqrdc_ zqrsl_
# vnl_real_eigensystem             rg_
# vnl_rpoly_roots                  rpoly_
# vnl_sparse_symmetric_eigensystem dnlaso_
# vnl_svd                          csvdc_ dsvdc_ ssvdc_ zsvdc_
# vnl_svd_economy                  csvdc_ dsvdc_ ssvdc_ zsvdc_
# vnl_svd_fixed                    csvdc_ dsvdc_ ssvdc_ zsvdc_
# vnl_symmetric_eigensystem        rs_

  include_directories(
    ${NETLIB_INCLUDE_DIR}
    ${NETLIB_INCLUDE_DIR}/linalg
    )

  set( vnl_algo_sources
    vnl_algo_fwd.h
    vnl_netlib.h

    # matrix decompositions
    vnl_svd.hxx vnl_svd.h
    vnl_svd_economy.hxx vnl_svd_economy.h
    vnl_svd_fixed.hxx vnl_svd_fixed.h
    vnl_matrix_inverse.hxx vnl_matrix_inverse.h
    vnl_qr.hxx vnl_qr.h
    vnl_scatter_3x3.hxx vnl_scatter_3x3.h
    vnl_cholesky.cxx vnl_cholesky.h
    vnl_ldl_cholesky.cxx vnl_ldl_cholesky.h
    vnl_sparse_lu.cxx vnl_sparse_lu.h
    vnl_real_eigensystem.cxx vnl_real_eigensystem.h
    vnl_complex_eigensystem.cxx vnl_complex_eigensystem.h
    vnl_symmetric_eigensystem.hxx vnl_symmetric_eigensystem.h
    vnl_generalized_eigensystem.cxx vnl_generalized_eigensystem.h
    vnl_sparse_symmetric_eigensystem.cxx vnl_sparse_symmetric_eigensystem.h
    vnl_generalized_schur.cxx vnl_generalized_schur.h
    vnl_complex_generalized_schur.cxx vnl_complex_generalized_schur.h

    # optimisation
    vnl_discrete_diff.cxx vnl_discrete_diff.h
    vnl_levenberg_marquardt.cxx vnl_levenberg_marquardt.h
    vnl_sparse_lm.cxx           vnl_sparse_lm.h
    vnl_conjugate_gradient.cxx vnl_conjugate_gradient.h
    vnl_lbfgs.cxx vnl_lbfgs.h
    vnl_lbfgsb.cxx vnl_lbfgsb.h
    vnl_amoeba.cxx vnl_amoeba.h
    vnl_powell.cxx vnl_powell.h
    vnl_brent.cxx vnl_brent.h
    vnl_lsqr.cxx vnl_lsqr.h
    vnl_solve_qp.cxx vnl_solve_qp.h
    vnl_bracket_minimum.cxx vnl_bracket_minimum.h
    vnl_brent_minimizer.cxx vnl_brent_minimizer.h

    # equation solvers
    vnl_rpoly_roots.cxx vnl_rpoly_roots.h
    vnl_cpoly_roots.cxx vnl_cpoly_roots.h
    vnl_rnpoly_solve.cxx vnl_rnpoly_solve.h
    vnl_fit_parabola.h

    # fft
    vnl_fft.cxx vnl_fft.h
    vnl_fft_base.hxx vnl_fft_base.h
    vnl_fft_1d.hxx vnl_fft_1d.h
    vnl_fft_2d.hxx vnl_fft_2d.h
    vnl_fft_prime_factors.hxx vnl_fft_prime_factors.h

    # stuff
    vnl_convolve.hxx vnl_convolve.h
    vnl_determinant.cxx vnl_determinant.hxx vnl_determinant.h
    vnl_chi_squared.hxx vnl_chi_squared.h
    vnl_gaussian_kernel_1d.cxx vnl_gaussian_kernel_1d.h
    vnl_adjugate.hxx vnl_adjugate.h
    vnl_orthogonal_complement.hxx vnl_orthogonal_complement.h
    vnl_matrix_update.h

    # integral
    vnl_simpson_integral.cxx vnl_simpson_integral.h
    vnl_adaptsimpson_integral.cxx vnl_adaptsimpson_integral.h
  )

  aux_source_directory(Templates vnl_algo_sources)

  # If VXL_INSTALL_INCLUDE_DIR is the default value
  if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
    set(_algo_install_dir ${VXL_INSTALL_INCLUDE_DIR}/core/vnl/algo)
  else()
    set(_algo_install_dir ${VXL_INSTALL_INCLUDE_DIR}/vnl/algo)
  endif()
  vxl_add_library(LIBRARY_NAME ${VXL_LIB_PREFIX}vnl_algo
    LIBRARY_SOURCES ${vnl_algo_sources}
    HEADER_INSTALL_DIR vnl/algo)
  target_link_libraries( ${VXL_LIB_PREFIX}vnl_algo ${NETLIB_LIBRARIES} ${VXL_LIB_PREFIX}vnl )
  set(CURR_LIB_NAME vnl_algo)
  set_vxl_library_properties(
     TARGET_NAME ${VXL_LIB_PREFIX}${CURR_LIB_NAME}
     BASE_NAME ${CURR_LIB_NAME}
     EXPORT_HEADER_FILE ${VXLCORE_BINARY_INCLUDE_DIR}/vnl/algo/${CURR_LIB_NAME}_export.h
     INSTALL_DIR   ${_algo_install_dir}
     USE_HIDDEN_VISIBILITY
  )

  if( BUILD_TESTING )
    add_subdirectory(tests)
  endif()
endif()

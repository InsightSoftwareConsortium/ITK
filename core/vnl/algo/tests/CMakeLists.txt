# This is core/vnl/algo/tests/CMakeLists.txt

include( ${VXL_CMAKE_DIR}/FindNetlib.cmake )

# message( NETLIB_FOUND " is " ${NETLIB_FOUND} )
if(NETLIB_FOUND)

  include_directories( ${NETLIB_INCLUDE_DIR} )

  add_executable( vnl_algo_test_all
    # Driver source and utilities
    test_driver.cxx
    test_util.cxx       test_util.h

    # The tests
    test_algo.cxx
    test_amoeba.cxx
    test_cholesky.cxx
    test_complex_algo.cxx
    test_complex_eigensystem.cxx
    test_convolve.cxx
    test_cpoly_roots.cxx
    test_determinant.cxx
    test_fft.cxx
    test_fft1d.cxx
    test_fft2d.cxx
    test_functions.cxx
    test_generalized_eigensystem.cxx
    test_ldl_cholesky.cxx
    test_levenberg_marquardt.cxx
    test_matrix_update.cxx
    test_minimizers.cxx
    test_powell.cxx
    test_qr.cxx
    test_qsvd.cxx
    test_rank.cxx
    test_real_eigensystem.cxx
    test_rnpoly_roots.cxx
    test_rpoly_roots.cxx
    test_sparse_matrix.cxx
    test_svd.cxx
    test_svd_fixed.cxx
    test_symmetric_eigensystem.cxx
    test_integral.cxx
    test_solve_qp.cxx
    test_sparse_lu.cxx
    test_bracket_minimum.cxx
    test_brent_minimizer.cxx
    test_sparse_lm.cxx
  )

  target_link_libraries( vnl_algo_test_all ${VXL_LIB_PREFIX}vnl_algo ${VXL_LIB_PREFIX}testlib ${CMAKE_THREAD_LIBS} )

  add_test( NAME vnl_algo_test_algo COMMAND vnl_algo_test_all test_algo                    )
  add_test( NAME vnl_algo_test_amoeba COMMAND vnl_algo_test_all test_amoeba                  )
  add_test( NAME vnl_algo_test_cholesky COMMAND vnl_algo_test_all test_cholesky                )
  add_test( NAME vnl_algo_test_complex_algo COMMAND vnl_algo_test_all test_complex_algo            )
  add_test( NAME vnl_algo_test_complex_eigensystem COMMAND vnl_algo_test_all test_complex_eigensystem     )
  add_test( NAME vnl_algo_test_convolve COMMAND vnl_algo_test_all test_convolve                )
  add_test( NAME vnl_algo_test_cpoly_roots COMMAND vnl_algo_test_all test_cpoly_roots             )
  add_test( NAME vnl_algo_test_determinant COMMAND vnl_algo_test_all test_determinant             )
  add_test( NAME vnl_algo_test_fft COMMAND vnl_algo_test_all test_fft                     )
  add_test( NAME vnl_algo_test_fft1d COMMAND vnl_algo_test_all test_fft1d                   )
  add_test( NAME vnl_algo_test_fft2d COMMAND vnl_algo_test_all test_fft2d                   )
  add_test( NAME vnl_algo_test_functions COMMAND vnl_algo_test_all test_functions               )
  add_test( NAME vnl_algo_test_generalized_eigensystem COMMAND vnl_algo_test_all test_generalized_eigensystem )
  add_test( NAME vnl_algo_test_ldl_cholesky COMMAND vnl_algo_test_all test_ldl_cholesky            )
  add_test( NAME vnl_algo_test_levenberg_marquardt COMMAND vnl_algo_test_all test_levenberg_marquardt     )
  add_test( NAME vnl_algo_test_matrix_update COMMAND vnl_algo_test_all test_matrix_update           )
  add_test( NAME vnl_algo_test_minimizers COMMAND vnl_algo_test_all test_minimizers              )
  add_test( NAME vnl_algo_test_powell COMMAND vnl_algo_test_all test_powell                  )
  add_test( NAME vnl_algo_test_qr COMMAND vnl_algo_test_all test_qr                      )
  add_test( NAME vnl_algo_test_qsvd COMMAND vnl_algo_test_all test_qsvd                    )
  add_test( NAME vnl_algo_test_rank COMMAND vnl_algo_test_all test_rank                    )
  add_test( NAME vnl_algo_test_real_eigensystem COMMAND vnl_algo_test_all test_real_eigensystem        )
  add_test( NAME vnl_algo_test_rnpoly_roots COMMAND vnl_algo_test_all test_rnpoly_roots            )
  add_test( NAME vnl_algo_test_rpoly_roots COMMAND vnl_algo_test_all test_rpoly_roots             )
  add_test( NAME vnl_algo_test_integral COMMAND vnl_algo_test_all test_integral                )
  add_test( NAME vnl_algo_test_solve_qp COMMAND vnl_algo_test_all test_solve_qp                )
  add_test( NAME vnl_algo_test_sparse_lu COMMAND vnl_algo_test_all test_sparse_lu               )
  add_test( NAME vnl_algo_test_bracket_minimum COMMAND vnl_algo_test_all test_bracket_minimum         )
  add_test( NAME vnl_algo_test_brent_minimizer COMMAND vnl_algo_test_all test_brent_minimizer         )
  add_test( NAME vnl_algo_test_sparse_lm COMMAND vnl_algo_test_all test_sparse_lm               )
  add_test( NAME vnl_algo_test_sparse_matrix COMMAND vnl_algo_test_all test_sparse_matrix           )
  add_test( NAME vnl_algo_test_svd COMMAND vnl_algo_test_all test_svd                     )
  add_test( NAME vnl_algo_test_svd_fixed COMMAND vnl_algo_test_all test_svd_fixed               )
  add_test( NAME vnl_algo_test_symmetric_eigensystem COMMAND vnl_algo_test_all test_symmetric_eigensystem   )
endif()

# GCC 2.95 has problems when compiling test_algo.cxx with "-O2" flag.
# The solution is to turn off optimization for GCC < 3.0
if(CMAKE_COMPILER_IS_GNUCXX)
  if( VNL_COMPILER_IS_GNUCXX_2XX MATCHES "VNL_COMPILER_IS_GNUCXX_2XX")
    exec_program(${CMAKE_CXX_COMPILER} ARGS --version OUTPUT_VARIABLE CMAKE_CXX_COMPILER_VERSION)
    if(CMAKE_CXX_COMPILER_VERSION MATCHES ".*2\\.9[0-9]\\.[0-9].*")
      set( VNL_COMPILER_IS_GNUCXX_2XX 1 CACHE INTERNAL "Are we using an version of gcc < 3.0")
    else()
      set( VNL_COMPILER_IS_GNUCXX_2XX 0 CACHE INTERNAL "Are we using an version of gcc < 3.0")
    endif()
  endif()

  if(VNL_COMPILER_IS_GNUCXX_2XX)
    # We only need to remove "-O2" flag from test_algo.cxx.
    # But it is much easier to do it for all
    foreach(var CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_RELEASE CMAKE_CXX_FLAGS_DEBUG
                CMAKE_CXX_FLAGS_RELWITHDEBINFO CMAKE_CXX_FLAGS_MINSIZEREL)
      string(REGEX REPLACE "-O2" "" "${var}" "${${var}}")
    endforeach()
  endif()
endif()

add_executable( vnl_algo_test_include test_include.cxx )
target_link_libraries( vnl_algo_test_include ${VXL_LIB_PREFIX}vnl_algo )
add_executable( vnl_algo_test_template_include test_template_include.cxx )
target_link_libraries( vnl_algo_test_template_include ${VXL_LIB_PREFIX}vnl_algo )

# This is core/vnl/algo/tests/CMakeLists.txt

INCLUDE( ${MODULE_PATH}/FindNetlib.cmake )

# MESSAGE( NETLIB_FOUND " is " ${NETLIB_FOUND} )
IF(NETLIB_FOUND)

  INCLUDE_DIRECTORIES( ${NETLIB_INCLUDE_DIR} )

  ADD_EXECUTABLE( vnl_algo_test_all
    # Driver source and utilities
    test_driver.cxx
    test_util.cxx       test_util.h

    # The tests
    test_algo.cxx
    test_amoeba.cxx
    test_cholesky.cxx
    test_complex_algo.cxx
    test_complex_eigensystem.cxx
    #test_convolve.cxx # Removing for ITK: needs vul
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
    test_sparse_matrix.cxx
    test_svd.cxx
    #test_svd_fixed.cxx # Removing for ITK: needs vul
    #test_symmetric_eigensystem.cxx # Removing for ITK: needs vul
    test_integral.cxx
    test_solve_qp.cxx
    test_sparse_lu.cxx
    test_bracket_minimum.cxx
    test_brent_minimizer.cxx
    test_sparse_lm.cxx
  )


  TARGET_LINK_LIBRARIES( vnl_algo_test_all itkvnl_algo itktestlib ${CMAKE_THREAD_LIBS} )


  itk_add_test( vnl_algo_test_algo                     ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_algo                    )
  itk_add_test( vnl_algo_test_amoeba                   ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_amoeba                  )
  itk_add_test( vnl_algo_test_cholesky                 ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_cholesky                )
  itk_add_test( vnl_algo_test_complex_algo             ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_complex_algo            )
  itk_add_test( vnl_algo_test_complex_eigensystem      ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_complex_eigensystem     )
  #itk_add_test( vnl_algo_test_convolve                 ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_convolve                )
  itk_add_test( vnl_algo_test_cpoly_roots              ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_cpoly_roots             )
  itk_add_test( vnl_algo_test_determinant              ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_determinant             )
  itk_add_test( vnl_algo_test_fft                      ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_fft                     )
  itk_add_test( vnl_algo_test_fft1d                    ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_fft1d                   )
  itk_add_test( vnl_algo_test_fft2d                    ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_fft2d                   )
  itk_add_test( vnl_algo_test_functions                ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_functions               )
  itk_add_test( vnl_algo_test_generalized_eigensystem  ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_generalized_eigensystem )
  itk_add_test( vnl_algo_test_ldl_cholesky             ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_ldl_cholesky            )
  itk_add_test( vnl_algo_test_levenberg_marquardt      ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_levenberg_marquardt     )
  itk_add_test( vnl_algo_test_matrix_update            ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_matrix_update           )
  itk_add_test( vnl_algo_test_minimizers               ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_minimizers              )
  itk_add_test( vnl_algo_test_powell                   ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_powell                  )
  itk_add_test( vnl_algo_test_qr                       ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_qr                      )
  itk_add_test( vnl_algo_test_qsvd                     ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_qsvd                    )
  itk_add_test( vnl_algo_test_rank                     ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_rank                    )
  itk_add_test( vnl_algo_test_real_eigensystem         ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_real_eigensystem        )
  itk_add_test( vnl_algo_test_rnpoly_roots             ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_rnpoly_roots            )
  itk_add_test( vnl_algo_test_integral                 ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_integral                )
  itk_add_test( vnl_algo_test_solve_qp                 ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_solve_qp                )
  itk_add_test( vnl_algo_test_sparse_lu                ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_sparse_lu               )
  itk_add_test( vnl_algo_test_bracket_minimum          ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_bracket_minimum         )
  itk_add_test( vnl_algo_test_brent_minimizer          ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_brent_minimizer         )
  itk_add_test( vnl_algo_test_sparse_lm                ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_sparse_lm               )
  itk_add_test( vnl_algo_test_sparse_matrix            ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_sparse_matrix           )
  itk_add_test( vnl_algo_test_svd                      ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_svd                     )
  #itk_add_test( vnl_algo_test_svd_fixed                ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_svd_fixed               )
  #itk_add_test( vnl_algo_test_symmetric_eigensystem    ${EXECUTABLE_OUTPUT_PATH}/vnl_algo_test_all test_symmetric_eigensystem   )
ENDIF(NETLIB_FOUND)

# GCC 2.95 has problems when compiling test_algo.cxx with "-O2" flag.
# The solution is to turn off optimization for GCC < 3.0
IF(CMAKE_COMPILER_IS_GNUCXX)
  IF ( VNL_COMPILER_IS_GNUCXX_2XX MATCHES "VNL_COMPILER_IS_GNUCXX_2XX")
    EXEC_PROGRAM(${CMAKE_CXX_COMPILER} ARGS --version OUTPUT_VARIABLE CMAKE_CXX_COMPILER_VERSION)
    IF(CMAKE_CXX_COMPILER_VERSION MATCHES ".*2\\.9[0-9]\\.[0-9].*")
      SET ( VNL_COMPILER_IS_GNUCXX_2XX 1 CACHE INTERNAL "Are we using an version of gcc < 3.0")
    ELSE(CMAKE_CXX_COMPILER_VERSION MATCHES ".*2\\.9[0-9]\\.[0-9].*")
      SET ( VNL_COMPILER_IS_GNUCXX_2XX 0 CACHE INTERNAL "Are we using an version of gcc < 3.0")
    ENDIF(CMAKE_CXX_COMPILER_VERSION MATCHES ".*2\\.9[0-9]\\.[0-9].*")
  ENDIF ( VNL_COMPILER_IS_GNUCXX_2XX MATCHES "VNL_COMPILER_IS_GNUCXX_2XX")

  IF(VNL_COMPILER_IS_GNUCXX_2XX)
    # We only need to remove "-O2" flag from test_algo.cxx.
    # But it is much easier to do it for all
    FOREACH(var CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_RELEASE CMAKE_CXX_FLAGS_DEBUG
                CMAKE_CXX_FLAGS_RELWITHDEBINFO CMAKE_CXX_FLAGS_MINSIZEREL)
      STRING(REGEX REPLACE "-O2" "" "${var}" "${${var}}")
    ENDFOREACH(var)
  ENDIF(VNL_COMPILER_IS_GNUCXX_2XX)
ENDIF ( CMAKE_COMPILER_IS_GNUCXX )

ADD_EXECUTABLE( vnl_algo_test_include test_include.cxx )
TARGET_LINK_LIBRARIES( vnl_algo_test_include itkvnl_algo )
ADD_EXECUTABLE( vnl_algo_test_template_include test_template_include.cxx )
TARGET_LINK_LIBRARIES( vnl_algo_test_template_include itkvnl_algo )

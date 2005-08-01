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
    test_complex_eigensystem.cxx
    #test_convolve.cxx # Removing for ITK: needs vul
    test_cpoly_roots.cxx
    test_determinant.cxx
    test_fft.cxx
    test_fft1d.cxx
    test_fft2d.cxx
    test_functions.cxx
    test_generalized_eigensystem.cxx
    test_levenberg_marquardt.cxx
    test_minimizers.cxx
    test_qr.cxx
    test_qsvd.cxx
    test_rank.cxx
    test_real_eigensystem.cxx
    test_rnpoly_roots.cxx
    test_rpoly_roots.cxx
    test_sparse_matrix.cxx
    test_svd.cxx
    #test_symmetric_eigensystem.cxx # Removing for ITK: needs vul
    test_integral.cxx
  )


  TARGET_LINK_LIBRARIES( vnl_algo_test_all itkvnl_algo itktestlib ${CMAKE_THREAD_LIBS} )


  ADD_TEST( vnl_test_algo                     vnl_algo_test_all test_algo                    )
  ADD_TEST( vnl_test_amoeba                   vnl_algo_test_all test_amoeba                  )
  ADD_TEST( vnl_test_cholesky                 vnl_algo_test_all test_cholesky                )
  ADD_TEST( vnl_test_complex_eigensystem      vnl_algo_test_all test_complex_eigensystem     )
  #ADD_TEST( vnl_test_convolve                 vnl_algo_test_all test_convolve                )
  ADD_TEST( vnl_test_cpoly_roots              vnl_algo_test_all test_cpoly_roots             )
  ADD_TEST( vnl_test_determinant              vnl_algo_test_all test_determinant             )
  ADD_TEST( vnl_test_fft                      vnl_algo_test_all test_fft                     )
  ADD_TEST( vnl_test_fft1d                    vnl_algo_test_all test_fft1d                   )
  ADD_TEST( vnl_test_fft2d                    vnl_algo_test_all test_fft2d                   )
  ADD_TEST( vnl_test_functions                vnl_algo_test_all test_functions               )
  ADD_TEST( vnl_test_generalized_eigensystem  vnl_algo_test_all test_generalized_eigensystem )
  ADD_TEST( vnl_test_levenberg_marquardt      vnl_algo_test_all test_levenberg_marquardt     )
  ADD_TEST( vnl_test_minimizers               vnl_algo_test_all test_minimizers              )
  ADD_TEST( vnl_test_qr                       vnl_algo_test_all test_qr                      )
  ADD_TEST( vnl_test_qsvd                     vnl_algo_test_all test_qsvd                    )
  ADD_TEST( vnl_test_rank                     vnl_algo_test_all test_rank                    )
  ADD_TEST( vnl_test_real_eigensystem         vnl_algo_test_all test_real_eigensystem        )
  ADD_TEST( vnl_test_rnpoly_roots             vnl_algo_test_all test_rnpoly_roots            )
  ADD_TEST( vnl_test_rpoly_roots              vnl_algo_test_all test_rpoly_roots             )
  ADD_TEST( vnl_test_integral                 vnl_algo_test_all test_integral                )

  IF ( SITE MATCHES "isbe.man.ac.uk" )
    # For some reason my box (linux-gcc-3.2) has a problem with the optimisation
    # settings for dnlaso.c in netlib. It cannot be fixed until > CMAKE 1.8.3
  ELSE ( SITE MATCHES "isbe.man.ac.uk" )
    ADD_TEST( vnl_test_sparse_matrix            vnl_algo_test_all test_sparse_matrix           )
  ENDIF ( SITE MATCHES "isbe.man.ac.uk" )
  ADD_TEST( vnl_test_svd                      vnl_algo_test_all test_svd                     )
  #ADD_TEST( vnl_test_symmetric_eigensystem    vnl_algo_test_all test_symmetric_eigensystem   )
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

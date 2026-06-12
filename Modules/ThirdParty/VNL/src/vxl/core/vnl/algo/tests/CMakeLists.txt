# This is core/vnl/algo/tests/CMakeLists.txt

add_executable( vnl_algo_test_all
  # Driver source and utilities
  test_driver.cxx
  test_util.cxx       test_util.h

  # The tests
  test_algo.cxx
  test_amoeba.cxx
  test_cholesky.cxx
  test_complex_algo.cxx
  test_determinant.cxx
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
  test_sparse_matrix.cxx
  test_svd.cxx
  test_svd_fixed.cxx
  test_symmetric_eigensystem.cxx
  test_integral.cxx
  test_solve_qp.cxx
  test_sparse_lu.cxx
  test_bracket_minimum.cxx
  test_brent_minimizer.cxx
)

target_link_libraries( vnl_algo_test_all itkvnl_algo itktestlib ${CMAKE_THREAD_LIBS} )

add_test( NAME vnl_algo_test_algo COMMAND vnl_algo_test_all test_algo                    )
add_test( NAME vnl_algo_test_amoeba COMMAND vnl_algo_test_all test_amoeba                  )
add_test( NAME vnl_algo_test_cholesky COMMAND vnl_algo_test_all test_cholesky                )
add_test( NAME vnl_algo_test_complex_algo COMMAND vnl_algo_test_all test_complex_algo            )
add_test( NAME vnl_algo_test_determinant COMMAND vnl_algo_test_all test_determinant             )
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
add_test( NAME vnl_algo_test_integral COMMAND vnl_algo_test_all test_integral                )
add_test( NAME vnl_algo_test_solve_qp COMMAND vnl_algo_test_all test_solve_qp                )
add_test( NAME vnl_algo_test_sparse_lu COMMAND vnl_algo_test_all test_sparse_lu               )
add_test( NAME vnl_algo_test_bracket_minimum COMMAND vnl_algo_test_all test_bracket_minimum         )
add_test( NAME vnl_algo_test_brent_minimizer COMMAND vnl_algo_test_all test_brent_minimizer         )
add_test( NAME vnl_algo_test_symmetric_eigensystem COMMAND vnl_algo_test_all test_symmetric_eigensystem   )
add_test( NAME vnl_algo_test_sparse_matrix COMMAND vnl_algo_test_all test_sparse_matrix           )
add_test( NAME vnl_algo_test_svd COMMAND vnl_algo_test_all test_svd                     )
add_test( NAME vnl_algo_test_svd_fixed COMMAND vnl_algo_test_all test_svd_fixed               )

add_executable( vnl_algo_test_include test_include.cxx )
target_link_libraries( vnl_algo_test_include itkvnl_algo )
add_executable( vnl_algo_test_template_include test_template_include.cxx )
target_link_libraries( vnl_algo_test_template_include itkvnl_algo )

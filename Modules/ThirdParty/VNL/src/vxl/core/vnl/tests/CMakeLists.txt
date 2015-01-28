# This is core/vnl/tests/CMakeLists.txt

ADD_EXECUTABLE( vnl_test_all
  # Driver source and utilities
  test_driver.cxx

  # The tests
  test_bignum.cxx
  test_complex.cxx
  test_inverse.cxx
  test_diag_matrix.cxx
  test_diag_matrix_fixed.cxx
  test_file_matrix.cxx
  test_finite.cxx
  test_math.cxx
  test_na.cxx
  #test_matlab.cxx # Removing for ITK: needs vul and vpl
  test_matrix.cxx
  test_matrix_exp.cxx
  test_matrix_fixed.cxx
  test_vector_fixed_ref.cxx
  test_matrix_fixed_ref.cxx
  test_numeric_traits.cxx
  test_power.cxx
  test_quaternion.cxx
  test_rational.cxx
  test_real_polynomial.cxx
  test_real_npolynomial.cxx
  test_resize.cxx
  test_rotation_matrix.cxx
  test_sample.cxx
  test_sym_matrix.cxx
  test_transpose.cxx
  test_fastops.cxx
  test_vector.cxx
  test_gamma.cxx
  test_random.cxx
  test_alignment.cxx
  test_arithmetic.cxx  test_arithmetic_body.h
  test_hungarian_algorithm.cxx
  test_integrant.cxx
  test_bessel.cxx
  test_crs_index.cxx
  test_sparse_lst_sqr_function.cxx
  test_sparse_matrix.cxx
  test_pow_log.cxx
)

IF(CMAKE_COMPILER_IS_GNUCXX)
 SET_SOURCE_FILES_PROPERTIES(test_rational.cxx PROPERTIES COMPILE_FLAGS -O1)
 SET_SOURCE_FILES_PROPERTIES(test_bignum.cxx PROPERTIES COMPILE_FLAGS -O3)
 SET_SOURCE_FILES_PROPERTIES(test_finite.cxx PROPERTIES COMPILE_FLAGS -O0)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

TARGET_LINK_LIBRARIES(vnl_test_all itkvnl itktestlib ${CMAKE_THREAD_LIBS})

ADD_EXECUTABLE(vnl_basic_operation_timings basic_operation_timings.cxx)
TARGET_LINK_LIBRARIES(vnl_basic_operation_timings itkvnl)

itk_add_test( NAME vnl_test_bignum                 COMMAND vnl_test_all test_bignum                 )
itk_add_test( NAME vnl_test_complex                COMMAND vnl_test_all test_complex                )
itk_add_test( NAME vnl_test_diag_matrix            COMMAND vnl_test_all test_diag_matrix            )
itk_add_test( NAME vnl_test_diag_matrix_fixed      COMMAND vnl_test_all test_diag_matrix_fixed      )
itk_add_test( NAME vnl_test_file_matrix            COMMAND vnl_test_all test_file_matrix            )
itk_add_test( NAME vnl_test_finite                 COMMAND vnl_test_all test_finite                 )
itk_add_test( NAME vnl_test_inverse                COMMAND vnl_test_all test_inverse                )
itk_add_test( NAME vnl_test_math                   COMMAND vnl_test_all test_math                   )
#itk_add_test( NAME vnl_test_matlab                 COMMAND vnl_test_all test_matlab                 )
itk_add_test( NAME vnl_test_matrix                 COMMAND vnl_test_all test_matrix                 )
itk_add_test( NAME vnl_test_matrix_exp             COMMAND vnl_test_all test_matrix_exp             )
itk_add_test( NAME vnl_test_matrix_fixed           COMMAND vnl_test_all test_matrix_fixed           )
itk_add_test( NAME vnl_test_vector_fixed_ref       COMMAND vnl_test_all test_vector_fixed_ref       )
itk_add_test( NAME vnl_test_matrix_fixed_ref       COMMAND vnl_test_all test_matrix_fixed_ref       )
itk_add_test( NAME vnl_test_numeric_traits         COMMAND vnl_test_all test_numeric_traits         )
itk_add_test( NAME vnl_test_na                     COMMAND vnl_test_all test_na                     )
itk_add_test( NAME vnl_test_random                 COMMAND vnl_test_all test_random                 )
itk_add_test( NAME vnl_test_rational               COMMAND vnl_test_all test_rational               )
itk_add_test( NAME vnl_test_real_polynomial        COMMAND vnl_test_all test_real_polynomial        )
itk_add_test( NAME vnl_test_real_npolynomial       COMMAND vnl_test_all test_real_npolynomial       )
itk_add_test( NAME vnl_test_resize                 COMMAND vnl_test_all test_resize                 )
itk_add_test( NAME vnl_test_rotation_matrix        COMMAND vnl_test_all test_rotation_matrix        )
itk_add_test( NAME vnl_test_sample                 COMMAND vnl_test_all test_sample                 )
itk_add_test( NAME vnl_test_sym_matrix             COMMAND vnl_test_all test_sym_matrix             )
itk_add_test( NAME vnl_test_transpose              COMMAND vnl_test_all test_transpose              )
itk_add_test( NAME vnl_test_fastops                COMMAND vnl_test_all test_fastops                )
itk_add_test( NAME vnl_test_vector                 COMMAND vnl_test_all test_vector                 )
itk_add_test( NAME vnl_test_gamma                  COMMAND vnl_test_all test_gamma                  )
itk_add_test( NAME vnl_test_arithmetic             COMMAND vnl_test_all test_arithmetic             )
itk_add_test( NAME vnl_test_alignment              COMMAND vnl_test_all test_alignment              )
itk_add_test( NAME vnl_test_hungarian_algorithm    COMMAND vnl_test_all test_hungarian_algorithm    )
itk_add_test( NAME vnl_test_integrant              COMMAND vnl_test_all test_integrant              )
itk_add_test( NAME vnl_test_bessel                 COMMAND vnl_test_all test_bessel                 )
itk_add_test( NAME vnl_test_quaternion             COMMAND vnl_test_all test_quaternion             )
itk_add_test( NAME vnl_test_crs_index              COMMAND vnl_test_all test_crs_index              )
itk_add_test( NAME vnl_test_sparse_lst_sqr_function COMMAND vnl_test_all test_sparse_lst_sqr_function)
itk_add_test( NAME vnl_test_power                  COMMAND vnl_test_all test_power                  )
itk_add_test( NAME vnl_test_sparse_matrix          COMMAND vnl_test_all test_sparse_matrix          )
itk_add_test( NAME test_pow_log                    COMMAND vnl_test_all test_pow_log                )

ADD_EXECUTABLE(vnl_test_include test_include.cxx)
TARGET_LINK_LIBRARIES(vnl_test_include itkvnl)
ADD_EXECUTABLE(vnl_test_template_include test_template_include.cxx)
TARGET_LINK_LIBRARIES(vnl_test_template_include itkvnl)

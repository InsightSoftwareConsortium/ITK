# vxl/vnl/examples/CMakeLists.txt

link_libraries( ${VXL_LIB_PREFIX}vnl_algo ${VXL_LIB_PREFIX}vnl ${VXL_LIB_PREFIX}vul )

add_executable(vnl_amoeba                vnl_amoeba.cxx)
add_executable(vnl_complex_svd           vnl_complex_svd.cxx)
add_executable(vnl_planefit              vnl_planefit.cxx)
add_executable(vnl_polyroots             vnl_polyroots.cxx)
add_executable(vnl_polynomial_factoring  vnl_polynomial_factoring.cxx)
add_executable(vnl_polynomial_RPN        vnl_polynomial_RPN.cxx)
add_executable(vnl_rosenbrock            vnl_rosenbrock.cxx)
add_executable(vnl_svd                   vnl_svd.cxx)
add_executable(vnl_svd_economy           vnl_svd_economy.cxx)
add_executable(vnl_lsqr_test             vnl_lsqr_test.cxx)
add_executable(vnl_sample_beta           vnl_sample_beta.cxx)
add_executable(vnl_sparse_matrix_example vnl_sparse_matrix_example.cxx)
add_executable(vnl_complex_squareroot    vnl_complex_squareroot.cxx)

add_executable(time_fastops              time_fastops.cxx)
add_executable(calculate                 calculate.cxx)

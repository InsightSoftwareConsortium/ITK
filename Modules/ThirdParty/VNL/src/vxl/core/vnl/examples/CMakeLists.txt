# vxl/vnl/examples/CMakeLists.txt
SET(EXECUTABLE_OUTPUT_PATH ${vxl_BINARY_DIR}/core/vnl/examples)

#LINK_LIBRARIES( vnl_algo vnl vul )
LINK_LIBRARIES( itkvnl_algo itkvnl )

#ADD_EXECUTABLE(time_fastops    time_fastops.cxx)
ADD_EXECUTABLE(vnl_amoeba      vnl_amoeba.cxx)
ADD_EXECUTABLE(vnl_complex_svd vnl_complex_svd.cxx)
ADD_EXECUTABLE(vnl_planefit    vnl_planefit.cxx)
ADD_EXECUTABLE(vnl_polyroots   vnl_polyroots.cxx)
ADD_EXECUTABLE(vnl_rosenbrock  vnl_rosenbrock.cxx)
ADD_EXECUTABLE(vnl_svd         vnl_svd.cxx)
#ADD_EXECUTABLE(vnl_svd_economy vnl_svd_economy.cxx)
ADD_EXECUTABLE(vnl_lsqr_test   vnl_lsqr_test.cxx)
ADD_EXECUTABLE(vnl_sample_beta vnl_sample_beta.cxx)
ADD_EXECUTABLE(vnl_sparse_matrix_example vnl_sparse_matrix_example.cxx)
ADD_EXECUTABLE(vnl_complex_squareroot    vnl_complex_squareroot.cxx)

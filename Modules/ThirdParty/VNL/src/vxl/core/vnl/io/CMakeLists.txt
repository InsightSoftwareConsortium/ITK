# vxl/vnl/io/CMakeLists.txt

SET(vnl_io_sources
  vnl_io_nonlinear_minimizer.cxx  vnl_io_nonlinear_minimizer.h
  vnl_io_real_npolynomial.cxx     vnl_io_real_npolynomial.h
  vnl_io_real_polynomial.cxx      vnl_io_real_polynomial.h
  vnl_io_rational.cxx             vnl_io_rational.h
  vnl_io_bignum.cxx               vnl_io_bignum.h

  vnl_io_diag_matrix.txx          vnl_io_diag_matrix.h
  vnl_io_matrix.txx               vnl_io_matrix.h
  vnl_io_sparse_matrix.txx        vnl_io_sparse_matrix.h
  vnl_io_sym_matrix.txx           vnl_io_sym_matrix.h
  vnl_io_vector.txx               vnl_io_vector.h
  vnl_io_vector_fixed.txx         vnl_io_vector_fixed.h

  vnl_io_matrix_fixed.txx         vnl_io_matrix_fixed.h
)

AUX_SOURCE_DIRECTORY(Templates vnl_io_sources)

IF(CMAKE_COMPILER_IS_GNUCXX)
  SET_SOURCE_FILES_PROPERTIES(Templates/vsl_vector_io+vcl_vector+vcl_vector+vnl_vector+double----.cxx
                              PROPERTIES
                              COMPILE_FLAGS -ftemplate-depth-35)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

ADD_LIBRARY(vnl_io ${vnl_io_sources} )
TARGET_LINK_LIBRARIES( vnl_io vnl vsl )

INSTALL_TARGETS(/lib vnl_io)
INSTALL_NOBASE_HEADER_FILES(/include/vxl/core/vnl/io ${vnl_io_sources})

IF(BUILD_TESTING)
  SUBDIRS(tests)
ENDIF(BUILD_TESTING)

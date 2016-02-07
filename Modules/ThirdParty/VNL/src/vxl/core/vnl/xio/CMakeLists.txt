# vxl/vnl/xio/CMakeLists.txt

set(vnl_xio_sources
  vnl_xio_matrix_fixed.hxx   vnl_xio_matrix_fixed.h
  vnl_xio_matrix.hxx         vnl_xio_matrix.h
  vnl_xio_vector_fixed.hxx   vnl_xio_vector_fixed.h
  vnl_xio_vector.hxx         vnl_xio_vector.h
  vnl_xio_quaternion.hxx     vnl_xio_quaternion.h
)

aux_source_directory(Templates vnl_xio_sources)

vxl_add_library(LIBNAME ${VXL_LIB_PREFIX}vnl_xio LIBSRCS ${vnl_xio_sources} )
target_link_libraries( ${VXL_LIB_PREFIX}vnl_xio ${VXL_LIB_PREFIX}vnl ${VXL_LIB_PREFIX}vsl )

if(BUILD_TESTING)
  add_subdirectory(tests)
endif()

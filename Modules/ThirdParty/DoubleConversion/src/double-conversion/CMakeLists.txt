if("${DOUBLE_CONVERSION_CORRECT_DOUBLE_OPERATIONS}" MATCHES "^${DOUBLE_CONVERSION_CORRECT_DOUBLE_OPERATION}$")
  set(_test_source "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/double-correction-needed.cc")
  file(WRITE ${_test_source} "
  #include <stdio.h>
  int main(int, char **)
  {
    const double correct = 89255.0/1e22;
    printf(\"89255.0/1e22 = %g\", correct);
    if( correct != 89255e-22 )
      {
      // correction required
      return 0;
      }
    return 1;
  }")
  try_run(DOUBLE_CONVERSION_CORRECT_DOUBLE_OPERATIONS DOUBLE_CORRECT_NEEDED_COMPILED
    ${CMAKE_CURRENT_BINARY_DIR} ${_test_source}
    RUN_OUTPUT_VARIABLE _double_correction_out)
  message(STATUS "Double correction test returned: ${_double_correction_out}")
endif()
configure_file(double-conversion-configure.h.in double-conversion-configure.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

set(headers
  bignum.h
  cached-powers.h
  diy-fp.h
  double-conversion.h
  fast-dtoa.h
  fixed-dtoa.h
  ieee.h
  strtod.h
  utils.h
  ${CMAKE_CURRENT_BINARY_DIR}/double-conversion-configure.h
  )

add_library(itkdouble-conversion
bignum.cc
bignum-dtoa.cc
cached-powers.cc
diy-fp.cc
double-conversion.cc
fast-dtoa.cc
fixed-dtoa.cc
strtod.cc
${headers}
)

if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(itkdouble-conversion PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif()

itk_module_target(itkdouble-conversion NO_INSTALL)

#
# install command to set up library install
# given the above PUBLIC_HEADER property set, this
# pulls along all the header files with the library.
install(TARGETS itkdouble-conversion
  EXPORT ${ITKDoubleConversion-targets}
  RUNTIME DESTINATION "${ITKDoubleConversion_INSTALL_RUNTIME_DIR}" COMPONENT Applications
  LIBRARY DESTINATION "${ITKDoubleConversion_INSTALL_LIBRARY_DIR}" COMPONENT Libraries
  ARCHIVE DESTINATION "${ITKDoubleConversion_INSTALL_LIBRARY_DIR}" COMPONENT Development)

install(FILES ${headers}
  DESTINATION "${ITKDoubleConversion_INSTALL_INCLUDE_DIR}" COMPONENT Development)

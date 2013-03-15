
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
  DoubleToString.h
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
# associates the list of headers with the library
# for the purposes of installation/import into other projects
#
# install command to set up library install
# given the above PUBLIC_HEADER property set, this
# pulls along all the header files with the library.
if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS itkdouble-conversion
    EXPORT ${DOUBLECONVERSION_TARGETS_NAME}
    RUNTIME DESTINATION "${DOUBLECONVERSION_INSTALL_BIN_DIR}" COMPONENT Applications
    LIBRARY DESTINATION "${DOUBLECONVERSION_INSTALL_LIB_DIR}/lib" COMPONENT Libraries
    ARCHIVE DESTINATION "${DOUBLECONVERSION_INSTALL_LIB_DIR}/lib" COMPONENT Development)
endif()

if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  install(FILES ${headers}
    DESTINATION "${DOUBLECONVERSION_INSTALL_INCLUDE_DIR}" COMPONENT Development)
endif()

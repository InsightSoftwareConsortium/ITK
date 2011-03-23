project(ITK-IO-Base)

# check if the platform has c style IO functions that uses wchar_t * filenames
try_compile(ITK_SUPPORTS_WCHAR_T_FILENAME_CSTYLEIO
  ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/CMakeTmp
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkTestCStyleIOWCharFilename.cxx
  OUTPUT_VARIABLE ITK_SUPPORTS_WCHAR_T_FILENAME_CSTYLEIO_OUTPUT)

# check if the platform has IO streams constructors that uses wchar_t * filenames
try_compile(ITK_SUPPORTS_WCHAR_T_FILENAME_IOSTREAMS_CONSTRUCTORS
  ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/CMakeTmp
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkTestIOStreamsWCharFilenameConstructors.cxx
  OUTPUT_VARIABLE ITK_SUPPORTS_WCHAR_T_FILENAME_IOSTREAMS_CONSTRUCTORS_OUTPUT)

# check if the platform can compile fdstream.hpp
if(HAVE_UNISTD_H)
  set(ITK_SUPPORTS_FDSTREAM_HPP_COMPILE_DEFINITIONS -DITK_HAVE_UNISTD_H=${HAVE_UNISTD_H})
endif()
try_compile(ITK_SUPPORTS_FDSTREAM_HPP
  ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/CMakeTmp
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkTestFDStream.cxx
  CMAKE_FLAGS -DINCLUDE_DIRECTORIES:STRING=${ITK-IO-Base_SOURCE_DIR}/include
  COMPILE_DEFINITIONS ${ITK_SUPPORTS_FDSTREAM_HPP_COMPILE_DEFINITIONS}
  OUTPUT_VARIABLE ITK_SUPPORTS_FDSTREAM_HPP_OUTPUT)

configure_file(src/itkIOConfigure.h.in itkIOConfigure.h)

set(ITK-IO-Base_INCLUDE_DIRS ${ITK-IO-Base_BINARY_DIR})
set(ITK-IO-Base_LIBRARIES ITK-IO-Base)
itk_module_impl()

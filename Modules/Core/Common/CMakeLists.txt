project(ITKCommon)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})

# Version string should not include patch level.  The major.minor is
# enough to distinguish available features of the toolkit.
set(ITK_VERSION_STRING "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")

include(TestBigEndian)
test_big_endian(CMAKE_WORDS_BIGENDIAN)

include(FindThreads)
set(ITK_USE_WIN32_THREADS 0)
set(ITK_USE_PTHREADS 0)
set(ITK_HP_PTHREADS 0)
if(CMAKE_USE_WIN32_THREADS_INIT)
  set(ITK_USE_WIN32_THREADS 1)
elseif(CMAKE_USE_PTHREADS_INIT)
  set(ITK_USE_PTHREADS 1)
elseif(CMAKE_HP_PTHREADS_INIT)
  set(ITK_HP_PTHREADS 1)
endif()
set(CMAKE_THREAD_LIBS "${CMAKE_THREAD_LIBS_INIT}" CACHE STRING "Thread library used.")
mark_as_advanced(CMAKE_THREAD_LIBS)

#
# See if compiler preprocessor has the __FUNCTION__ directive used by itkExceptionMacro
#
include(CheckCPPDirective)
check_cpp_directive_exists(__FUNCTION__ ITK_CPP_FUNCTION)

include(CheckIncludeFiles)

# check if the platform has the header file "fenv.h" which has been added as part of the C99 standard
check_include_files(fenv.h HAVE_FENV_H)
# check if the platform has some widely used header files
check_include_files("unistd.h" HAVE_UNISTD_H)
# check if the platform has the header file "stdint.h" which has been added as part of the C99 standard
check_include_files("stdint.h" HAVE_STDINT_H)
# every system should have this header
check_include_files("stddef.h" HAVE_STDDEF_H)
check_include_files("sys/types.h" HAVE_SYS_TYPES_H)


# Check if this platform support the sse2 rounding functions for 32 and 64 bits
include(CheckSupportForSSERounding)



# Check for the new C++11 alignas type specifier for future portable alignment
try_compile( ITK_HAS_CPP11_ALIGNAS
  ${ITK_BINARY_DIR}/CMakeTmp
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasCPP11Alignas.cxx )

# check of the optional TR1 type_traits, alternative boost
# implementation could also be searched for
include(CheckIncludeFileCXX)
check_include_file_cxx( tr1/type_traits ITK_HAS_STLTR1_TR1_TYPE_TRAITS )
check_include_file_cxx( type_traits ITK_HAS_STLTR1_TYPE_TRAITS )

# Check for C++11 type_traits
try_compile( ITK_HAS_CPP11_TYPETRAITS
  ${ITK_BINARY_DIR}/CMakeTmp
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckCxx11StlTypeTraits.cxx )

try_compile( ITK_HAS_GNU_ATTRIBUTE_ALIGNED
  ${ITK_BINARY_DIR}/CMakeTmp
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasGNUAttributeAligned.cxx
  CMAKE_FLAGS "-DCMAKE_BUILD_TYPE:STRING=Debug" )

configure_file(src/itkConfigure.h.in itkConfigure.h)

set(ITKCommon_INCLUDE_DIRS
  ${ITKCommon_BINARY_DIR}
  )
set(ITKCommon_LIBRARIES ITKCommon)

itk_module_impl()

install(FILES
  ${ITKCommon_BINARY_DIR}/itkConfigure.h
  DESTINATION ${ITKCommon_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )

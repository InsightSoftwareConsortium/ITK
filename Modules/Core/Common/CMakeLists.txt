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
elseif(CMAKE_USE_PTHREADS_INIT AND NOT EMSCRIPTEN)
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


# Test for atomics and other compiler intrinsics
include(itkCheckBuiltins)


# Check for atomic functions
include(CheckSymbolExists)
if(WIN32 AND NOT MINGW)
  check_symbol_exists(InterlockedAdd "windows.h" ITK_HAS_INTERLOCKEDADD)
endif()

if(CMAKE_CXX_STANDARD)
   set(TRY_COMP_CXX_STANDARD
       -DCMAKE_CXX_STANDARD:STRING=${CMAKE_CXX_STANDARD})
endif()
# Identify the version of CXX compiler used when ITK was built. This needs to be
# identified so that external applications can identify how ITK was built.
set(ITK_COMPILED_CXX_STANDARD_VERSION 1)
foreach(CXX_TEST_VERSION 199711L 201103L 201402L)
  try_compile(ITK_MIN_CXX_LEVEL_TEST
    ${CMAKE_CURRENT_BINARY_DIR}/CMakeTmp
    ${CMAKE_CURRENT_LIST_DIR}/CMake/itkGetCXXCompilerVersion.cxx
    CMAKE_FLAGS
        ${TRY_COMP_CXX_STANDARD}
    COMPILE_DEFINITIONS -DITK_CXX_TEST_VERSION=${CXX_TEST_VERSION}
    OUTPUT_VARIABLE ITK_COMPILED_CXX_STANDARD_VERSION_LOG
  )
  if(ITK_MIN_CXX_LEVEL_TEST)
     set(ITK_COMPILED_CXX_STANDARD_VERSION ${CXX_TEST_VERSION})
  endif()
endforeach()

# check of the optional TR1 type_traits, alternative boost
# implementation could also be searched for
include(CheckIncludeFileCXX)
check_include_file_cxx( tr1/type_traits ITK_HAS_STLTR1_TR1_TYPE_TRAITS )
check_include_file_cxx( type_traits ITK_HAS_STLTR1_TYPE_TRAITS )

# Check for C++11 type_traits
try_compile( ITK_HAS_CPP11_TYPETRAITS
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckCxx11StlTypeTraits.cxx )

try_compile( ITK_HAS_GNU_ATTRIBUTE_ALIGNED
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasGNUAttributeAligned.cxx
  CMAKE_FLAGS "-DCMAKE_BUILD_TYPE:STRING=Debug" )

set(ITK_WINDOWS_EXPORT_ALL_SYMBOLS ${CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS})

try_compile(ITK_HAS_MALLINFO
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasMallinfo.cxx
  )

try_compile(ITK_HAS_FEENABLEEXCEPT
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasFeenableexcept.cxx
  )

#-----------------------------------------------------------------------------
# Make default visibility as an option for generated export header

# check if private (hidden, local) symbols can be dynamic_cast between
# different instances.
include(itkCheckPrivateDynamicCast)


set(_template_visibility_init OFF)
if(NOT ITK_PRIVATE_DYNAMIC_CAST)
  set(_template_visibility_init ON)
endif()
# This adds default symbol visibility to template class types, which
# results in 'weak external typeinfo' in libraries that use these
# types, whether with implicit or explicit instantiation, default
# hidden or non-hidden visibility.
# This is required on Mac OSX to avoid dynamic_cast failures across binaries.
# See Change-Id: Ib4a6b8cafe8720c3a2a5b3e6ba833d11002978df for more information."
option(ITK_TEMPLATE_VISIBILITY_DEFAULT "Set symbol visibility to default for template class declarations. https://itk.org/Wiki/ITK/FAQ" "${_template_visibility_init}")
mark_as_advanced(ITK_TEMPLATE_VISIBILITY_DEFAULT)


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

# configure and install compiler feature header files
# that have fine granular support of C++11 features
# identified in a way consistent with CMake 3.1.0 and
# greater
include(CMake/itkGenerateNewCompilerFeatureDetectionHeaders.cmake)

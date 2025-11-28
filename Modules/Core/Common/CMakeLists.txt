project(ITKCommon)
set(
  CMAKE_MODULE_PATH
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake
  ${CMAKE_MODULE_PATH}
)

# Version string should not include patch level.  The major.minor is
# enough to distinguish available features of the toolkit.
set(ITK_VERSION_STRING "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")

include(TestBigEndian)
test_big_endian(CMAKE_WORDS_BIGENDIAN)

include(FindThreads)
set(ITK_USE_WIN32_THREADS 0)
set(ITK_USE_PTHREADS 0)
if(CMAKE_USE_WIN32_THREADS_INIT)
  set(ITK_USE_WIN32_THREADS 1)
elseif(CMAKE_USE_PTHREADS_INIT AND NOT EMSCRIPTEN)
  set(ITK_USE_PTHREADS 1)
endif()
set(
  CMAKE_THREAD_LIBS
  "${CMAKE_THREAD_LIBS_INIT}"
  CACHE STRING
  "Thread library used."
)
mark_as_advanced(CMAKE_THREAD_LIBS)
set(
  ITK_DEFAULT_MAX_THREADS
  128
  CACHE STRING
  "The maximum number of threads allowed. Default: 128"
)
mark_as_advanced(ITK_DEFAULT_MAX_THREADS)

set(ITK_DEFAULT_THREADER "Auto" CACHE STRING "Default multithreader.")
mark_as_advanced(ITK_DEFAULT_THREADER)
set_property(
  CACHE
    ITK_DEFAULT_THREADER
  PROPERTY
    STRINGS
      Auto
      TBB
      Pool
      Platform
)

# See if compiler preprocessor has the __FUNCTION__ directive used by itkExceptionMacro
include(CheckCPPDirective)
check_cpp_directive_exists(__FUNCTION__ ITK_CPP_FUNCTION)

include(CheckIncludeFiles)

# check if the platform has the header file "fenv.h" which has been added as part of the C99 standard
check_include_files(
  fenv.h
  HAVE_FENV_H
)
# check if the platform has some widely used header files
check_include_files(
  "unistd.h"
  HAVE_UNISTD_H
)

if(CMAKE_CXX_STANDARD)
  set(TRY_COMP_CXX_STANDARD -DCMAKE_CXX_STANDARD:STRING=${CMAKE_CXX_STANDARD})
endif()
# Identify the version of CXX compiler used when ITK was built. This needs to be
# identified so that external applications can identify how ITK was built.
set(ITK_COMPILED_CXX_STANDARD_VERSION 1)
foreach(CXX_TEST_VERSION 201103L 201402L 201703L)
  try_compile(
    ITK_MIN_CXX_LEVEL_TEST
    ${CMAKE_CURRENT_BINARY_DIR}/CMakeTmp
    ${CMAKE_CURRENT_LIST_DIR}/CMake/itkGetCXXCompilerVersion.cxx
    CMAKE_FLAGS
      ${TRY_COMP_CXX_STANDARD}
    COMPILE_DEFINITIONS
      -DITK_CXX_TEST_VERSION=${CXX_TEST_VERSION}
    OUTPUT_VARIABLE ITK_COMPILED_CXX_STANDARD_VERSION_LOG
  )
  if(ITK_MIN_CXX_LEVEL_TEST)
    set(ITK_COMPILED_CXX_STANDARD_VERSION ${CXX_TEST_VERSION})
  endif()
endforeach()

try_compile(
  ITK_HAS_GNU_ATTRIBUTE_ALIGNED
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasGNUAttributeAligned.cxx
  CMAKE_FLAGS
    "-DCMAKE_BUILD_TYPE:STRING=Debug"
)

set(ITK_WINDOWS_EXPORT_ALL_SYMBOLS ${CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS})

try_compile(
  ITK_HAS_MALLINFO
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasMallinfo.cxx
)

try_compile(
  ITK_HAS_MALLINFO2
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasMallinfo2.cxx
)

try_compile(
  ITK_HAS_FEENABLEEXCEPT
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasFeenableexcept.cxx
)

try_compile(
  ITK_HAS_FEGETENV
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasFegetenv.cxx
)

try_compile(
  ITK_HAS_FESETENV
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasFesetenv.cxx
)

try_compile(
  ITK_HAS_STRUCT_FENV_T_CONTROL
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasFenvtStructMember.cxx
  COMPILE_DEFINITIONS
    -DITK_CHECK_FENV_T_CONTROL
)

try_compile(
  ITK_HAS_STRUCT_FENV_T_CONTROL_WORD
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasFenvtStructMember.cxx
  COMPILE_DEFINITIONS
    -DITK_CHECK_FENV_T_CONTROL_WORD
)

try_compile(
  ITK_HAS_STRUCT_FENV_T_CW
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasFenvtStructMember.cxx
  COMPILE_DEFINITIONS
    -DITK_CHECK_FENV_T_CW
)

try_compile(
  ITK_HAS_SCHED_GETAFFINITY
  ${ITK_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/itkCheckHasSchedGetAffinity.cxx
)

#-----------------------------------------------------------------------------
# Make default visibility as an option for generated export header

# check if private (hidden, local) symbols can be dynamic_cast between
# different instances.
include(itkCheckPrivateDynamicCast)

set(_template_visibility_init OFF)
if(
  NOT
    ITK_PRIVATE_DYNAMIC_CAST
  AND
    NOT
      (
        CMAKE_CXX_VISIBILITY_PRESET
          STREQUAL
          "hidden"
        AND
          NOT
            BUILD_SHARED_LIBS
      )
)
  set(_template_visibility_init ON)
endif()
# This adds default symbol visibility to template class types, which
# results in 'weak external typeinfo' in libraries that use these
# types, whether with implicit or explicit instantiation, default
# hidden or non-hidden visibility.
# This is required on Mac OSX to avoid dynamic_cast failures across binaries.
# See Change-Id: Ib4a6b8cafe8720c3a2a5b3e6ba833d11002978df for more information."
option(
  ITK_TEMPLATE_VISIBILITY_DEFAULT
  "Set symbol visibility to default for template class declarations. https://insightsoftwareconsortium.github.io/ITKWikiArchive/Wiki/ITK/FAQ"
  "${_template_visibility_init}"
)
mark_as_advanced(ITK_TEMPLATE_VISIBILITY_DEFAULT)

# Configuration variable is named ITK_USE_TBB instead of simply using the
# TBB module name (Module_ITKTBB) for code readability.
set(ITK_USE_TBB ${Module_ITKTBB})

configure_file(src/itkConfigure.h.in itkConfigure.h)
configure_file(src/itkConfigurePrivate.h.in itkConfigurePrivate.h)

set(ITKCommon_INCLUDE_DIRS ${ITKCommon_BINARY_DIR})
set(ITKCommon_LIBRARIES ITKCommon)

itk_module_impl()

install(
  FILES
    ${ITKCommon_BINARY_DIR}/itkConfigure.h
  DESTINATION ${ITKCommon_INSTALL_INCLUDE_DIR}
  COMPONENT Development
)

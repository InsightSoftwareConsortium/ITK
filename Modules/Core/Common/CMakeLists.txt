project(ITK-Common)
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

configure_file(src/itkConfigure.h.in itkConfigure.h)
configure_file(src/itkFloatingPointExceptionsConfigure.h.in  itkFloatingPointExceptionsConfigure.h)

set(ITK-Common_INCLUDE_DIRS
  ${ITK-Common_BINARY_DIR}
  )
set(ITK-Common_LIBRARIES ITK-Common)

itk_module_impl()

install(FILES
  ${ITK-Common_BINARY_DIR}/itkConfigure.h
  DESTINATION ${ITK-Common_INSTALL_INCLUDE_DIR}
  )

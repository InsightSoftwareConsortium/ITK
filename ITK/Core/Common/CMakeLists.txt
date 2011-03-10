cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-Common)
find_package(itk-base 4.0 EXACT REQUIRED)

set(ITK_VERSION_MAJOR "4")
set(ITK_VERSION_MINOR "0")
set(ITK_VERSION_PATCH "0")

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

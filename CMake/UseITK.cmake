
#
#  W A R N I N G
#  -------------
#
# This file is not part of the ITK API.  It exists purely as an
# implementation detail.  This CMake module may change from version to
# version without notice, or even be removed.
#
# We mean it.
#

# This file sets up include directories, link directories, IO settings and
# compiler settings for a project to use ITK.  It should not be
# included directly, but rather through the ITK_USE_FILE setting
# obtained from ITKConfig.cmake.


# Add compiler flags needed to use ITK.
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
include(${ITK_CMAKE_DIR}/ITKInitializeCXXStandard.cmake)

if(MSVC)
  if(ITK_MSVC_STATIC_CRT)
    message(STATUS "ITK is setting ${PROJECT_NAME}'s MSVC_RUNTIME_LIBRARY to static")
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
  else()
    message(STATUS "ITK is setting ${PROJECT_NAME}'s MSVC_RUNTIME_LIBRARY to dynamic")
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")
  endif()
endif()

# Add include directories needed to use ITK.
include_directories(BEFORE ${ITK_INCLUDE_DIRS})

# Add link directories needed to use ITK.
link_directories(${ITK_LIBRARY_DIRS})

itk_generate_factory_registration()


#-----------------------------------------------------------------------------
if(NOT ITK_NO_IO_FACTORY_REGISTER_MANAGER)
  set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS ITK_IO_FACTORY_REGISTER_MANAGER)
  include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/ITKFactoryRegistration)
endif()

if(NOT ITK_NO_FFT_FACTORY_REGISTER_MANAGER)
  set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS ITK_FFT_FACTORY_REGISTER_MANAGER)
  include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/ITKFactoryRegistration)
endif()

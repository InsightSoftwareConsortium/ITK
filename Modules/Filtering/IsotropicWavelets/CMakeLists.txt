project(IsotropicWavelets)

set(IsotropicWavelets_LIBRARIES IsotropicWavelets)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  # Hide clang warnings about undefined-var-template
  if(CMAKE_CXX_COMPILER_ID MATCHES "Clang") # using regular Clang or AppleClang
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-undefined-var-template")
  endif()
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()

project(TotalVariation)

set(TotalVariation_LIBRARIES TotalVariation)

set(ep_name proxTV)
if(FALSE) # NOT DEFINED "${ep_name}_DIR")
  include(ExternalProject)
  # proxTV depends on LAPACKE and LAPACK, not our bussiness for now
  # Build it
  ExternalProject_Add(
    ${ep_name}
    GIT_REPOSITORY https://github.com/phcerdan/proxTV
    GIT_TAG cmake_support
  )
else()
endif()
find_package(proxTV REQUIRED CONFIG)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  set(ITK_DIR ${CMAKE_BINARY_DIR})
  itk_module_impl()
endif()

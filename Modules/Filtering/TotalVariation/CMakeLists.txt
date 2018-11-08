project(TotalVariation)

set(TotalVariation_LIBRARIES TotalVariation)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
endif()

message(WARNING "ITKEigen_INCLUDE_DIRS: ${ITKEigen_INCLUDE_DIRS}")
message(WARNING "ITKEigen3_INCLUDE_DIRS: ${ITKEigen3_INCLUDE_DIRS}")
message(WARNING "ITKCommon_INCLUDE_DIR: ${ITKCommon_INCLUDE_DIRS}")

if(NOT DEFINED proxTV_DIR)
  message(WARNING "NOT DEFINED proxTV_DIR")
  include(FetchContent)
  set(ep_name proxTV-ep)
  # Build it
  set(unmangled_dirs "")
  foreach(dir ${ITKEigen3_INCLUDE_DIRS})
    message(STATUS "dir: ${dir}")
    list(APPEND unmangled_dirs "${dir}/itkeigen")
  endforeach(dir)
  message(STATUS "  unmangled_dirs: ${unmangled_dirs}")

  FetchContent_Declare(
    ${ep_name}
    GIT_REPOSITORY https://github.com/phcerdan/proxTV
    GIT_TAG use_eigen
  )

  FetchContent_GetProperties(${ep_name})

  set(proxTV_USE_LAPACK 0)
  set(Eigen3_DIR ${unmangled_dirs})
  if(NOT ${ep_name}_POPULATED)
    FetchContent_Populate(${ep_name})
    message(
      STATUS
      "ARGUMENTS: ${proxtv-ep_SOURCE_DIR}"
      "${proxtv-ep_BINARY_DIR}"
    )
    add_subdirectory("${proxtv-ep_SOURCE_DIR}" "${proxtv-ep_BINARY_DIR}")
    set(proxTV_DIR ${proxtv-ep_BINARY_DIR})
    find_package(proxTV REQUIRED CONFIG)
  endif()
  message(STATUS "  GOLA")
else()
  find_package(proxTV REQUIRED CONFIG)
endif()

if(NOT ITK_SOURCE_DIR)
  include(ITKModuleExternal)
else()
  set(ITK_DIR ${CMAKE_BINARY_DIR})
  itk_module_impl()
endif()

find_package(TBB CONFIG) # must have TBBConfig.cmake, provided since version tbb2017_20170604oss

if (NOT TBB_FOUND)
  list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/CMake")
  find_package(TBB)
endif()
if (NOT TBB_FOUND)
  message(FATAL_ERROR "find_package(TBB) failed.")
endif()

get_target_property(TBB_INCLUDE_DIRS TBB::tbb INTERFACE_INCLUDE_DIRECTORIES)

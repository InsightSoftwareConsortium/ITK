project(ITKTBB)
set(ITKTBB_THIRD_PARTY 1)

set(ITKTBB_SYSTEM_INCLUDE_DIRS ${TBB_INCLUDE_DIRS})
set(ITKTBB_LIBRARIES TBB::tbb)
set(ITKTBB_NO_SRC 1)

# When this module is loaded by an app, load TBB too.
set(ITKTBB_EXPORT_CODE_INSTALL "
set(TBB_DIR \"${TBB_DIR}\")
find_package(TBB REQUIRED CONFIG)
")

# When this module is configured by an app, configure TBB too.
set(ITKTBB_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(TBB_DIR \"${TBB_DIR}\")
  find_package(TBB REQUIRED CONFIG)
endif()
")

itk_module_impl()

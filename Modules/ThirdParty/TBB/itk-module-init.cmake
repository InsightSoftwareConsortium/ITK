find_package(TBB REQUIRED CONFIG) # must have TBBConfig.cmake, provided since version tbb2017_20170604oss
get_target_property(TBB_INCLUDE_DIRS TBB::tbb INTERFACE_INCLUDE_DIRECTORIES)

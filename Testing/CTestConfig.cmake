get_filename_component(this_dir "${CMAKE_CURRENT_LIST_FILE}" PATH)
get_filename_component(parent_dir "${this_dir}" PATH)
include("${parent_dir}/CTestConfig.cmake")

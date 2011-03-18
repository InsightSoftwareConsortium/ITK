# vxl/config/cmake/config/vxl_shared_link_test/CMakeLists.txt
#

PROJECT(vxl_pic_compatible)

ADD_LIBRARY(cmTryCompileStaticLib STATIC static_src.cxx)
ADD_LIBRARY(cmTryCompileSharedLib SHARED shared_src.cxx)
TARGET_LINK_LIBRARIES(cmTryCompileSharedLib cmTryCompileStaticLib ${LINK_LIBRARIES})
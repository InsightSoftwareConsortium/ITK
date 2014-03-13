# vxl/config/cmake/config/vxl_shared_link_test/CMakeLists.txt
#
IF(POLICY CMP0025)
  CMAKE_POLICY(SET CMP0025 NEW)
ENDIF()
IF(POLICY CMP0042)
  CMAKE_POLICY(SET CMP0042 NEW)
ENDIF()
PROJECT(vxl_pic_compatible)

ADD_LIBRARY(cmTryCompileStaticLib STATIC static_src.cxx)
ADD_LIBRARY(cmTryCompileSharedLib SHARED shared_src.cxx)
TARGET_LINK_LIBRARIES(cmTryCompileSharedLib cmTryCompileStaticLib ${LINK_LIBRARIES})

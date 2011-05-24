project(Video-IO-OpenCV)

set(CMAKE_MODULE_PATH ${Video-Core-OpenCV_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})
find_package ( OpenCV REQUIRED )

set(Video-IO-OpenCV_INCLUDE_DIRS ${OpenCV_INCLUDE_DIRS} )
set(Video-IO-OpenCV_LIBRARIES ${OpenCV_LIBS} Video-Core-Common Video-IO-OpenCV)
itk_module_impl()

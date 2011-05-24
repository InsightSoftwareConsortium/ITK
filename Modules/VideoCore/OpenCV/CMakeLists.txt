project(Video-Core-OpenCV)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})
find_package ( OpenCV REQUIRED )

set( Video-Core-OpenCV_INCLUDE_DIRS ${OpenCV_INCLUDE_DIRS} )
itk_module_impl()

project(Video-IO-OpenCV)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})

find_package ( OpenCV REQUIRED )

set(Video-IO-OpenCV_INCLUDE_DIRS ${OpenCV_INCLUDE_DIRS} )
set(Video-IO-OpenCV_LIBRARIES ${OpenCV_LIBS} Video-Common Video-IO-OpenCV)
itk_module_impl()

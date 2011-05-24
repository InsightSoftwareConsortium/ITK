project(Video-Filters-OpenCV)

set(CMAKE_MODULE_PATH ${Video-Core-OpenCV_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})
find_package ( OpenCV REQUIRED )

set(Video-Filters-OpenCV_INCLUDE_DIRS ${OpenCV_INCLUDE_DIRS} )
set(Video-Filters-OpenCV_LIBRARIES ${OpenCV_LIBS})
itk_module_impl()

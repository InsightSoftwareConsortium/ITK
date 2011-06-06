project(Video-Core-OpenCV)

find_package ( OpenCV REQUIRED )

set( Video-Core-OpenCV_INCLUDE_DIRS ${OpenCV_INCLUDE_DIRS} )
itk_module_impl()

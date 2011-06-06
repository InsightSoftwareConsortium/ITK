project(Video-Filters-OpenCV)

find_package ( OpenCV REQUIRED )

set(Video-Filters-OpenCV_INCLUDE_DIRS ${OpenCV_INCLUDE_DIRS} )
set(Video-Filters-OpenCV_LIBRARIES ${OpenCV_LIBS})
itk_module_impl()

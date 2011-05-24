project(Video-IO-VXL)

find_package ( VXL REQUIRED )
include( ${VXL_CMAKE_DIR}/UseVXL.cmake )

set(Video-IO-VXL_INCLUDE_DIRS ${VXL_INCLUDE_DIRS} )
set(Video-IO-VXL_LIBRARIES Video-IO-VXL Video-Core-Common vcl vidl avcodec avformat)
itk_module_impl()

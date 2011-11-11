project(ITKVideoBridgeOpenCV)

set(ITKVideoBridgeOpenCV_LIBRARIES ITKVideoBridgeOpenCV)

set(ITKVideoBridgeOpenCV_SYSTEM_INCLUDE_DIRS ${OpenCV_INCLUDE_DIRS})
set(ITKVideoBridgeOpenCV_SYSTEM_LIBRARY_DIRS ${OpenCV_LIB_DIR})

itk_module_impl()

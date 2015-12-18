project(ITKVideoBridgeOpenCV)

set(ITKVideoBridgeOpenCV_LIBRARIES ITKVideoBridgeOpenCV)

# When this module is loaded by an app, load OpenCV too.
set(ITKVideoBridgeOpenCV_EXPORT_CODE_INSTALL "
set(OpenCV_DIR \"${OpenCV_DIR}\")
find_package(OpenCV REQUIRED)
")
set(ITKVideoBridgeOpenCV_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(OpenCV_DIR \"${OpenCV_DIR}\")
  find_package(OpenCV REQUIRED)
endif()
")

set(ITKVideoBridgeOpenCV_SYSTEM_INCLUDE_DIRS ${OpenCV_INCLUDE_DIRS})
set(ITKVideoBridgeOpenCV_SYSTEM_LIBRARY_DIRS ${OpenCV_LIB_DIR})

itk_module_impl()

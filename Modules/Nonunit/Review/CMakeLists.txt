project(ITKReview)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})
set(ITKReview_LIBRARIES ITKReview)

set(ITK_USE_REVIEW ON)


itk_module_impl()

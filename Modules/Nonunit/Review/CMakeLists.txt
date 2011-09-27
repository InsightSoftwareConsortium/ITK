project(ITKReview)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})
set(ITKReview_LIBRARIES ITKReview)

set(ITK_USE_REVIEW ON)

# Deal with MINC2 options
option(ITK_USE_MINC2 "Use MINC2 File Format" OFF)
mark_as_advanced(ITK_USE_MINC2)
if( ITK_USE_MINC2 )
  find_package( MINC2 )
  if( NOT MINC2_FOUND )
    message("You requested to use MINC2 but some components are missing")
  endif( NOT MINC2_FOUND )
endif( ITK_USE_MINC2 )

itk_module_impl()

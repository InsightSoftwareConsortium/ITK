project(ITKVtkGlue)
set(ITKVtkGlue_LIBRARIES ITKVtkGlue)

#
# Add the third party includes and libraries
#

set(ITKVtkGlue_SYSTEM_INCLUDE_DIRS ${VTK_INCLUDE_DIRS})
set(ITKVtkGlue_SYSTEM_LIBRARY_DIRS ${VTK_LIBRARY_DIRS})

itk_module_impl()

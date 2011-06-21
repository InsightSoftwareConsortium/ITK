project(ITK-ItkVtkGlue)
set(ITK-ItkVtkGlue_LIBRARIES ITK-ItkVtkGlue)

#
# Add the third party includes and libraries
#
set(ITK-ItkVtkGlue_SYSTEM_INCLUDE_DIRS ${VTK_INCLUDE_DIRS})
set(ITK-ItkVtkGlue_SYSTEM_LIBRARY_DIRS ${VTK_LIBRARY_DIRS})

itk_module_impl()

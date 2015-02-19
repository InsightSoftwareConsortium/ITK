itk_wrap_module(ITKImageFusion)

# Save default includes so they can be reseted afterwards
set(tmp_default_includes ${WRAPPER_DEFAULT_INCLUDE})

# Add itkStatisticsLabelObject.h, it is not automatically detected
# (but it maybe should)
list(APPEND WRAPPER_DEFAULT_INCLUDE "itkStatisticsLabelObject.h")

itk_auto_load_submodules()
itk_end_wrap_module()

# Reset the WRAPPER_DEFAULT_INCLUDE to original value
set(WRAPPER_DEFAULT_INCLUDE ${tmp_default_includes})

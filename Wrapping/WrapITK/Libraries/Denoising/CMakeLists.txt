WRAP_LIBRARY(Denoising)

set(WRAPPER_LIBRARY_GROUPS
  itkCurvatureFlowImageFilter
  itkMinMaxCurvatureFlowImageFilter
  itkVotingBinaryImageFilter
)

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics Base)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()

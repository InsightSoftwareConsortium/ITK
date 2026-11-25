set(
  ITKReview_SRC
  itkVoxBoCUBImageIO.cxx
  itkVoxBoCUBImageIOFactory.cxx
)

itk_module_add_library(ITKReview ${ITKReview_SRC})

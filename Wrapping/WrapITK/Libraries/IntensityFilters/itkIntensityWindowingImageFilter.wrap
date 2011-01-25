WRAP_CLASS("itk::IntensityWindowingImageFilter" POINTER_WITH_SUPERCLASS)
  # Create rescale filters from every scalar type to every scalar type. Also force
  # filters from every scalar to uchar, to allow for saving as 8-bit images.
  UNIQUE(to_types "UC;${WRAP_ITK_SCALAR}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_SCALAR}" "${to_types}")
END_WRAP_CLASS()

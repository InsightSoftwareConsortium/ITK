WRAP_CLASS("itk::HardConnectedComponentImageFilter" POINTER)
  # Create wrappers from every selected integral (signed and un) type to every
  # selected unsigned type. Also force ulong output for the watershed filter.
  UNIQUE(to_types "UL;${WRAP_ITK_INT}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_INT}" "${to_types}")
END_WRAP_CLASS()

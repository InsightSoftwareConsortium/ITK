WRAP_CLASS("itk::CastImageFilter" POINTER_WITH_SUPERCLASS)
  # Create cast filters between all scalar types. Also force that cast-to-uchar
  # filters are created for all scalar types.
  UNIQUE(types "${WRAP_ITK_SCALAR};UC")
  WRAP_IMAGE_FILTER_COMBINATIONS("${types}" "${types}")
END_WRAP_CLASS()

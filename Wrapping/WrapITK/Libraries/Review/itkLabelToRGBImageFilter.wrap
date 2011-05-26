WRAP_CLASS("itk::LabelToRGBImageFilter" POINTER_WITH_SUPERCLASS)
  UNIQUE(label_types "${WRAP_ITK_INT};UL")

  if(WRAP_rgb_unsigned_short)
    WRAP_IMAGE_FILTER_COMBINATIONS("${label_types}" RGBUS)
  endif(WRAP_rgb_unsigned_short)

  if(WRAP_rgb_unsigned_char)
    WRAP_IMAGE_FILTER_COMBINATIONS("${label_types}" RGBUC)
  endif(WRAP_rgb_unsigned_char)

END_WRAP_CLASS()

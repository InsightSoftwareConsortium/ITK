WRAP_CLASS("itk::LabelOverlayImageFilter" POINTER_WITH_SUPERCLASS)
  UNIQUE(label_types "${WRAP_ITK_INT};UL")

  if(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_COMBINATIONS(US "${label_types}" RGBUS)
  endif(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)

  if(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_COMBINATIONS(UC "${label_types}" RGBUC)
  endif(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)

END_WRAP_CLASS()

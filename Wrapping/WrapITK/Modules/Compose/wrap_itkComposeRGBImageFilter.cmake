WRAP_CLASS("itk::ComposeRGBImageFilter" POINTER_WITH_SUPERCLASS)
  IF(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_TYPES(UC RGBUC)
  ENDIF(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)

  IF(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_TYPES(US RGBUS)
  ENDIF(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)
END_WRAP_CLASS()

WRAP_CLASS("itk::RGBToLuminanceImageFilter" POINTER_WITH_SUPERCLASS)
  IF(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_TYPES(RGBUC UC)
  ENDIF(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)

  IF(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_TYPES(RGBUS US)
  ENDIF(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)

  IF(WRAP_rgba_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_TYPES(RGBAUC UC)
  ENDIF(WRAP_rgba_unsigned_char AND WRAP_unsigned_char)

  IF(WRAP_rgba_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_TYPES(RGBAUS US)
  ENDIF(WRAP_rgba_unsigned_short AND WRAP_unsigned_short)
END_WRAP_CLASS()

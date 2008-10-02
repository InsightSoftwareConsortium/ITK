WRAP_CLASS("itk::ComposeRGBAImageFilter" POINTER_WITH_SUPERCLASS)

  IF(WRAP_rgba_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_TYPES(UC RGBAUC)
  ENDIF(WRAP_rgba_unsigned_char AND WRAP_unsigned_char)

  IF(WRAP_rgba_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_TYPES(US RGBAUS)
  ENDIF(WRAP_rgba_unsigned_short AND WRAP_unsigned_short)

END_WRAP_CLASS()

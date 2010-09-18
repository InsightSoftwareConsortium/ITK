WRAP_CLASS("itk::ComposeRGBImageFilter" POINTER_WITH_SUPERCLASS)
  if(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_TYPES(UC RGBUC)
  endif(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)

  if(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_TYPES(US RGBUS)
  endif(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)
END_WRAP_CLASS()

WRAP_CLASS("itk::RelabelComponentImageFilter" POINTER)
  WRAP_IMAGE_FILTER_USIGN_INT(2)
  WRAP_IMAGE_FILTER_SIGN_INT(2)
  
  # Wrap from ulong to other integral types, even if ulong isn't wrapped. This
  # is needed to be able to use image from watershed image filter.
  IF(NOT WRAP_unsigned_long)
    WRAP_IMAGE_FILTER_COMBINATIONS("UL" "${WRAP_ITK_INT}")
  ENDIF(NOT WRAP_unsigned_long)
END_WRAP_CLASS()

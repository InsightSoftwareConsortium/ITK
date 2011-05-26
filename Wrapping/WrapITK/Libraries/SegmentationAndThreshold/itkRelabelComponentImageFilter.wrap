WRAP_CLASS("itk::RelabelComponentImageFilter" POINTER)
  WRAP_IMAGE_FILTER_USIGN_INT(2)
  WRAP_IMAGE_FILTER_SIGN_INT(2)

  # Wrap from ulong to other integral types, even if ulong isn't wrapped. This
  # is needed to be able to use image from watershed image filter.
  remove(types "${WRAP_ITK_INT}" UL)
  WRAP_IMAGE_FILTER_COMBINATIONS("UL" "${types}")
END_WRAP_CLASS()

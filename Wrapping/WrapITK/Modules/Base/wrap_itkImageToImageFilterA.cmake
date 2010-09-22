WRAP_CLASS("itk::ImageToImageFilter" POINTER)
  # Wrap from each scalar type to each other, and also to uchar (for 8-bit saving)
  # and to ulong (for watershed).
  UNIQUE(from_types "UC;${WRAP_ITK_SCALAR}")
  UNIQUE(to_types "UC;UL;${WRAP_ITK_SCALAR}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${from_types}" "${to_types}")

  # Wrap from ulong to other integral types, even if ulong isn't wrapped. This
  # is needed for the relabel components image filter.
  if(NOT WRAP_unsigned_long)
    WRAP_IMAGE_FILTER_COMBINATIONS("UL" "${WRAP_ITK_INT}")
  endif(NOT WRAP_unsigned_long)

  # Vector types
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_VECTOR}" "${WRAP_ITK_VECTOR}")

  # RGB types
  UNIQUE(rgb "RGBUC;${WRAP_ITK_RGB}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${rgb}" "${rgb}")

  # RGBA types
  UNIQUE(rgba "RGBAUC;${WRAP_ITK_RGBA}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${rgba}" "${rgba}")

END_WRAP_CLASS()

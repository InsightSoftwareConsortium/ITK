WRAP_CLASS("itk::InPlaceImageFilter" POINTER)
  # Wrap from each scalar type to each other, and also to uchar (for 8-bit saving)
  UNIQUE(types "UC;${WRAP_ITK_SCALAR}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${types}" "${types}")

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

  # int <-> RGB
  if(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_TYPES(RGBUC UC)
  endif(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)

  if(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_TYPES(RGBUS US)
  endif(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)

  if(WRAP_rgb_unsigned_char)
    UNIQUE(types "UL;${WRAP_ITK_SCALAR}")
    WRAP_IMAGE_FILTER_COMBINATIONS("${types}" "RGBUC")
  endif(WRAP_rgb_unsigned_char)

  if(WRAP_rgb_unsigned_short)
    UNIQUE(types "UL;${WRAP_ITK_SCALAR}")
    WRAP_IMAGE_FILTER_COMBINATIONS("${types}" "RGBUS")
  endif(WRAP_rgb_unsigned_short)

  # RGBA types
  UNIQUE(rgba "RGBAUC;${WRAP_ITK_RGBA}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${rgba}" "${rgba}")

  # int <-> RGBA
  if(WRAP_rgba_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_TYPES(RGBAUC UC)
  endif(WRAP_rgba_unsigned_char AND WRAP_unsigned_char)

  if(WRAP_rgba_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_TYPES(RGBAUS US)
  endif(WRAP_rgba_unsigned_short AND WRAP_unsigned_short)

  if(WRAP_rgba_unsigned_char)
    UNIQUE(types "UL;${WRAP_ITK_SCALAR}")
    WRAP_IMAGE_FILTER_COMBINATIONS("${types}" "RGBAUC")
  endif(WRAP_rgba_unsigned_char)

  if(WRAP_rgba_unsigned_short)
    UNIQUE(types "UL;${WRAP_ITK_SCALAR}")
    WRAP_IMAGE_FILTER_COMBINATIONS("${types}" "RGBAUS")
  endif(WRAP_rgba_unsigned_short)

END_WRAP_CLASS()

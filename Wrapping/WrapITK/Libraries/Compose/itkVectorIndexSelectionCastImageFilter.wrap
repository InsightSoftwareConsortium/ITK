WRAP_CLASS("itk::VectorIndexSelectionCastImageFilter" POINTER_WITH_SUPERCLASS)

  # begin with VectorImages
  UNIQUE(to_types "UC;${WRAP_ITK_SCALAR}")
  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${to_types})
      WRAP_TEMPLATE("${ITKM_VI${t}${d}}${ITKM_I${t}${d}}" "${ITKT_VI${t}${d}},${ITKT_I${t}${d}}")
    endforeach(t)
  endforeach(d)

  # continue with vector and rgb pixel types
  if(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_TYPES(RGBUS US)
  endif(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)

  if(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_TYPES(RGBUC UC)
  endif(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)

  if(WRAP_vector_double AND WRAP_double)
    WRAP_IMAGE_FILTER_TYPES(VD D)
  endif(WRAP_vector_double AND WRAP_double)

  if(WRAP_vector_float AND WRAP_float)
    WRAP_IMAGE_FILTER_TYPES(VF F)
  endif(WRAP_vector_float AND WRAP_float)

  # Wrap RGBA image types
  if(WRAP_rgba_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_TYPES(RGBAUS US)
  endif(WRAP_rgba_unsigned_short AND WRAP_unsigned_short)
  if(WRAP_rgba_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_TYPES(RGBAUC UC)
  endif(WRAP_rgba_unsigned_char AND WRAP_unsigned_char)

END_WRAP_CLASS()

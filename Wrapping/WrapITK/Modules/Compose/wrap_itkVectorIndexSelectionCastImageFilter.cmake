WRAP_CLASS("itk::VectorIndexSelectionCastImageFilter" POINTER_WITH_SUPERCLASS)
  
  # begin with VectorImages
  UNIQUE(to_types "UC;${WRAP_ITK_SCALAR}")
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${to_types})
      WRAP_TEMPLATE("${ITKM_VI${t}${d}}${ITKM_I${t}${d}}" "${ITKT_VI${t}${d}},${ITKT_I${t}${d}}")
    ENDFOREACH(t)
  ENDFOREACH(d)
  
  # continue with vector and rgb pixel types
  IF(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)
    WRAP_IMAGE_FILTER_TYPES(RGBUS US)
  ENDIF(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)

  IF(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)
    WRAP_IMAGE_FILTER_TYPES(RGBUC UC)
  ENDIF(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)

  IF(WRAP_vector_double AND WRAP_double)
    WRAP_IMAGE_FILTER_TYPES(VD D)
  ENDIF(WRAP_vector_double AND WRAP_double)
  
  IF(WRAP_vector_float AND WRAP_float)
    WRAP_IMAGE_FILTER_TYPES(VF F)
  ENDIF(WRAP_vector_float AND WRAP_float)
END_WRAP_CLASS()

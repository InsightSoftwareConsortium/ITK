WRAP_CLASS("itk::ImageToImageFilter" POINTER)
  # Wrap from each scalar type to each other, and also to uchar (for 8-bit saving)
  # and to ulong (for watershed).
  UNIQUE(from_types "UC;${WRAP_ITK_SCALAR}")
  UNIQUE(to_types "UC;UL;${WRAP_ITK_SCALAR}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${from_types}" "${to_types}")
  
  # Wrap from ulong to other integral types, even if ulong isn't wrapped. This
  # is needed for the relabel components image filter.
  IF(NOT WRAP_unsigned_long)
    WRAP_IMAGE_FILTER_COMBINATIONS("UL" "${WRAP_ITK_INT}")
  ENDIF(NOT WRAP_unsigned_long)
  
  # Vector types
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_VECTOR}" "${WRAP_ITK_VECTOR}")
  
  # RGB types
  UNIQUE(rgb "RGBUC;${WRAP_ITK_RGB}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${rgb}" "${rgb}")

  # complex types
  WRAP_IMAGE_FILTER_COMPLEX_REAL(2)
 
  # scalar <-> RGB
  UNIQUE(stypes "UL;${WRAP_ITK_SCALAR}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${stypes}" "${WRAP_ITK_RGB}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_RGB}" "${stypes}")
 
  # VectorImage <-> scalar
  UNIQUE(to_types "UC;${WRAP_ITK_SCALAR}")
    FOREACH(d ${WRAP_ITK_DIMS})
      FOREACH(t ${to_types})
        WRAP_TEMPLATE("${ITKM_VI${t}${d}}${ITKM_I${t}${d}}" "${ITKT_VI${t}${d}},${ITKT_I${t}${d}}")
        WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_VI${t}${d}}" "${ITKT_I${t}${d}},${ITKT_VI${t}${d}}")
      ENDFOREACH(t)
    ENDFOREACH(d)
      
  # *Vector <-> scalar
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_VECTOR}" "${WRAP_ITK_SCALAR}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_SCALAR}" "${WRAP_ITK_VECTOR}")

  # complex <-> scalar
  IF(WRAP_complex_float AND WRAP_float)
    WRAP_IMAGE_FILTER_TYPES(CF F)
    WRAP_IMAGE_FILTER_TYPES(F CF)
  ENDIF(WRAP_complex_float AND WRAP_float)

  IF(WRAP_complex_double AND WRAP_double)
    WRAP_IMAGE_FILTER_TYPES(CD D)
    WRAP_IMAGE_FILTER_TYPES(D CD)
  ENDIF(WRAP_complex_double AND WRAP_double)

  # Wrap dim=3 -> dim=2, dim=3 -> dim=2, etc.
  FOREACH(d ${WRAP_ITK_DIMS})    
    FOREACH(d2 ${WRAP_ITK_DIMS})
      IF (NOT "${d}" EQUAL "${d2}") # this was already taken care of elsewhere
        FOREACH(t ${WRAP_ITK_SCALAR})
          WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_I${t}${d2}}"
                        "${ITKT_I${t}${d}},${ITKT_I${t}${d2}}")
        ENDFOREACH(t)
      ENDIF(NOT "${d}" EQUAL "${d2}")
    ENDFOREACH(d2)
  ENDFOREACH(d)

END_WRAP_CLASS()


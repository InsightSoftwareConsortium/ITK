WRAP_CLASS("itk::MultiplyImageFilter" POINTER_WITH_SUPERCLASS)
  WRAP_IMAGE_FILTER_SCALAR(3)
  WRAP_IMAGE_FILTER_COMPLEX_REAL(3)

  FOREACH(d ${WRAP_ITK_DIMS})
    IF(WRAP_complex_float AND WRAP_float)
      WRAP_TEMPLATE("${ITKM_ICF${d}}${ITKM_IF${d}}${ITKM_ICF${d}}" "${ITKT_ICF${d}},${ITKT_IF${d}},${ITKT_ICF${d}}")
    ENDIF(WRAP_complex_float AND WRAP_float)
  
    IF(WRAP_complex_double AND WRAP_double)
      WRAP_TEMPLATE("${ITKM_ICD${d}}${ITKM_ID${d}}${ITKM_ICD${d}}" "${ITKT_ICD${d}},${ITKT_ID${d}},${ITKT_ICD${d}}")
    ENDIF(WRAP_complex_double AND WRAP_double)
  ENDFOREACH(d)

END_WRAP_CLASS()

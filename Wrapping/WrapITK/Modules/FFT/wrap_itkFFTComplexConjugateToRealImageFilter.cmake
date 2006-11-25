WRAP_CLASS("itk::FFTComplexConjugateToRealImageFilter" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
    IF(WRAP_complex_float AND WRAP_float)
      WRAP_TEMPLATE("${ITKM_F}${d}" "${ITKT_F},${d}")
    ENDIF(WRAP_complex_float AND WRAP_float)
  
    IF(WRAP_complex_double AND WRAP_double)
      WRAP_TEMPLATE("${ITKM_D}${d}" "${ITKT_D},${d}")
    ENDIF(WRAP_complex_double AND WRAP_double)
  ENDFOREACH(d)
END_WRAP_CLASS()


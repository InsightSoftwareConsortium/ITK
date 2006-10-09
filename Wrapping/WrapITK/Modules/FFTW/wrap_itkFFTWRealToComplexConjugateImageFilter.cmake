WRAP_CLASS("itk::FFTWRealToComplexConjugateImageFilter" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
    IF(WRAP_complex_float AND WRAP_float AND FFTWF_LIB)
      WRAP_TEMPLATE("${ITKM_F}${d}" "${ITKT_F},${d}")
    ENDIF(WRAP_complex_float AND WRAP_float AND FFTWF_LIB)
  
    IF(WRAP_complex_double AND WRAP_double AND FFTWD_LIB)
      WRAP_TEMPLATE("${ITKM_D}${d}" "${ITKT_D},${d}")
    ENDIF(WRAP_complex_double AND WRAP_double AND FFTWD_LIB)
  ENDFOREACH(d)
END_WRAP_CLASS()


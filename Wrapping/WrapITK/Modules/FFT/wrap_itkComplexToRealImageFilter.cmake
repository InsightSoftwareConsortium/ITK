WRAP_CLASS("itk::ComplexToRealImageFilter" POINTER_WITH_SUPERCLASS)
  IF(WRAP_complex_float AND WRAP_float)
    WRAP_IMAGE_FILTER_TYPES(CF F)
  ENDIF(WRAP_complex_float AND WRAP_float)

  IF(WRAP_complex_double AND WRAP_double)
    WRAP_IMAGE_FILTER_TYPES(CD D)
  ENDIF(WRAP_complex_double AND WRAP_double)
END_WRAP_CLASS()

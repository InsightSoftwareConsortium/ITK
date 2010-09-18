WRAP_CLASS("itk::ComplexToRealImageFilter" POINTER_WITH_SUPERCLASS)
  if(WRAP_complex_float AND WRAP_float)
    WRAP_IMAGE_FILTER_TYPES(CF F)
  endif(WRAP_complex_float AND WRAP_float)

  if(WRAP_complex_double AND WRAP_double)
    WRAP_IMAGE_FILTER_TYPES(CD D)
  endif(WRAP_complex_double AND WRAP_double)
END_WRAP_CLASS()

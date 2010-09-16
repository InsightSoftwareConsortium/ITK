WRAP_CLASS("itk::FFTWRealToComplexConjugateImageFilter" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    if(WRAP_complex_float AND WRAP_float AND FFTWF_LIB)
      WRAP_TEMPLATE("${ITKM_F}${d}" "${ITKT_F},${d}")
    endif(WRAP_complex_float AND WRAP_float AND FFTWF_LIB)

    if(WRAP_complex_double AND WRAP_double AND FFTWD_LIB)
      WRAP_TEMPLATE("${ITKM_D}${d}" "${ITKT_D},${d}")
    endif(WRAP_complex_double AND WRAP_double AND FFTWD_LIB)
  endforeach(d)
END_WRAP_CLASS()


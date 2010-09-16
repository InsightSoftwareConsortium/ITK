WRAP_CLASS("itk::Compose3DCovariantVectorImageFilter" POINTER_WITH_SUPERCLASS)
  if(WRAP_float AND WRAP_covariant_vector_float)
    WRAP_IMAGE_FILTER_TYPES(F CVF 3)
  endif(WRAP_float AND WRAP_covariant_vector_float)

  if(WRAP_double AND WRAP_covariant_vector_double)
    WRAP_IMAGE_FILTER_TYPES(D CVD 3)
  endif(WRAP_double AND WRAP_covariant_vector_double)
END_WRAP_CLASS()

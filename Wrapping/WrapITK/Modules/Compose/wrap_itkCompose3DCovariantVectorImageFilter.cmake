WRAP_CLASS("itk::Compose3DCovariantVectorImageFilter" POINTER_WITH_SUPERCLASS)
  IF(WRAP_float AND WRAP_covariant_vector_float)
    WRAP_IMAGE_FILTER_TYPES(F CVF 3)
  ENDIF(WRAP_float AND WRAP_covariant_vector_float)

  IF(WRAP_double AND WRAP_covariant_vector_double)
    WRAP_IMAGE_FILTER_TYPES(D CVD 3)
  ENDIF(WRAP_double AND WRAP_covariant_vector_double)
END_WRAP_CLASS()

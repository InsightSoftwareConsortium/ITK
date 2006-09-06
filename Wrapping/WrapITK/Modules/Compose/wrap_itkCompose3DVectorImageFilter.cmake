WRAP_CLASS("itk::Compose3DVectorImageFilter" POINTER_WITH_SUPERCLASS)
  IF(WRAP_float AND WRAP_vector_float)
    WRAP_IMAGE_FILTER_TYPES(F VF 3)
  ENDIF(WRAP_float AND WRAP_vector_float)

  IF(WRAP_double AND WRAP_vector_double)
    WRAP_IMAGE_FILTER_TYPES(D VD 3)
  ENDIF(WRAP_double AND WRAP_vector_double)
END_WRAP_CLASS()

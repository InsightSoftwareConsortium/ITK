WRAP_CLASS("itk::GradientImageFilter" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${WRAP_ITK_SCALAR})

      IF(WRAP_covariant_vector_float)
        WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_F}${ITKM_F}" "${ITKT_I${t}${d}},${ITKT_F},${ITKT_F}")
      ENDIF(WRAP_covariant_vector_float)
  
      IF(WRAP_covariant_vector_double)
        WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_D}${ITKM_D}" "${ITKT_I${t}${d}},${ITKT_D},${ITKT_D}")
      ENDIF(WRAP_covariant_vector_double)

    ENDFOREACH(t)
  ENDFOREACH(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::GradientImageFilter" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${WRAP_ITK_SCALAR})

      if(WRAP_covariant_vector_float)
        WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_F}${ITKM_F}" "${ITKT_I${t}${d}},${ITKT_F},${ITKT_F}")
      endif(WRAP_covariant_vector_float)

      if(WRAP_covariant_vector_double)
        WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_D}${ITKM_D}" "${ITKT_I${t}${d}},${ITKT_D},${ITKT_D}")
      endif(WRAP_covariant_vector_double)

    endforeach(t)
  endforeach(d)
END_WRAP_CLASS()

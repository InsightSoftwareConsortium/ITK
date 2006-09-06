WRAP_CLASS("itk::ImportImageFilter" POINTER)

  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_${t}}${d}" "${ITKT_${t}},${d}")
    ENDFOREACH(t)
  ENDFOREACH(d)

END_WRAP_CLASS()


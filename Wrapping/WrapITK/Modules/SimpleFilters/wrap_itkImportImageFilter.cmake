WRAP_CLASS("itk::ImportImageFilter" POINTER)

  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_${t}}${d}" "${ITKT_${t}},${d}")
    endforeach(t)
  endforeach(d)

END_WRAP_CLASS()

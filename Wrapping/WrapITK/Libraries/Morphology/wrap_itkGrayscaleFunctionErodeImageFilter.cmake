# class hierarchy is not the same with or without consolidated morphology
set(opts POINTER_WITH_SUPERCLASS)

WRAP_CLASS("itk::GrayscaleFunctionErodeImageFilter" ${opts})
  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_I${t}${d}}${ITKM_SE${d}}"    "${ITKT_I${t}${d}},${ITKT_I${t}${d}},${ITKT_SE${d}}")
    endforeach(t)
  endforeach(d)
END_WRAP_CLASS()

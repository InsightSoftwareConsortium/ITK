# class hierarchy is not the same with or without consolidated morphology
IF(ITK_USE_CONSOLIDATED_MORPHOLOGY)
  SET(opts POINTER_WITH_SUPERCLASS)
ELSE(ITK_USE_CONSOLIDATED_MORPHOLOGY)
  SET(opts POINTER)
ENDIF(ITK_USE_CONSOLIDATED_MORPHOLOGY)

WRAP_CLASS("itk::GrayscaleFunctionErodeImageFilter" ${opts})
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_I${t}${d}}${ITKM_SE${d}}"    "${ITKT_I${t}${d}},${ITKT_I${t}${d}},${ITKT_SE${d}}")
    ENDFOREACH(t)
  ENDFOREACH(d)
END_WRAP_CLASS()

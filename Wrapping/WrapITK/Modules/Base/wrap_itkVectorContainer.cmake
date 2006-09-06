WRAP_CLASS("itk::VectorContainer" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_UI}${ITKM_LSN${t}${d}}"  "${ITKT_UI},${ITKT_LSN${t}${d}}")
    ENDFOREACH(t)
    
    WRAP_TEMPLATE("${ITKM_UL}${ITKM_VD${d}}"    "${ITKT_UL},${ITKT_VD${d}}")
  ENDFOREACH(d)
END_WRAP_CLASS()

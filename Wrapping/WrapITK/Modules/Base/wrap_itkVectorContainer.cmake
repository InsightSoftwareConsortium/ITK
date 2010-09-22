WRAP_CLASS("itk::VectorContainer" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_UI}${ITKM_LSN${t}${d}}"  "${ITKT_UI},${ITKT_LSN${t}${d}}")
    endforeach(t)

    WRAP_TEMPLATE("${ITKM_UL}${ITKM_VD${d}}"    "${ITKT_UL},${ITKT_VD${d}}")
  endforeach(d)
END_WRAP_CLASS()

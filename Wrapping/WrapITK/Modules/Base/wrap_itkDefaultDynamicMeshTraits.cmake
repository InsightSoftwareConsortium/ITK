WRAP_CLASS("itk::DefaultDynamicMeshTraits")
  FOREACH(d ${WRAP_ITK_DIMS})
  WRAP_TEMPLATE("${ITKM_D}${d}${d}${ITKM_D}${ITKM_D}" "${ITKT_D},${d},${d},${ITKT_D},${ITKT_D}")
  WRAP_TEMPLATE("${ITKM_D}${d}${d}${ITKM_D}" "${ITKT_D},${d},${d},${ITKT_D}")
  ENDFOREACH(d)  


END_WRAP_CLASS()

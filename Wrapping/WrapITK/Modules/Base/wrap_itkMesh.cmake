WRAP_INCLUDE("itkDefaultStaticMeshTraits.h")
WRAP_INCLUDE("itkDefaultDynamicMeshTraits.h")

WRAP_CLASS("itk::Mesh" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_D}${d}S"
      "${ITKT_D},${d},itk::DefaultStaticMeshTraits< ${ITKT_D},${d},${d},${ITKT_D} >")
    WRAP_TEMPLATE("${ITKM_D}${d}D"
      "${ITKT_D},${d},itk::DefaultDynamicMeshTraits< ${ITKT_D},${d},${d},${ITKT_D} >")
  ENDFOREACH(d)
END_WRAP_CLASS()

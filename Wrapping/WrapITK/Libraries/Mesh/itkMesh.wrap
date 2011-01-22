WRAP_INCLUDE("itkDefaultStaticMeshTraits.h")
WRAP_INCLUDE("itkDefaultDynamicMeshTraits.h")
WRAP_INCLUDE("itkQuadEdgeMeshTraits.h")

WRAP_CLASS("itk::Mesh" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
#    WRAP_TEMPLATE("${ITKM_D}${d}S"
#      "${ITKT_D},${d},itk::DefaultStaticMeshTraits< ${ITKT_D},${d},${d},${ITKT_D} >")
#    WRAP_TEMPLATE("${ITKM_D}${d}D"
#      "${ITKT_D},${d},itk::DefaultDynamicMeshTraits< ${ITKT_D},${d},${d},${ITKT_D} >")
    WRAP_TEMPLATE("${ITKM_D}${d}Q"
      "${ITKT_D},${d},itk::QuadEdgeMeshTraits< ${ITKT_D},${d},${ITKT_B},${ITKT_B},${ITKT_F},${ITKT_F} >")
  endforeach(d)
END_WRAP_CLASS()

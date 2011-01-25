WRAP_INCLUDE("itkVectorContainer.h")
WRAP_INCLUDE("itkMapContainer.h")

WRAP_CLASS("itk::BoundingBox" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_UL}${d}${ITKM_D}VC${ITKM_UL}${ITKM_PD${d}}" "${ITKT_UL},${d},${ITKT_D}, itk::VectorContainer< ${ITKT_UL}, ${ITKT_PD${d}} >")
    WRAP_TEMPLATE("${ITKM_UL}${d}${ITKM_D}MC${ITKM_UL}${ITKM_PD${d}}" "${ITKT_UL},${d},${ITKT_D}, itk::MapContainer< ${ITKT_UL}, ${ITKT_PD${d}} >")
  endforeach(d)
END_WRAP_CLASS()

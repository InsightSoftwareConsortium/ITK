WRAP_INCLUDE("itkDefaultStaticMeshTraits.h")

WRAP_CLASS("itk::MeshToVTKPolyData" POINTER)
  FILTER_DIMS(d3 3)
  foreach(d ${d3})
    WRAP_TEMPLATE("M${ITKM_D}${d}S" "itk::Mesh< ${ITKT_D},${d},itk::DefaultStaticMeshTraits< ${ITKT_D},${d},${d},${ITKT_D} > >")
  endforeach(d)
END_WRAP_CLASS()

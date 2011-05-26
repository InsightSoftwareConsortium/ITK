# this code supposes that the dimension is 3
# ${ITKM_I${t}3}  note the '3' after ${t}
FILTER_DIMS(d3 3)
if(d3)

  WRAP_INCLUDE("itkMesh.h")
  WRAP_INCLUDE("itkQuadEdgeMeshTraits.h")

  WRAP_CLASS("itk::ConformalFlatteningMeshFilter" POINTER)
    WRAP_TEMPLATE("MD3QMD3Q"
      "itk::Mesh< ${ITKT_D},3,itk::QuadEdgeMeshTraits< ${ITKT_D},3,${ITKT_B},${ITKT_B},${ITKT_F},${ITKT_F} > >, itk::Mesh< ${ITKT_D},3,itk::QuadEdgeMeshTraits< ${ITKT_D},3,${ITKT_B},${ITKT_B},${ITKT_F},${ITKT_F} > >")
  END_WRAP_CLASS()

endif(d3)

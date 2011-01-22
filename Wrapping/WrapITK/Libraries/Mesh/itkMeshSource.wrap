WRAP_INCLUDE("itkMesh.h")
WRAP_INCLUDE("itkQuadEdgeMeshTraits.h")

WRAP_CLASS("itk::MeshSource" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("MD${d}Q" "itk::Mesh< ${ITKT_D},${d},itk::QuadEdgeMeshTraits< ${ITKT_D},${d},${ITKT_B},${ITKT_B},${ITKT_F},${ITKT_F} > >")
  endforeach(d)
END_WRAP_CLASS()

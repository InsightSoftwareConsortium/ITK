WRAP_INCLUDE("itkQuadEdgeCellTraitsInfo.h")
WRAP_INCLUDE("itkCellInterface.h")

WRAP_CLASS("itk::QuadEdgeMeshLineCell" AUTOPOINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("CIDQEMCTIF${d}" "itk::CellInterface< double, itk::QuadEdgeMeshCellTraitsInfo< ${d} > >")
  endforeach(d)
END_WRAP_CLASS()

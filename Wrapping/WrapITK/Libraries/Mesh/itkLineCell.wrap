WRAP_INCLUDE("itkQuadEdgeCellTraitsInfo.h")
WRAP_INCLUDE("itkCellInterface.h")

WRAP_CLASS("itk::LineCell" AUTOPOINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("CIDQEMCTI${d}" "itk::CellInterface< double, itk::QuadEdgeMeshCellTraitsInfo< ${d} > >")
  endforeach(d)
END_WRAP_CLASS()

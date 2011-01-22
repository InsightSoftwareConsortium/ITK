WRAP_INCLUDE("itkQuadEdgeCellTraitsInfo.h")

WRAP_CLASS("itk::CellInterface" AUTOPOINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("DQEMCTI${d}" "double, itk::QuadEdgeMeshCellTraitsInfo< ${d} >")
  endforeach(d)
END_WRAP_CLASS()

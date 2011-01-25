# this class is define in a header with a different name
WRAP_INCLUDE("itkQuadEdgeCellTraitsInfo.h")
set(WRAPPER_AUTO_INCLUDE_HEADERS OFF)

WRAP_CLASS("itk::QuadEdgeMeshCellTraitsInfo")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${d}" "${d}")
  endforeach(d)
END_WRAP_CLASS()

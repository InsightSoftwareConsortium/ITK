# for the time being this filter is only wrapped for 3D
FILTER_DIMS(d3 3)
if(d3)
  WRAP_CLASS("itk::QuadEdgeMesh" POINTER)
    foreach(d 3)
      WRAP_TEMPLATE("${ITKM_D}${d}S" "${ITKT_D},${d}")
    endforeach(d)
  END_WRAP_CLASS()
endif(d3)

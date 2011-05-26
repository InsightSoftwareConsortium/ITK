WRAP_CLASS("itk::GeodesicActiveContourLevelSetImageFilter" POINTER)

  # apply a filter to the dimensions... the code will not compile for dim<=1
  FILTER_DIMS(ds 2+)

  foreach(d ${ds})
    foreach(t ${WRAP_ITK_REAL})
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_I${t}${d}}${ITKM_${t}}" "${ITKT_I${t}${d}},${ITKT_I${t}${d}},${ITKT_${t}}")
    endforeach(t)
  endforeach(d)

END_WRAP_CLASS()

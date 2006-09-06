FILTER_DIMS(d2 2)
IF(d2)

ENDIF(d2)



FILTER_DIMS(d3 3)
IF(d3)
  WRAP_CLASS("itk::Rigid3DPerspectiveTransform" POINTER)
    WRAP_TEMPLATE("${ITKM_D}" "${ITKT_D}")
  END_WRAP_CLASS()
ENDIF(d3)


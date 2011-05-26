
WRAP_INCLUDE("itkShapeDetectionLevelSetImageFilter.h")

WRAP_CLASS("itk::VectorThresholdSegmentationLevelSetImageFilter" POINTER)

  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${WRAP_ITK_REAL})
      foreach(t2 ${WRAP_ITK_VECTOR_REAL})
        WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_I${t2}${d}${d}}${ITKM_${t}}" "${ITKT_I${t}${d}},${ITKT_I${t2}${d}${d}},${ITKT_${t}}")
      endforeach(t2)
    endforeach(t)
  endforeach(d)

END_WRAP_CLASS()

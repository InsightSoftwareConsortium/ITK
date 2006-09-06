WRAP_CLASS("itk::NarrowBandThresholdSegmentationLevelSetImageFilter" POINTER)

  # WRAP_IMAGE_FILTER_USIGN_INT(2)
  # WRAP_IMAGE_FILTER_SIGN_INT(2)
  # WRAP_IMAGE_FILTER_REAL(2)
  
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${WRAP_ITK_REAL})
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_I${t}${d}}${ITKM_${t}}" "${ITKT_I${t}${d}},${ITKT_I${t}${d}},${ITKT_${t}}")
    ENDFOREACH(t)
  ENDFOREACH(d)

END_WRAP_CLASS()

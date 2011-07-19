WRAP_CLASS("itk::NarrowBandThresholdSegmentationLevelSetImageFilter" POINTER)

  # WRAP_IMAGE_FILTER_USIGN_INT(2)
  # WRAP_IMAGE_FILTER_SIGN_INT(2)
  # WRAP_IMAGE_FILTER_REAL(2)

  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${WRAP_ITK_REAL})
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_I${t}${d}}${ITKM_${t}}" "${ITKT_I${t}${d}},${ITKT_I${t}${d}},${ITKT_${t}}")
    endforeach(t)
  endforeach(d)

END_WRAP_CLASS()

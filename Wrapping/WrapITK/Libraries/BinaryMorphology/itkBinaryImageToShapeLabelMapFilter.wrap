WRAP_CLASS("itk::BinaryImageToShapeLabelMapFilter" POINTER)
  foreach(t ${WRAP_ITK_USIGN_INT})
    foreach(d ${WRAP_ITK_DIMS})
      # image -> label collection image
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_LM${d}}" "${ITKT_I${t}${d}}, ${ITKT_LM${d}}")
    endforeach(d)
  endforeach(t)
END_WRAP_CLASS()

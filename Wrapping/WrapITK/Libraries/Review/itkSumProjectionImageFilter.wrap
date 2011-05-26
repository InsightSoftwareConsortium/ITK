WRAP_CLASS("itk::SumProjectionImageFilter" POINTER_WITH_SUPERCLASS)
  WRAP_IMAGE_FILTER_SCALAR(2)

  # reduce the output dimension by 1, if possible
  foreach(t ${WRAP_ITK_SCALAR})
    foreach(d ${WRAP_ITK_DIMS})
      DECREMENT(d1 ${d})
      FILTER_DIMS(d2 "${d1}")
      if(d2)
        WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_I${t}${d2}}" "${ITKT_I${t}${d}},${ITKT_I${t}${d2}}")
      endif(d2)
    endforeach(d)
  endforeach(t)
END_WRAP_CLASS()

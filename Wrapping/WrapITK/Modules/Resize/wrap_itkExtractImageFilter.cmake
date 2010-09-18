WRAP_CLASS("itk::ExtractImageFilter" POINTER)

  WRAP_IMAGE_FILTER_USIGN_INT(2)
  WRAP_IMAGE_FILTER_SIGN_INT(2)
  WRAP_IMAGE_FILTER_REAL(2)

  foreach(d1 ${WRAP_ITK_DIMS})
    foreach(d2 ${WRAP_ITK_DIMS})
      if("${d1}" GREATER "${d2}")
        foreach(t ${WRAP_ITK_SCALAR})
          WRAP_TEMPLATE("${ITKM_I${t}${d1}}${ITKM_I${t}${d2}}"
                        "${ITKT_I${t}${d1}},${ITKT_I${t}${d2}}")
        endforeach(t)
      endif("${d1}" GREATER "${d2}")
    endforeach(d2)
  endforeach(d1)

END_WRAP_CLASS()


WRAP_CLASS("itk::ExtractImageFilter" POINTER)

  WRAP_IMAGE_FILTER_USIGN_INT(2)
  WRAP_IMAGE_FILTER_SIGN_INT(2)
  WRAP_IMAGE_FILTER_REAL(2)
  
  FOREACH(d1 ${WRAP_ITK_DIMS})
    FOREACH(d2 ${WRAP_ITK_DIMS})
      IF("${d1}" GREATER "${d2}")
        FOREACH(t ${WRAP_ITK_SCALAR})
          WRAP_TEMPLATE("${ITKM_I${t}${d1}}${ITKM_I${t}${d2}}"
                        "${ITKT_I${t}${d1}},${ITKT_I${t}${d2}}")
        ENDFOREACH(t)
      ENDIF("${d1}" GREATER "${d2}")
    ENDFOREACH(d2)
  ENDFOREACH(d1)

END_WRAP_CLASS()


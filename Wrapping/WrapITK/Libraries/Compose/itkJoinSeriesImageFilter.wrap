WRAP_CLASS("itk::JoinSeriesImageFilter" POINTER)
  foreach(d1 ${WRAP_ITK_DIMS})
    foreach(d2 ${WRAP_ITK_DIMS})
      if("${d1}" LESS "${d2}")
        foreach(t ${WRAP_ITK_SCALAR})
          WRAP_TEMPLATE("${ITKM_I${t}${d1}}${ITKM_I${t}${d2}}"
                        "${ITKT_I${t}${d1}},${ITKT_I${t}${d2}}")
        endforeach(t)
      endif("${d1}" LESS "${d2}")
    endforeach(d2)
  endforeach(d1)

END_WRAP_CLASS()

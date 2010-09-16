WRAP_CLASS("itk::ImportImageContainer" POINTER)

  foreach(d ${EXPLICIT_ITK_INTEGRAL})
    foreach(t ${EXPLICIT_ITK_SCALAR})
     WRAP_TEMPLATE("${ITKM_${d}}${ITKM_${t}}" "${ITKT_${d}},${ITKT_${t}}")
    endforeach(t)
    foreach(t ${EXPLICIT_ITK_RGB})
     WRAP_TEMPLATE("${ITKM_${d}}${ITKM_${t}}" "${ITKT_${d}},${ITKT_${t}}")
    endforeach(t)

    foreach(i ${EXPLICIT_ITK_DIMS})
      foreach(t ${EXPLICIT_ITK_VECTOR})
        WRAP_TEMPLATE("${ITKM_${d}}${ITKM_${t}${i}}" "${ITKT_${d}},${ITKT_${t}${i}}")
      endforeach(t)
    endforeach(i)
  endforeach(d)

 foreach(i ${EXPLICIT_ITK_INTEGRAL})

 endforeach(i)
END_WRAP_CLASS()


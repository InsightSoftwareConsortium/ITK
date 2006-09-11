WRAP_CLASS("itk::ImportImageContainer" POINTER)

  FOREACH(d ${EXPLICIT_ITK_INTEGRAL})
    FOREACH(t ${EXPLICIT_ITK_SCALAR})
     WRAP_TEMPLATE("${ITKM_${d}}${ITKM_${t}}" "${ITKT_${d}},${ITKT_${t}}")
    ENDFOREACH(t) 
    FOREACH(t ${EXPLICIT_ITK_RGB})
     WRAP_TEMPLATE("${ITKM_${d}}${ITKM_${t}}" "${ITKT_${d}},${ITKT_${t}}")
    ENDFOREACH(t)
    
    FOREACH(i ${EXPLICIT_ITK_DIMS})
      FOREACH(t ${EXPLICIT_ITK_VECTOR})
        WRAP_TEMPLATE("${ITKM_${d}}${ITKM_${t}${i}}" "${ITKT_${d}},${ITKT_${t}${i}}")
      ENDFOREACH(t)
    ENDFOREACH(i)
  ENDFOREACH(d)

 FOREACH(i ${EXPLICIT_ITK_INTEGRAL})
 
 ENDFOREACH(i)
END_WRAP_CLASS()


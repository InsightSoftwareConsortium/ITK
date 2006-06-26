WRAP_CLASS("itk::ImportImageContainer" POINTER)

  FOREACH(d ${WRAP_ITK_INTEGRAL})
    FOREACH(t ${WRAP_ITK_SCALAR})
     WRAP_TEMPLATE("${ITKM_${d}}${ITKM_${t}}" "${ITKT_${d}},${ITKT_${t}}")
    ENDFOREACH(t) 
    FOREACH(t ${WRAP_ITK_RGB})
     WRAP_TEMPLATE("${ITKM_${d}}${ITKM_${t}}" "${ITKT_${d}},${ITKT_${t}}")
    ENDFOREACH(t)
    
    FOREACH(i ${WRAP_ITK_DIMS})
      FOREACH(t ${WRAP_ITK_VECTOR})
        WRAP_TEMPLATE("${ITKM_${d}}${ITKM_${t}${i}}" "${ITKT_${d}},${ITKT_${t}${i}}")
      ENDFOREACH(t)
    ENDFOREACH(i)
  ENDFOREACH(d)

 FOREACH(i ${WRAP_ITK_INTEGRAL})
 
 ENDFOREACH(i)
END_WRAP_CLASS()


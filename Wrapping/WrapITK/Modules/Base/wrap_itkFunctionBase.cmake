WRAP_CLASS("itk::FunctionBase" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
   
     IF(WRAP_float)
      WRAP_TEMPLATE("${ITKM_IF${d}}${ITKM_D}" "${ITKT_IF${d}},${ITKT_D}")    
    ENDIF(WRAP_float)
    IF(WRAP_unsigned_short)
      WRAP_TEMPLATE("${ITKM_IUS${d}}${ITKM_D}" "${ITKT_IUS${d}},${ITKT_D}")    
    ENDIF(WRAP_unsigned_short)

    WRAP_TEMPLATE("${ITKM_PF${d}}${ITKM_D}"   "${ITKT_PF${d}} ,${ITKT_D}")
    WRAP_TEMPLATE("${ITKM_PD${d}}${ITKM_D}"   "${ITKT_PD${d}} ,${ITKT_D}")
    WRAP_TEMPLATE("${ITKM_CIF${d}}${ITKM_AD}" "${ITKT_CIF${d}} ,${ITKT_AD}")
    WRAP_TEMPLATE("${ITKM_CID${d}}${ITKM_AD}" "${ITKT_CID${d}} ,${ITKT_AD}")
  ENDFOREACH(d)

  WRAP_TEMPLATE("${ITKM_D}${ITKM_D}"     "${ITKT_D},${ITKT_D}")
END_WRAP_CLASS()

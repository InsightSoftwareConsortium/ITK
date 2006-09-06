
SET(WRAPPER_AUTO_INCLUDE_HEADERS OFF)
WRAP_INCLUDE("itkNarrowBand.h")

WRAP_CLASS("itk::BandNode")
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("I${d}${ITKM_${t}}"  "itk::Index<${d}>, ${ITKT_${t}}")    
    ENDFOREACH(t)
  ENDFOREACH(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::NarrowBand" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("BNI${d}${ITKM_${t}}"  "itk::BandNode< itk::Index<${d}>, ${ITKT_${t}}>")    
    ENDFOREACH(t)
  ENDFOREACH(d)
END_WRAP_CLASS()

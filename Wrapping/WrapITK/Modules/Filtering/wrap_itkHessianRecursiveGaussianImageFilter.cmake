
WRAP_CLASS("itk::SymmetricSecondRankTensor")
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_D}${d}" "${ITKT_D}, ${d}")
  ENDFOREACH(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::Image" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("SSRT${ITKM_D}${d}${d}" "itk::SymmetricSecondRankTensor< ${ITKT_D}, ${d} >, ${d}")
  ENDFOREACH(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::ImageSource" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("ISSRT${ITKM_D}${d}${d}" "itk::Image< itk::SymmetricSecondRankTensor< ${ITKT_D}, ${d} >, ${d} >")
  ENDFOREACH(d)
END_WRAP_CLASS()
  
WRAP_CLASS("itk::ImageToImageFilter" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_I${t}${d}}ISSRT${ITKM_D}${d}${d}" "${ITKT_I${t}${d}}, itk::Image< itk::SymmetricSecondRankTensor< ${ITKT_D}, ${d} >, ${d} >")
      WRAP_TEMPLATE("ISSRT${ITKM_D}${d}${d}${ITKM_I${t}${d}}" "itk::Image< itk::SymmetricSecondRankTensor< ${ITKT_D}, ${d} >, ${d} >, ${ITKT_I${t}${d}}")
    ENDFOREACH(t)
  ENDFOREACH(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::HessianRecursiveGaussianImageFilter" POINTER)
  WRAP_IMAGE_FILTER_SCALAR(1)
END_WRAP_CLASS()
      

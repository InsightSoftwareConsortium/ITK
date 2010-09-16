
WRAP_CLASS("itk::SymmetricSecondRankTensor")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_D}${d}" "${ITKT_D}, ${d}")
  endforeach(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::Image" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("SSRT${ITKM_D}${d}${d}" "itk::SymmetricSecondRankTensor< ${ITKT_D}, ${d} >, ${d}")
  endforeach(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::ImageSource" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("ISSRT${ITKM_D}${d}${d}" "itk::Image< itk::SymmetricSecondRankTensor< ${ITKT_D}, ${d} >, ${d} >")
  endforeach(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::ImageToImageFilter" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_I${t}${d}}ISSRT${ITKM_D}${d}${d}" "${ITKT_I${t}${d}}, itk::Image< itk::SymmetricSecondRankTensor< ${ITKT_D}, ${d} >, ${d} >")
      WRAP_TEMPLATE("ISSRT${ITKM_D}${d}${d}${ITKM_I${t}${d}}" "itk::Image< itk::SymmetricSecondRankTensor< ${ITKT_D}, ${d} >, ${d} >, ${ITKT_I${t}${d}}")
    endforeach(t)
  endforeach(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::HessianRecursiveGaussianImageFilter" POINTER)
  WRAP_IMAGE_FILTER_SCALAR(1)
END_WRAP_CLASS()


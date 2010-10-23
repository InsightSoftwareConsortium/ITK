WRAP_CLASS("itk::ImageToImageFilter" POINTER)
  # complex types
  WRAP_IMAGE_FILTER_COMPLEX_REAL(2)

  # scalar <-> RGB
  UNIQUE(stypes "UL;${WRAP_ITK_SCALAR}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${stypes}" "${WRAP_ITK_RGB}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_RGB}" "${stypes}")

  # scalar <-> RGBA
  UNIQUE(stypes "UL;${WRAP_ITK_SCALAR}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${stypes}" "${WRAP_ITK_RGBA}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_RGBA}" "${stypes}")

  # VectorImage <-> scalar
  UNIQUE(to_types "UC;${WRAP_ITK_SCALAR}")
    foreach(d ${WRAP_ITK_DIMS})
      foreach(t ${to_types})
        foreach(t2 ${to_types})
          WRAP_TEMPLATE("${ITKM_VI${t}${d}}${ITKM_I${t2}${d}}" "${ITKT_VI${t}${d}},${ITKT_I${t2}${d}}")
          WRAP_TEMPLATE("${ITKM_I${t2}${d}}${ITKM_VI${t}${d}}" "${ITKT_I${t2}${d}},${ITKT_VI${t}${d}}")
        endforeach(t2)
      endforeach(t)
    endforeach(d)

  # *Vector <-> scalar
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_VECTOR}" "${WRAP_ITK_SCALAR}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_SCALAR}" "${WRAP_ITK_VECTOR}")

  # complex <-> scalar
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_REAL}" "${WRAP_ITK_COMPLEX_REAL}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_COMPLEX_REAL}" "${WRAP_ITK_REAL}")

  # scalar <-> SymmetricSecondRankTensor
  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_ISSRT${ITKM_D}${d}${d}}" "${ITKT_I${t}${d}}, ${ITKT_ISSRT${ITKM_D}${d}${d}}")
      WRAP_TEMPLATE("${ITKM_ISSRT${ITKM_D}${d}${d}}${ITKM_I${t}${d}}" "${ITKT_ISSRT${ITKM_D}${d}${d}}, ${ITKT_I${t}${d}}")
    endforeach(t)
  endforeach(d)

  # Wrap dim=3 -> dim=2, dim=3 -> dim=2, etc.
  foreach(d ${WRAP_ITK_DIMS})
    foreach(d2 ${WRAP_ITK_DIMS})
      if(NOT "${d}" EQUAL "${d2}") # this was already taken care of elsewhere
        foreach(t ${WRAP_ITK_SCALAR} ${WRAP_ITK_RGB} ${WRAP_ITK_COMPLEX_REAL})
          WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_I${t}${d2}}"
                        "${ITKT_I${t}${d}},${ITKT_I${t}${d2}}")
        endforeach(t)
      endif(NOT "${d}" EQUAL "${d2}")
    endforeach(d2)
  endforeach(d)

END_WRAP_CLASS()

WRAP_CLASS("itk::InPlaceImageFilter" POINTER)
  # VectorImage <-> scalar
  UNIQUE(to_types "UC;${WRAP_ITK_SCALAR}")
  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${to_types})
      WRAP_TEMPLATE("${ITKM_VI${t}${d}}${ITKM_I${t}${d}}" "${ITKT_VI${t}${d}},${ITKT_I${t}${d}}")
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_VI${t}${d}}" "${ITKT_I${t}${d}},${ITKT_VI${t}${d}}")
    endforeach(t)
  endforeach(d)

  # *Vector <-> scalar
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_VECTOR}" "${WRAP_ITK_SCALAR}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_SCALAR}" "${WRAP_ITK_VECTOR}")

  # complex types
  WRAP_IMAGE_FILTER_COMPLEX_REAL(2)

  # complex <-> scalar
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_REAL}" "${WRAP_ITK_COMPLEX_REAL}")
  WRAP_IMAGE_FILTER_COMBINATIONS("${WRAP_ITK_COMPLEX_REAL}" "${WRAP_ITK_REAL}")

END_WRAP_CLASS()

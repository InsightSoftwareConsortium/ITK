WRAP_CLASS("itk::ImageFunction" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    # UC is required for InterpolateImageFunction
    UNIQUE(types "${WRAP_ITK_SCALAR};UC")
    foreach(t ${types})
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_D}${ITKM_F}"  "${ITKT_I${t}${d}},${ITKT_D},${ITKT_F}")
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_D}${ITKM_D}"  "${ITKT_I${t}${d}},${ITKT_D},${ITKT_D}")
    endforeach(t)

    foreach(t ${WRAP_ITK_VECTOR})
      WRAP_TEMPLATE("${ITKM_I${t}${d}${d}}${ITKM_VD${d}}${ITKM_D}" "${ITKT_I${t}${d}${d}},${ITKT_VD${d}},${ITKT_D}")
    endforeach(t)
  endforeach(d)
END_WRAP_CLASS()

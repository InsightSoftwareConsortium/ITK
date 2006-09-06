WRAP_CLASS("itk::Neighborhood")

  # force bool, for structuring element
  UNIQUE(scalar_types "${WRAP_ITK_SCALAR};${WRAP_ITK_RGB};B")

  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${scalar_types})
      WRAP_TEMPLATE("${ITKM_${t}}${d}" "${ITKT_${t}},${d}")   
    ENDFOREACH(t)

    FOREACH(t ${WRAP_ITK_VECTOR})
      WRAP_TEMPLATE("${ITKM_${t}${d}}${d}" "${ITKT_${t}${d}},${d}")   
    ENDFOREACH(t)
  ENDFOREACH(d)
END_WRAP_CLASS()

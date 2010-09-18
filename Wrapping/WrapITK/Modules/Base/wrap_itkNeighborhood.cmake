WRAP_CLASS("itk::Neighborhood")

  # force bool, for structuring element
  UNIQUE(scalar_types "${WRAP_ITK_SCALAR};${WRAP_ITK_RGB};B")

  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${scalar_types})
      WRAP_TEMPLATE("${ITKM_${t}}${d}" "${ITKT_${t}},${d}")
    endforeach(t)

    foreach(t ${WRAP_ITK_VECTOR})
      WRAP_TEMPLATE("${ITKM_${t}${d}}${d}" "${ITKT_${t}${d}},${d}")
    endforeach(t)
  endforeach(d)
END_WRAP_CLASS()

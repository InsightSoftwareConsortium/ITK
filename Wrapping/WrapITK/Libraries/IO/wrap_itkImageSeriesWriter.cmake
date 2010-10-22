WRAP_CLASS("itk::ImageSeriesWriter" POINTER)
  # Force uchar image IO
  UNIQUE(image_types "UC;${WRAP_ITK_SCALAR};${WRAP_ITK_RGB}")
  foreach(d1 ${WRAP_ITK_DIMS})
    foreach(d2 ${WRAP_ITK_DIMS})
      if("${d1}" GREATER "${d2}")
        foreach(t ${image_types})
          WRAP_TEMPLATE("${ITKM_I${t}${d1}}${ITKM_I${t}${d2}}"
                        "${ITKT_I${t}${d1}},${ITKT_I${t}${d2}}")
        endforeach(t)
      endif("${d1}" GREATER "${d2}")
    endforeach(d2)
  endforeach(d1)
END_WRAP_CLASS()

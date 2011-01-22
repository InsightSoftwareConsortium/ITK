WRAP_CLASS("itk::ExtractImageFilter" POINTER)

  WRAP_IMAGE_FILTER_ALL_TYPES(2)

  foreach(d1 ${WRAP_ITK_DIMS})
    foreach(d2 ${WRAP_ITK_DIMS})
      if("${d1}" GREATER "${d2}")
        foreach(t ${WRAP_ITK_SCALAR} ${WRAP_ITK_RGB} ${WRAP_ITK_COMPLEX_REAL})
          WRAP_TEMPLATE("${ITKM_I${t}${d1}}${ITKM_I${t}${d2}}"
                        "${ITKT_I${t}${d1}},${ITKT_I${t}${d2}}")
        endforeach(t)
        # vector types requires to have the same dimension in the image and in the vector and so can't be
        # extracted in an image of lower dimension
        # foreach(t ${WRAP_ITK_VECTOR})
        #  WRAP_TEMPLATE("${ITKM_I${t}${d1}${d1}}${ITKM_I${t}${d2}${d2}}"
        #                "${ITKT_I${t}${d1}${d1}},${ITKT_I${t}${d2}${d2}}")
        # endforeach(t)
      endif("${d1}" GREATER "${d2}")
    endforeach(d2)
  endforeach(d1)

END_WRAP_CLASS()

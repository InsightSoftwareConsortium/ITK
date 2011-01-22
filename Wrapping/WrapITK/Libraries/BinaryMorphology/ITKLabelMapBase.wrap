WRAP_CLASS("itk::ImageSource" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_LM${d}}" "${ITKT_LM${d}}")
  endforeach(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::ImageToImageFilter" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    # label collection -> label collection
    WRAP_TEMPLATE("${ITKM_LM${d}}${ITKM_LM${d}}" "${ITKT_LM${d}}, ${ITKT_LM${d}}")

    foreach(t ${WRAP_ITK_SCALAR})
      # image -> label collection image
      WRAP_TEMPLATE("${ITKM_I${t}${d}}${ITKM_LM${d}}" "${ITKT_I${t}${d}}, ${ITKT_LM${d}}")
      # label collection -> image
      WRAP_TEMPLATE("${ITKM_LM${d}}${ITKM_I${t}${d}}" "${ITKT_LM${d}}, ${ITKT_I${t}${d}}")
    endforeach(t)

    # label map -> rgb image
    if(WRAP_rgb_unsigned_char)
      WRAP_TEMPLATE("${ITKM_LM${d}}${ITKM_IRGBUC${d}}" "${ITKT_LM${d}}, ${ITKT_IRGBUC${d}}")
    endif(WRAP_rgb_unsigned_char)

    if(WRAP_rgb_unsigned_short)
      WRAP_TEMPLATE("${ITKM_LM${d}}${ITKM_IRGBUS${d}}" "${ITKT_LM${d}}, ${ITKT_IRGBUS${d}}")
    endif(WRAP_rgb_unsigned_short)

  endforeach(d)
END_WRAP_CLASS()

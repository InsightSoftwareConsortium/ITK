WRAP_CLASS("itk::LabelMapContourOverlayImageFilter" POINTER)

  foreach(d ${WRAP_ITK_DIMS})

    if(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)
      WRAP_TEMPLATE("${ITKM_LM${d}}${ITKM_IUC${d}}${ITKM_IRGBUC${d}}" "${ITKT_LM${d}}, ${ITKT_IUC${d}}, ${ITKT_IRGBUC${d}}")
    endif(WRAP_rgb_unsigned_char AND WRAP_unsigned_char)

    if(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)
      WRAP_TEMPLATE("${ITKM_LM${d}}${ITKM_IUS${d}}${ITKM_IRGBUS${d}}" "${ITKT_LM${d}}, ${ITKT_IUS${d}}, ${ITKT_IRGBUS${d}}")
    endif(WRAP_rgb_unsigned_short AND WRAP_unsigned_short)

  endforeach(d)

END_WRAP_CLASS()

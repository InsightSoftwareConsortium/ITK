WRAP_CLASS("itk::ImageSpatialObject" POINTER)
  # unsigned char required for the ImageMaskSpatialObject
  UNIQUE(types "UC;${WRAP_ITK_SCALAR}")

  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${types})
      WRAP_TEMPLATE("${d}${ITKM_${t}}" "${d},${ITKT_${t}}")
    endforeach(t)
  endforeach(d)
END_WRAP_CLASS()

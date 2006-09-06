WRAP_CLASS("itk::ImageSpatialObject" POINTER)
  # unsigned char required for the ImageMaskSpatialObject
  UNIQUE(types "UC;${WRAP_ITK_SCALAR}")
  
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(t ${types})
      WRAP_TEMPLATE("${d}${ITKM_${t}}" "${d},${ITKT_${t}}")
    ENDFOREACH(t)
  ENDFOREACH(d)
END_WRAP_CLASS()

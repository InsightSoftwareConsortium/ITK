WRAP_CLASS("itk::ImageFileReader" POINTER)
  # Force uchar image IO 
  UNIQUE(image_types "UC;${WRAP_ITK_ALL_TYPES}")
  WRAP_IMAGE_FILTER("${image_types}" 1)

  UNIQUE(to_types "${WRAP_ITK_SCALAR};UC")
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(type ${to_types})
      WRAP_TEMPLATE("${ITKM_VI${type}${d}}"  "${ITKT_VI${type}${d}}")
    ENDFOREACH(type)
  ENDFOREACH(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::ImageSource" POINTER)
  # Force uchar and ulong image sources for saving in 8 bits and watershed filter
  UNIQUE(image_types "UC;RGBUC;RGBAUC;UL;${WRAP_ITK_ALL_TYPES}")
  WRAP_IMAGE_FILTER("${image_types}" 1)

  UNIQUE(to_types "${WRAP_ITK_SCALAR};UC")
  foreach(d ${WRAP_ITK_DIMS})
    foreach(type ${to_types})
      WRAP_TEMPLATE("${ITKM_VI${type}${d}}"  "${ITKT_VI${type}${d}}")
    endforeach(type)
    WRAP_TEMPLATE("${ITKM_ISSRT${ITKM_D}${d}${d}}"  "${ITKT_ISSRT${ITKM_D}${d}${d}}")
  endforeach(d)
END_WRAP_CLASS()

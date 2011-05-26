WRAP_CLASS("itk::ImageRegion")
  foreach(d ${EXPLICIT_ITK_DIMS})
    WRAP_TEMPLATE("${d}"  "${d}")
  endforeach(d)
END_WRAP_CLASS()


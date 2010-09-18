WRAP_CLASS("itk::ImageBase" POINTER)
  foreach(d ${EXPLICIT_ITK_DIMS})
    WRAP_TEMPLATE("${d}"  "${d}")
  endforeach(d)
END_WRAP_CLASS()


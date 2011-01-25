WRAP_CLASS("itk::ImageBase" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${d}"  "${d}")
  endforeach(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::Image" POINTER)
  set(WRAPPER_TEMPLATES "${itk_Wrap_Image}")
END_WRAP_CLASS()

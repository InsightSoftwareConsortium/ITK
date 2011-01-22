WRAP_CLASS("itk::DTITubeSpatialObjectPoint")
#  foreach(d ${WRAP_ITK_DIMS})
#    WRAP_TEMPLATE("${d}" "${d}")
#  endforeach(d)

  # seems to be usable only with dim=3
  FILTER_DIMS(d 3)
  if(d)
    WRAP_TEMPLATE(3 3)
  endif(d)

END_WRAP_CLASS()

WRAP_CLASS("itk::SpatialObject" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${d}" "${d}")
  ENDFOREACH(d)
END_WRAP_CLASS()

# the file must have a different name than the class name for java
# that's why it is here

# only available for dim 3

FILTER_DIMS(d 3)
IF(d)
  WRAP_NON_TEMPLATE_CLASS("itk::CylinderSpatialObject" POINTER)
ENDIF(d)

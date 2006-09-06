# the file must have a different name than the class name for java
# that's why ITK is uppercase in the file name

# only available for dim 3

FILTER_DIMS(d 3)
IF(d)
  WRAP_NON_TEMPLATE_CLASS("itk::CylinderSpatialObject" POINTER)
ENDIF(d)

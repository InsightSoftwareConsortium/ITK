WRAP_NON_TEMPLATE_CLASS("itk::Region")
WRAP_NON_TEMPLATE_CLASS("itk::MeshRegion")

WRAP_CLASS("itk::ImageRegion")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE(${d} ${d})
  endforeach(d)
END_WRAP_CLASS()

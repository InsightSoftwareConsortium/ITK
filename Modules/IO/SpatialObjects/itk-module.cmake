set(DOCUMENTATION "This module contains classes for reading and writing
SpatialObjects as opposed to general images.  The SpatialObjects are written in
<a
href=\"http://www.vtk.org/Wiki/MetaIO/Documentation#Spatial_Objects\">MetaIO</a>
format.")


itk_module(ITKIOSpatialObjects
  ENABLE_SHARED
  DEPENDS
    ITKSpatialObjects
    ITKIOXML
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)

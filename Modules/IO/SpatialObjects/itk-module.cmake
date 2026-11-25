set(
  DOCUMENTATION
  "This module contains classes for reading and writing
SpatialObjects as opposed to general images.  The SpatialObjects are written in
<a
href=\"https://www.vtk.org/Wiki/MetaIO/Documentation#Spatial_Objects\">MetaIO</a>
format."
)

itk_module(
  ITKIOSpatialObjects
  DEPENDS
    ITKSpatialObjects
    ITKIOXML
  COMPILE_DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
    ITKSpatialObjects
    ITKIOXML
    ITKMetaIO
  DESCRIPTION "${DOCUMENTATION}"
)

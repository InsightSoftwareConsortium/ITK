SUBDIRS(Numerics Common BasicFilters Algorithms)

LINK_LIBRARIES (
VXLNumerics
)

CABLE_DEFINE_SET(ScalarType "unsigned short")
CABLE_DEFINE_SET(Dimension 2)

CABLE_OPEN_NAMESPACE(itk)
  CABLE_DEFINE_SET(PointType "Point<$ScalarType, $Dimension>"
                   SOURCE_FILES itkPoint)

  CABLE_DEFINE_SET(VectorType "Vector<$ScalarType, $Dimension>"
                   SOURCE_FILES itkVector)

  CABLE_DEFINE_SET(PixelType $ScalarType)

  CABLE_DEFINE_SET(MeshType "Mesh<$PixelType>"
                   SOURCE_FILES itkMesh)

  CABLE_DEFINE_SET(ImageType "Image<$PixelType, $Dimension>"
                   SOURCE_FILES itkImage)

CABLE_CLOSE_NAMESPACE(itk)


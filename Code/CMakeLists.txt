SUBDIRS(Numerics Common BasicFilters Algorithms)

LINK_LIBRARIES (
VXLNumerics
)

# All the CABLE sets defined here will be inherited into the
# subdirectories for convenient use in their CMakeLists files.

# For now, we just want one combination of scalar type and dimension
# to be used in generating instantiations.  Pick a common value for
# each ("unsigned short" and 2, respectively).
CABLE_DEFINE_SET(ScalarType "unsigned short")
CABLE_DEFINE_SET(Dimension 2)

# Open the "itk" namespace.  It must be closed before the end of
# the CMakeLists file.
CABLE_OPEN_NAMESPACE(itk)

  # Define a few specializations of the "Point" class.  We need the
  # "itkPoint" source file to use it, so list it after the
  # SOURCE_FILES keyword.
  CABLE_DEFINE_SET(PointType "Point<$ScalarType, $Dimension>"
                   SOURCE_FILES itkPoint)

  CABLE_DEFINE_SET(VectorType "Vector<$ScalarType, $Dimension>"
                   SOURCE_FILES itkVector vnl/vnl_vector vnl/vnl_c_vector)

  CABLE_DEFINE_SET(PixelType $ScalarType)

  CABLE_DEFINE_SET(MeshType "Mesh<$PixelType>"
                   SOURCE_FILES itkMesh)

  CABLE_DEFINE_SET(ImageType "Image<$PixelType, $Dimension>"
                   SOURCE_FILES itkImage)

# Close the current namespace and confirm that it was called "itk".
CABLE_CLOSE_NAMESPACE(itk)

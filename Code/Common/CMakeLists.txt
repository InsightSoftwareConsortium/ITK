LIBRARY(ITKCommon)

# Sources of non-templated classes.
SOURCE_FILES(
itkCommand 
itkDataObject 
itkDirectory 
itkDynamicLoader 
itkExceptionObject 
itkFastMutexLock 
itkIndent 
itkLightObject 
itkMeshRegion 
itkMultiThreader 
itkMutexLock 
itkNumericTraits 
itkObject 
itkObjectFactoryBase 
itkOutputWindow 
itkProcessObject 
itkRegion 
itkTimeStamp 
itkVersion 
itkImageIOCommon
itkImageIO
)

SOURCE_FILES_REQUIRE(WIN32 SOURCES_BEGIN itkWin32OutputWindow)

# We want to do manual instantiation for this library.
UNIX_DEFINES(-DITK_MANUAL_INSTANTIATION)
WIN32_DEFINES(-DITK_MANUAL_INSTANTIATION)

# Put everything in the "Common" package (related to CABLE, everything
# will still be put in the ITKCommon library).
CABLE_PACKAGE(Common)

# Sources of templated classes.
CABLE_SOURCE_FILES(
itkArray
itkHexahedronCell
itkLineCell
itkTetrahedronCell
itkTriangleCell
itkVertexCell
itkQuadrilateralCell
itkPhysicalImage
itkAcosImageAdaptor
itkAsinImageAdaptor
itkAtanImageAdaptor
itkCosImageAdaptor
itkExpImageAdaptor
itkLog10ImageAdaptor
itkLogImageAdaptor
itkSinImageAdaptor
itkSqrtImageAdaptor
itkTanImageAdaptor
)

# All our classes are in the "itk" namespace.
CABLE_OPEN_NAMESPACE(itk)
  # Create a few sets consisting of pieces of class names.  These save us
  # the work of writing out all the combinations of these below.
  CABLE_DEFINE_SET(cellTopology Hexahedron Line Quadrilateral Tetrahedron
                                Triangle Vertex)
  CABLE_DEFINE_SET(cellOrBoundary Cell Boundary)
  CABLE_DEFINE_SET(iaFunc Acos Asin Cos Exp Log10 Log Sin Sqrt Tan)

  # Define the set of CellTraits corresponding to our Mesh types.
  CABLE_DEFINE_SET(CellTraitsType "$(MeshType)::CellTraits")

  # List the instantiations for this package.
  CABLE_INSTANTIATE_CLASS(
    "Array<int, 3>"
    $VectorType
    $PointType
    $ImageType
    $MeshType
    "PhysicalImage<$PixelType, $Dimension>"
    "$(cellTopology)$(cellOrBoundary)<$PixelType, $CellTraitsType>"
    "$(iaFunc)ImageAdaptor<$ImageType, $PixelType>"
  )
CABLE_CLOSE_NAMESPACE(itk)

LIBRARY(ITKCommon)

FIND_INCLUDE( FLTK_INCLUDE_DIR fltk.h H:/usr/local/fltk )

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
itkAcosImageAdaptor
itkAsinImageAdaptor
itkAtanImageAdaptor
itkAffineRegistrationTransform
itkAffineTransform
itkArray
itkBackwardDifferenceOperator
itkBoundingBox
itkByteSwapper
itkCellBoundary
itkCellInterface
itkCenteredAffineRegistrationTransform
itkCentralDerivativeImageFunction
itkCosImageAdaptor
itkCovariantVector
itkDerivativeOperator
itkEntropyPreservingGradientMagnitudeImageFunction
itkExpImageAdaptor
itkForwardDifferenceOperator
itkGaussianOperator
itkHexahedronCell
itkImage
itkImageAdaptor
itkImageBase
itkImageBufferIterator
itkImageConstIteratorWithIndex
itkImageIterator
itkImageIteratorWithIndex
itkImageLinearConstIterator
itkImageLinearIterator
itkImageRegion
itkImageRegionIterator
itkImageSliceConstIterator
itkImageSliceIterator
itkImageSource
itkImageToImageFilter
itkImportImageContainer
itkLevelSetCurvatureFunction
itkLineCell
itkLinearInterpolateImageFunction
itkLog10ImageAdaptor
itkLogImageAdaptor
itkMapContainer
itkMatrix
itkMesh
itkNeighborhood
itkNeighborhoodAlgorithm
itkNeighborhoodIterator
itkNeighborhoodOperator
itkNthElementImageAdaptor
itkPhysicalImage
itkPhysicalImageAdaptor
itkPoint
itkPointLocator
itkPointSet
itkQuadrilateralCell
itkRGBPixel
itkRandomAccessNeighborhoodIterator
itkRegionBoundaryNeighborhoodIterator
itkRegionNeighborhoodIterator
itkRegionNonBoundaryNeighborhoodIterator
itkSimpleImageRegionConstIterator
itkSimpleImageRegionIterator
itkSinImageAdaptor
itkSmartPointerForwardReference
itkSmartRegionNeighborhoodIterator
itkSqrtImageAdaptor
itkStatDenseHistogram
itkStatHistogram
itkStatSparseHistogram
itkTanImageAdaptor
itkTetrahedronCell
itkTriangleCell
itkTransformation
itkUpwindDerivativeImageFunction
itkVector
itkVectorContainer
itkVertexCell
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
  CABLE_DEFINE_SET(CellTraitsType "Mesh<float>::CellTraits")

  # List the instantiations for this package.
  CABLE_INSTANTIATE_CLASS(
    "Array<int, 3>"
    $VectorType
    $PointType
    $ImageType
    $PointSetType
    $MeshType
    "PhysicalImage<$PixelType, $Dimension>"
    "ImageRegion<$Dimension>"
    "ImageSource<$ImageType >"
    "ImageIterator<$ImageType >"
    "ImageIterator<$PhysicalImageType >"
    "ImageIteratorWithIndex<$ImageType >"
    "ImageIteratorWithIndex<$PhysicalImageType >"
    "ImageLinearIterator<$PhysicalImageType >"
    "SimpleImageRegionIterator<$ImageType >"
    "$(cellTopology)$(cellOrBoundary)<$PixelType, $CellTraitsType>"
    "$(iaFunc)ImageAdaptor<$ImageType, $PixelType>"
    "ImageToImageFilter<$ImageType, $ImageType >"
    "ImageToImageFilter<$PhysicalImageType, $PhysicalImageType >"
    "CentralDerivativeImageFunction<$ImageType >"
    "CentralDerivativeImageFunction<$PhysicalImageType >"
    "EntropyPreservingGradientMagnitudeImageFunction<$ImageType >"
    "EntropyPreservingGradientMagnitudeImageFunction<$PhysicalImageType >"
    "LinearInterpolateImageFunction<$ImageType >"
    "LinearInterpolateImageFunction<$PhysicalImageType >"
    "UpwindDerivativeImageFunction<$ImageType >"
    "UpwindDerivativeImageFunction<$PhysicalImageType >"
    "LevelSetCurvatureFunction<$ImageType >"
    "LevelSetCurvatureFunction<$PhysicalImageType >"
    "CenteredAffineRegistrationTransform<float, $Dimension, $PointType >"
  )
CABLE_CLOSE_NAMESPACE(itk)

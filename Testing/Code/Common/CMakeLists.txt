LINK_LIBRARIES (
ITKCommon
)

TESTS(
itkAdaptorComparisonTest
itkAffineTransformTest
itkArrayTest
itkBoundaryConditionTest
itkBoundingBoxTest
itkByteSwapTest
itkDataTypeTest
itkExceptionObjectTest
itkCommonHeaderTest
itkHashTableTest
itkImageAdaptorTest
itkImageIteratorTest
itkImageIteratorsForwardBackwardTest
itkImageLinearIteratorTest
itkIteratorTests
itkMatrixTest
itkMemoryLeakTest
itkMeshTest
itkMeshFstreamTest
itkNeighborhoodTest
itkNeighborhoodAlgorithmTest
itkNeighborhoodIteratorTest
itkNeighborhoodOperatorTest
itkPixelAccessTest
itkPointGeometryTest
itkPointSetTest
itkRandomAccessNeighborhoodIteratorTest
itkScalarTest
itkSliceIteratorTest
itkSmartPointerTest
itkTranslationTransformTest
itkVectorGeometryTest
itkVectorTest
itkSplineKernelTransformTest
)


ADD_EXECUTABLE(itkAdaptorComparisonTest itkAdaptorComparisonTest)
ADD_EXECUTABLE(itkAffineTransformTest itkAffineTransformTest)
ADD_EXECUTABLE(itkArrayTest itkArrayTest)
ADD_EXECUTABLE(itkBoundaryConditionTest itkBoundaryConditionTest)
ADD_EXECUTABLE(itkBoundingBoxTest itkBoundingBoxTest)
ADD_EXECUTABLE(itkByteSwapTest itkByteSwapTest)
ADD_EXECUTABLE(itkDataTypeTest itkDataTypeTest)
ADD_EXECUTABLE(itkExceptionObjectTest itkExceptionObjectTest)
ADD_EXECUTABLE(itkCommonHeaderTest itkCommonHeaderTest)
ADD_EXECUTABLE(itkHashTableTest itkHashTableTest)
ADD_EXECUTABLE(itkImageAdaptorTest itkImageAdaptorTest)
ADD_EXECUTABLE(itkImageIteratorTest itkImageIteratorTest)
ADD_EXECUTABLE(itkImageIteratorsForwardBackwardTest itkImageIteratorsForwardBackwardTest)
ADD_EXECUTABLE(itkImageLinearIteratorTest itkImageLinearIteratorTest)
ADD_EXECUTABLE(itkIteratorTests itkIteratorTests)
ADD_EXECUTABLE(itkMatrixTest itkMatrixTest)
ADD_EXECUTABLE(itkMemoryLeakTest itkMemoryLeakTest)
ADD_EXECUTABLE(itkMeshTest itkMeshTest)
ADD_EXECUTABLE(itkMeshFstreamTest itkMeshFstreamTest)
ADD_EXECUTABLE(itkNeighborhoodTest itkNeighborhoodTest)
ADD_EXECUTABLE(itkNeighborhoodAlgorithmTest itkNeighborhoodAlgorithmTest)
ADD_EXECUTABLE(itkNeighborhoodIteratorTest itkNeighborhoodIteratorTest)
ADD_EXECUTABLE(itkNeighborhoodOperatorTest itkNeighborhoodOperatorTest)
ADD_EXECUTABLE(itkPixelAccessTest itkPixelAccessTest)
ADD_EXECUTABLE(itkPointGeometryTest itkPointGeometryTest)
ADD_EXECUTABLE(itkPointSetTest itkPointSetTest)
ADD_EXECUTABLE(itkScalarTest itkScalarTest)
ADD_EXECUTABLE(itkSliceIteratorTest itkSliceIteratorTest)
ADD_EXECUTABLE(itkSmartPointerTest itkSmartPointerTest)
ADD_EXECUTABLE(itkTranslationTransformTest itkTranslationTransformTest)
ADD_EXECUTABLE(itkVectorGeometryTest itkVectorGeometryTest)
ADD_EXECUTABLE(itkVectorTest itkVectorTest)
ADD_EXECUTABLE(itkSplineKernelTransformTest itkSplineKernelTransformTest)
ADD_EXECUTABLE(itkRandomAccessNeighborhoodIteratorTest itkRandomAccessNeighborhoodIteratorTest)


ADD_LIBRARY(ITKCommonTest)

# Put everything in the "Common" package (related to CABLE, everything
# will still be put in the ITKCommonTest library).
CABLE_PACKAGE(Common ITKCommonTest)

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
  )
CABLE_CLOSE_NAMESPACE(itk)

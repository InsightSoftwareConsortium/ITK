
INCLUDE_DIRECTORIES(
../../..
../../../Code/Common
../../../Code/Numerics
../../../Code/Numerics/vxl
../../../Code/Numerics/vxl/vcl
)

LINK_DIRECTORIES(
../../../Code/Numerics/vxl
../../../Code/Common
)

LINK_LIBRARIES(
VXLNumerics
ITKCommon
)

TESTS(
itkAffineTransformTest
itkArrayTest
itkBoundingBoxTest
itkByteSwapTest
itkDataTypeTest
itkExceptionObjectTest
itkCommonHeaderTest
itkImageAdaptorTest
itkImageIteratorTest
itkIteratorTests
itkMeshInstanceTest
itkMeshTest
itkNeighborhoodOperatorTest
itkPixelAccessTest
itkPointGeometryTest
itkScalarTest
itkSliceIteratorTest
itkSmartPointerTest
itkVectorGeometryTest
itkVectorTest
)


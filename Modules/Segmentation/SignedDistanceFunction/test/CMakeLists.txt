itk_module_test()
set(ITKSignedDistanceFunctionTests
itkSphereSignedDistanceFunctionTest.cxx
itkPCAShapeSignedDistanceFunctionTest.cxx
)

CreateTestDriver(ITKSignedDistanceFunction  "${ITKSignedDistanceFunction-Test_LIBRARIES}" "${ITKSignedDistanceFunctionTests}")

itk_add_test(NAME itkSphereSignedDistanceFunctionTest
      COMMAND ITKSignedDistanceFunctionTestDriver itkSphereSignedDistanceFunctionTest)
itk_add_test(NAME itkPCAShapeSignedDistanceFunctionTest
      COMMAND ITKSignedDistanceFunctionTestDriver itkPCAShapeSignedDistanceFunctionTest)

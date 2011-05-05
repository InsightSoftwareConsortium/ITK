itk_module_test()
set(ITK-SignedDistanceFunctionTests
itkSphereSignedDistanceFunctionTest.cxx
itkPCAShapeSignedDistanceFunctionTest.cxx
itkSignedDistanceFunctionHeaderTest.cxx
)

CreateTestDriver(ITK-SignedDistanceFunction  "${ITK-SignedDistanceFunction-Test_LIBRARIES}" "${ITK-SignedDistanceFunctionTests}")

itk_add_test(NAME itkSignedDistanceFunctionHeaderTest
      COMMAND ITK-SignedDistanceFunctionTestDriver itkSignedDistanceFunctionHeaderTest)
itk_add_test(NAME itkSphereSignedDistanceFunctionTest
      COMMAND ITK-SignedDistanceFunctionTestDriver itkSphereSignedDistanceFunctionTest)
itk_add_test(NAME itkPCAShapeSignedDistanceFunctionTest
      COMMAND ITK-SignedDistanceFunctionTestDriver itkPCAShapeSignedDistanceFunctionTest)

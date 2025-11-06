itk_module_test()
set(
  ITKSignedDistanceFunctionGTests
  itkPCAShapeSignedDistanceFunctionGTest.cxx
  itkSphereSignedDistanceFunctionGTest.cxx
)

creategoogletestdriver(ITKSignedDistanceFunction "${ITKSignedDistanceFunction-Test_LIBRARIES}"
                       "${ITKSignedDistanceFunctionGTests}"
)

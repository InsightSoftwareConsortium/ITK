itk_module_test()

set(
  RansacTests
  itkRansacTest_PlaneEstimation.cxx
  itkRansacTest_SphereEstimation.cxx
)

createtestdriver(Ransac "${Ransac-Test_LIBRARIES}" "${RansacTests}")

itk_add_test(
  NAME itkRansacTest_PlaneEstimation
  COMMAND
    RansacTestDriver
    itkRansacTest_PlaneEstimation
)

itk_add_test(
  NAME itkRansacTest_SphereEstimation
  COMMAND
    RansacTestDriver
    itkRansacTest_SphereEstimation
)

itk_module_test()

set(RANSACTests itkRansacTest_LandmarkRegistration.cxx)

createtestdriver(RANSAC "${RANSAC-Test_LIBRARIES}" "${RANSACTests}")

itk_add_test(
  NAME itkRansacTest_LandmarkRegistration
  COMMAND
    RANSACTestDriver
    itkRansacTest_LandmarkRegistration
    DATA{Baseline/movingFeatureMesh.vtk}
    DATA{Baseline/fixedFeatureMesh.vtk}
    DATA{Baseline/movingMesh.vtk}
    DATA{Baseline/fixedMesh.vtk}
)

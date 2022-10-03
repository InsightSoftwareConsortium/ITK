itk_module_test()

set(RansacTests itkRansacTest_LandmarkRegistration.cxx)

createtestdriver(Ransac "${Ransac-Test_LIBRARIES}" "${RansacTests}")

itk_add_test(
  NAME itkRansacTest_LandmarkRegistration
  COMMAND
    RansacTestDriver
    itkRansacTest_LandmarkRegistration
    DATA{Baseline/movingFeatureMesh.vtk}
    DATA{Baseline/fixedFeatureMesh.vtk}
    DATA{Baseline/movingMesh.vtk}
    DATA{Baseline/fixedMesh.vtk}
)

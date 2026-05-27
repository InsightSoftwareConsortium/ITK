itk_module_test()

set(FpfhTests itkPointFeatureTest.cxx)

createtestdriver(Fpfh "${Fpfh-Test_LIBRARIES}" "${FpfhTests}")

itk_add_test(
  NAME itkPointFeatureTest
  COMMAND
    FpfhTestDriver
    itkPointFeatureTest
    DATA{Input/WSB_subsampled.vtk}
)

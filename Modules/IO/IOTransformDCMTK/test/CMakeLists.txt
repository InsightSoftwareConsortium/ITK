itk_module_test()
set(IOTransformDCMTKTests itkDCMTKTransformIOTest.cxx)

createtestdriver(IOTransformDCMTK "${IOTransformDCMTK-Test_LIBRARIES}" "${IOTransformDCMTKTests}")

itk_add_test(
  NAME itkDCMTKTransformIOTest
  COMMAND
    IOTransformDCMTKTestDriver
    itkDCMTKTransformIOTest
)

itk_module_test()
set(IOTransformDCMTKTests itkDCMTKTransformIOTest.cxx)

createtestdriver(IOTransformDCMTK "${IOTransformDCMTK-Test_LIBRARIES}" "${IOTransformDCMTKTests}")

itk_add_test(
  NAME itkDCMTKTransformIOTest
  COMMAND
    IOTransformDCMTKTestDriver
    itkDCMTKTransformIOTest
    DATA{Input/rect-centered/,REGEX:image[0-9]+.dcm}
    DATA{Input/rect-offset/,REGEX:image[0-9]+.dcm}
    DATA{Input/rect-offset-sro-rigid/transform.dcm}
)

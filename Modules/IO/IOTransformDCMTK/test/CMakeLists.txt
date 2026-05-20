itk_module_test()
set(
  IOTransformDCMTKTests
  itkDCMTKTransformIOTest.cxx
  itkDCMTKTransformIOResampleTest.cxx
)

createtestdriver(IOTransformDCMTK "${IOTransformDCMTK-Test_LIBRARIES}" "${IOTransformDCMTKTests}")

set(TEMP ${PROJECT_BINARY_DIR}/Testing/Temporary)

itk_add_test(
  NAME itkDCMTKTransformIOTest
  COMMAND
    IOTransformDCMTKTestDriver
    itkDCMTKTransformIOTest
    DATA{Input/rect-offset-sro-rigid/transform.dcm}
)

itk_add_test(
  NAME itkDCMTKTransformIOResampleTest
  COMMAND
    IOTransformDCMTKTestDriver
    --compareRadiusTolerance
    2
    --compare
    DATA{Baseline/DicomTransformResampledMovingOutputBaseline.mha}
    ${TEMP}/DicomTransformResampledMovingOutput.mha
    itkDCMTKTransformIOResampleTest
    DATA{Input/rect-centered/,REGEX:image[0-9]+.dcm}
    DATA{Input/rect-offset/,REGEX:image[0-9]+.dcm}
    DATA{Input/rect-offset-sro-rigid/transform.dcm}
    ${TEMP}/DicomTransformFixedOutput.mha
    ${TEMP}/DicomTransformResampledMovingOutput.mha
)

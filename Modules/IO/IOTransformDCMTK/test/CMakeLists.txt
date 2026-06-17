itk_module_test()
set(
  IOTransformDCMTKTests
  itkDCMTKTransformIOTest.cxx
  itkDCMTKTransformIOResampleTest.cxx
  itkDCMTKTransformIORigidScaleRoundTripTest.cxx
)

createtestdriver(IOTransformDCMTK "${IOTransformDCMTK-Test_LIBRARIES}" "${IOTransformDCMTKTests}")

set(TEMP ${PROJECT_BINARY_DIR}/Testing/Temporary)

set(
  IOTransformDCMTKGTests
  itkDCMTKSeriesFileNamesOrderingGTest.cxx
  itkDCMTKSeriesFileNamesParityGTest.cxx
)
creategoogletestdriver(IOTransformDCMTK "${IOTransformDCMTK-Test_LIBRARIES}" "${IOTransformDCMTKGTests}")

ExternalData_Expand_Arguments(
  ITKData
  _rect_centered_dir
  "DATA{Input/rect-centered/,REGEX:image[0-9]+.dcm}"
)
ExternalData_Expand_Arguments(
  ITKData
  _rect_offset_dir
  "DATA{Input/rect-offset/,REGEX:image[0-9]+.dcm}"
)
target_compile_definitions(
  IOTransformDCMTKGTestDriver
  PRIVATE
    "RECT_CENTERED_DIR=${_rect_centered_dir}"
    "RECT_OFFSET_DIR=${_rect_offset_dir}"
    "ITK_TEST_OUTPUT_DIR=${ITK_TEST_OUTPUT_DIR}"
)
set_property(
  TARGET
    IOTransformDCMTKGTestDriver
  APPEND
  PROPERTY
    DEPENDS
      ITKData
)

itk_add_test(
  NAME itkDCMTKTransformIOTest
  COMMAND
    IOTransformDCMTKTestDriver
    itkDCMTKTransformIOTest
    DATA{Input/rect-offset-sro-rigid/transform.dcm}
)

itk_add_test(
  NAME itkDCMTKTransformIORigidScaleRoundTripTest
  COMMAND
    IOTransformDCMTKTestDriver
    itkDCMTKTransformIORigidScaleRoundTripTest
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
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${TEMP}/DicomTransformResampledMovingOutput.mha
    ${TEMP}/DicomTransformFixedOutput.mha
)

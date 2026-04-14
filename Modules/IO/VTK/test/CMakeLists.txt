itk_module_test()
set(
  ITKIOVTKTests
  itkVTKImageIO2Test.cxx
  itkVTKImageIO2Test2.cxx
  itkVTKImageIOFileReadTest.cxx
  itkVTKImageIOStreamTest.cxx
  itkVTKImageIOTest.cxx
  itkVTKImageIOTest2.cxx
  itkVTKImageIOTest3.cxx
  itkVTIImageIOTest.cxx
  itkVTIImageIOReadWriteTest.cxx
)

createtestdriver(ITKIOVTK "${ITKIOVTK-Test_LIBRARIES}" "${ITKIOVTKTests}")

itk_add_test(
  NAME itkVTIImageIOTest
  COMMAND
    ITKIOVTKTestDriver
    itkVTIImageIOTest
    ${ITK_TEST_OUTPUT_DIR}
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTestCTHead1
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_CTHead1.vti
    itkVTIImageIOReadWriteTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_CTHead1.vti
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTestVisibleWomanEyeSlice
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VisibleWomanEyeSlice.vti
    itkVTIImageIOReadWriteTest
    DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VisibleWomanEyeSlice.vti
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTestHeadMRVolume
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_HeadMRVolume.vti
    itkVTIImageIOReadWriteTest
    DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_HeadMRVolume.vti
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTestVHFColor
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VHFColor.vti
    itkVTIImageIOReadWriteTest
    DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VHFColor.vti
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTestVHFColorZLib
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VHFColorZLib.vti
    itkVTIImageIOReadWriteTest
    DATA{Input/VHFColorZLib.vti}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VHFColorZLib.vti
)

itk_add_test(
  NAME itkVTKImageIO2Test
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIO2Test
    ${ITK_TEST_OUTPUT_DIR}
)
itk_add_test(
  NAME itkVTKImageIOTest
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIOTest
    ${ITK_TEST_OUTPUT_DIR}/test1.vtk
    ${ITK_TEST_OUTPUT_DIR}/test2.vtk
)
itk_add_test(
  NAME itkVTKImageIO2Test2
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIO2Test2
    ${ITK_TEST_OUTPUT_DIR}/itkVTKImageIO2Test2.vtk
)
itk_add_test(
  NAME itkVTKImageIOTest2
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIOTest2
    DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw}
    ${ITK_TEST_OUTPUT_DIR}/testVector.vtk
)
itk_add_test(
  NAME itkVTKImageIOTest3
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIOTest3
    DATA{Input/fibers.vtk}
)
itk_add_test(
  NAME itkVTKImageIOStreamTest
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIOStreamTest
    ${ITK_TEST_OUTPUT_DIR}/itkVTKImageIOStreamTest.vtk
)
itk_add_test(
  NAME itkVTKImageIOFileReadTest
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIOFileReadTest
    DATA{Input/matrix.vtk}
    DATA{Input/ironProt.vtk}
)

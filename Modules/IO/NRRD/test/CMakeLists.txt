itk_module_test()
set(ITKIONRRDTests
itkIONRRDHeaderTest.cxx
itkNrrdImageIOTest.cxx
itkNrrdComplexImageReadTest.cxx
itkNrrdComplexImageReadWriteTest.cxx
itkNrrdCovariantVectorImageReadTest.cxx
itkNrrdCovariantVectorImageReadWriteTest.cxx
itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest.cxx
itkNrrdDiffusionTensor3DImageReadTest.cxx
itkNrrdDiffusionTensor3DImageReadWriteTest.cxx
itkNrrdImageReadWriteTest.cxx
itkNrrdRGBAImageReadWriteTest.cxx
itkNrrdRGBImageReadWriteTest.cxx
itkNrrdVectorImageReadTest.cxx
itkNrrdVectorImageReadWriteTest.cxx
)

CreateTestDriver(ITKIONRRD  "${ITKIONRRD-Test_LIBRARIES}" "${ITKIONRRDTests}")

itk_add_test(NAME itkIONRRDHeaderTest
      COMMAND ITKIONRRDTestDriver itkIONRRDHeaderTest)
itk_add_test(NAME itkNrrdImageIOTest1
      COMMAND ITKIONRRDTestDriver itkNrrdImageIOTest
              ${ITK_TEST_OUTPUT_DIR}/testNrrd.nrrd)
itk_add_test(NAME itkNrrdImageIOTest2
      COMMAND ITKIONRRDTestDriver itkNrrdImageIOTest
              ${ITK_TEST_OUTPUT_DIR}/testNrrd.nhdr)
itk_add_test(NAME itkNrrdComplexImageReadTest
      COMMAND ITKIONRRDTestDriver itkNrrdComplexImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-complex-slow.nrrd)
itk_add_test(NAME itkNrrdComplexImageReadWriteTest
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-complex-fast.nrrd
              ${ITK_TEST_OUTPUT_DIR}/mini-complex.nrrd
    itkNrrdComplexImageReadWriteTest ${ITK_DATA_ROOT}/Input/mini-complex-slow.nrrd ${ITK_TEST_OUTPUT_DIR}/mini-complex.nrrd)
itk_add_test(NAME itkNrrdCovariantVectorImageReadTest
      COMMAND ITKIONRRDTestDriver itkNrrdCovariantVectorImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-covector-fast.nrrd)
itk_add_test(NAME itkNrrdCovariantVectorImageReadWriteTest
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-covector.nrrd
              ${ITK_TEST_OUTPUT_DIR}/mini-covector.nrrd
    itkNrrdCovariantVectorImageReadWriteTest ${ITK_DATA_ROOT}/Input/mini-covector-slow.nrrd ${ITK_TEST_OUTPUT_DIR}/mini-covector.nrrd)
itk_add_test(NAME itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDouble
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/small-tensors-double.nrrd
              ${ITK_TEST_OUTPUT_DIR}/small-tensors-double.nrrd
    itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest ${ITK_DATA_ROOT}/Input/small-tensors.nhdr ${ITK_TEST_OUTPUT_DIR}/small-tensors-double.nrrd)
itk_add_test(NAME itkNrrdDiffusionTensor3DImageReadTest1
      COMMAND ITKIONRRDTestDriver itkNrrdDiffusionTensor3DImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-ten-mask.nrrd)
itk_add_test(NAME itkNrrdDiffusionTensor3DImageReadTest2
      COMMAND ITKIONRRDTestDriver itkNrrdDiffusionTensor3DImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-ten-nomask-fast.nrrd)
itk_add_test(NAME itkNrrdDiffusionTensor3DImageReadTest3
      COMMAND ITKIONRRDTestDriver itkNrrdDiffusionTensor3DImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-ten-nomask-slow.nrrd)
itk_add_test(NAME itkNrrdDiffusionTensor3DImageReadWriteTest
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-ten.nrrd
              ${ITK_TEST_OUTPUT_DIR}/mini-ten.nrrd
    itkNrrdDiffusionTensor3DImageReadWriteTest ${ITK_DATA_ROOT}/Input/mini-ten-nomask-fast.nrrd ${ITK_TEST_OUTPUT_DIR}/mini-ten.nrrd)
itk_add_test(NAME itkNrrdDiffusionTensor3DImageReadWriteTest4
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/small-tensors.nhdr
              ${ITK_TEST_OUTPUT_DIR}/small-tensors.nhdr
    itkNrrdDiffusionTensor3DImageReadWriteTest ${ITK_DATA_ROOT}/Input/small-tensors.nhdr ${ITK_TEST_OUTPUT_DIR}/small-tensors.nhdr)
itk_add_test(NAME itkNrrdImageReadWriteTest1
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdImageReadWriteTest1.nrrd
              ${ITK_TEST_OUTPUT_DIR}/box.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/box.nhdr ${ITK_TEST_OUTPUT_DIR}/box.nrrd)
itk_add_test(NAME itkNrrdImageReadWriteTest2
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol2.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd ${ITK_TEST_OUTPUT_DIR}/vol2.nrrd)
itk_add_test(NAME itkNrrdImageReadWriteTest3
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol3.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-raw-big.nrrd ${ITK_TEST_OUTPUT_DIR}/vol3.nrrd)
itk_add_test(NAME itkNrrdImageReadWriteTest4
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol4.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-raw-little.nrrd ${ITK_TEST_OUTPUT_DIR}/vol4.nrrd)
itk_add_test(NAME itkNrrdImageReadWriteTest5
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol5.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-gzip-big.nrrd ${ITK_TEST_OUTPUT_DIR}/vol5.nrrd)
itk_add_test(NAME itkNrrdImageReadWriteTest6
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol6.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-gzip-little.nrrd ${ITK_TEST_OUTPUT_DIR}/vol6.nrrd)
itk_add_test(NAME itkNrrdImageReadWriteTest7
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol7.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-ascii.nhdr ${ITK_TEST_OUTPUT_DIR}/vol7.nrrd)
itk_add_test(NAME itkNrrdImageReadWriteTest8
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol8.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-raw-big.nhdr ${ITK_TEST_OUTPUT_DIR}/vol8.nrrd)
itk_add_test(NAME itkNrrdImageReadWriteTest9
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol9.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-raw-little.nhdr ${ITK_TEST_OUTPUT_DIR}/vol9.nrrd)
itk_add_test(NAME itkNrrdImageReadWriteTest10
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol10.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-gzip-big.nhdr ${ITK_TEST_OUTPUT_DIR}/vol10.nrrd)
itk_add_test(NAME itkNrrdImageReadWriteTest11
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol11.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-gzip-little.nhdr ${ITK_TEST_OUTPUT_DIR}/vol11.nrrd)
itk_add_test(NAME itkNrrdRGBAImageReadWriteTest
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBAImageReadWriteTest.png
              ${ITK_TEST_OUTPUT_DIR}/NrrdRGBAImageReadWriteTest.png
    itkNrrdRGBAImageReadWriteTest ${ITK_DATA_ROOT}/Input/testrgba.nrrd ${ITK_TEST_OUTPUT_DIR}/NrrdRGBAImageReadWriteTest.png)
itk_add_test(NAME itkNrrdRGBImageReadWriteTest0
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest0.png
              ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest0.png
    itkNrrdRGBImageReadWriteTest ${ITK_DATA_ROOT}/Input/testrgb-0.nhdr ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest0.png)
itk_add_test(NAME itkNrrdRGBImageReadWriteTest1
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest1.png
              ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest1.png
    itkNrrdRGBImageReadWriteTest ${ITK_DATA_ROOT}/Input/testrgb-1.nhdr ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest1.png)
itk_add_test(NAME itkNrrdRGBImageReadWriteTest2
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest2.png
              ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest2.png
    itkNrrdRGBImageReadWriteTest ${ITK_DATA_ROOT}/Input/testrgb-2.nhdr ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest2.png)
itk_add_test(NAME itkNrrdVectorImageReadTest
      COMMAND ITKIONRRDTestDriver itkNrrdVectorImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-vector-fast.nrrd)
itk_add_test(NAME itkNrrdVectorImageReadWriteTest
      COMMAND ITKIONRRDTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-vector.nrrd
              ${ITK_TEST_OUTPUT_DIR}/mini-vector.nrrd
    itkNrrdVectorImageReadWriteTest ${ITK_DATA_ROOT}/Input/mini-vector-slow.nrrd ${ITK_TEST_OUTPUT_DIR}/mini-vector.nrrd)

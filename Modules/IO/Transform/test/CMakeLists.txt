itk_module_test()
set(ITKIOTransformTests
itkTransformIOTest.cxx
itkCenteredRigid2DTransformTest.cxx
itkEuler2DTransformTest.cxx
itkTransformFileReaderWriterTest.cxx
)

CreateTestDriver(ITKIOTransform "${ITKIOTransform-Test_LIBRARIES}" "${ITKIOTransformTests}")

itk_add_test(NAME itkTransformIOTest
      COMMAND ITKIOTransformTestDriver itkTransformIOTest
              ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkCenteredRigid2DTransformTest
      COMMAND ITKIOTransformTestDriver itkCenteredRigid2DTransformTest
              ${ITK_TEST_OUTPUT_DIR}/testCenteredTransform.txt)
itk_add_test(NAME itkEuler2DTransformTest
      COMMAND ITKIOTransformTestDriver itkEuler2DTransformTest
              ${ITK_TEST_OUTPUT_DIR}/testEulerTransform.txt)
itk_add_test(NAME itkTransformFileReaderWriterTest0
      COMMAND ITKIOTransformTestDriver itkTransformFileReaderWriterTest
              DATA{${ITK_DATA_ROOT}/Input/itkAffineTransform00.txt} ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest0.txt)
itk_add_test(NAME itkTransformFileReaderWriterTest1
      COMMAND ITKIOTransformTestDriver itkTransformFileReaderWriterTest
              DATA{${ITK_DATA_ROOT}/Input/itkAffineTransform01.txt} ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest1.txt)
itk_add_test(NAME itkTransformFileReaderWriterTest2
      COMMAND ITKIOTransformTestDriver itkTransformFileReaderWriterTest
              DATA{${ITK_DATA_ROOT}/Input/itkAffineTransform01.txt} ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest1.mat)
itk_add_test(NAME itkTransformFileReaderWriterTest3
      COMMAND ITKIOTransformTestDriver itkTransformFileReaderWriterTest
              DATA{Baseline/itkBSplineTransform00.txt} ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransform00.mat)

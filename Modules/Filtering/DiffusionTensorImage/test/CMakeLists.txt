itk_module_test()
set(ITK-DiffusionTensorImageTests
itkDiffusionTensor3DTest.cxx
itkDiffusionTensor3DReconstructionImageFilterTest.cxx
itkDiffusionTensorImageHeaderTest.cxx
)

CreateTestDriver(ITK-DiffusionTensorImage  "${ITK-DiffusionTensorImage-Test_LIBRARIES}" "${ITK-DiffusionTensorImageTests}")

add_test(NAME itkDiffusionTensorImageHeaderTest
      COMMAND ITK-DiffusionTensorImageTestDriver itkDiffusionTensorImageHeaderTest)
add_test(NAME itkDiffusionTensor3DTest
      COMMAND ITK-DiffusionTensorImageTestDriver itkDiffusionTensor3DTest)
add_test(NAME itkDiffusionTensor3DReconstructionImageFilterTest
      COMMAND ITK-DiffusionTensorImageTestDriver itkDiffusionTensor3DReconstructionImageFilterTest)

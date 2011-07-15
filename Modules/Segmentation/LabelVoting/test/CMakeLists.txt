itk_module_test()
set(ITKLabelVotingTests
itkLabelVotingHeaderTest.cxx
itkVotingBinaryImageFilterTest.cxx
itkLabelVotingImageFilterTest.cxx
itkVotingBinaryIterativeHoleFillingImageFilterTest.cxx
itkBinaryMedianImageFilterTest.cxx
itkVotingBinaryHoleFillingImageFilterTest.cxx
)

CreateTestDriver(ITKLabelVoting  "${ITKLabelVoting-Test_LIBRARIES}" "${ITKLabelVotingTests}")

itk_add_test(NAME itkLabelVotingHeaderTest
      COMMAND ITKLabelVotingTestDriver itkLabelVotingHeaderTest)
itk_add_test(NAME itkVotingBinaryImageFilterTest
      COMMAND ITKLabelVotingTestDriver itkVotingBinaryImageFilterTest)
itk_add_test(NAME itkLabelVotingImageFilterTest
      COMMAND ITKLabelVotingTestDriver itkLabelVotingImageFilterTest)
itk_add_test(NAME itkVotingBinaryIterativeHoleFillingImageFilterTest
      COMMAND ITKLabelVotingTestDriver itkVotingBinaryIterativeHoleFillingImageFilterTest)
itk_add_test(NAME itkBinaryMedianImageFilterTest
      COMMAND ITKLabelVotingTestDriver itkBinaryMedianImageFilterTest)
itk_add_test(NAME itkVotingBinaryHoleFillingImageFilterTest
      COMMAND ITKLabelVotingTestDriver itkVotingBinaryHoleFillingImageFilterTest)

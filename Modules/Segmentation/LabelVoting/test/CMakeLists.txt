itk_module_test()
set(ITK-LabelVotingTests
itkLabelVotingHeaderTest.cxx
itkVotingBinaryImageFilterTest.cxx
itkLabelVotingImageFilterTest.cxx
itkVotingBinaryIterativeHoleFillingImageFilterTest.cxx
itkBinaryMedianImageFilterTest.cxx
itkVotingBinaryHoleFillingImageFilterTest.cxx
)

CreateTestDriver(ITK-LabelVoting  "${ITK-LabelVoting-Test_LIBRARIES}" "${ITK-LabelVotingTests}")

itk_add_test(NAME itkLabelVotingHeaderTest
      COMMAND ITK-LabelVotingTestDriver itkLabelVotingHeaderTest)
itk_add_test(NAME itkVotingBinaryImageFilterTest
      COMMAND ITK-LabelVotingTestDriver itkVotingBinaryImageFilterTest)
itk_add_test(NAME itkLabelVotingImageFilterTest
      COMMAND ITK-LabelVotingTestDriver itkLabelVotingImageFilterTest)
itk_add_test(NAME itkVotingBinaryIterativeHoleFillingImageFilterTest
      COMMAND ITK-LabelVotingTestDriver itkVotingBinaryIterativeHoleFillingImageFilterTest)
itk_add_test(NAME itkBinaryMedianImageFilterTest
      COMMAND ITK-LabelVotingTestDriver itkBinaryMedianImageFilterTest)
itk_add_test(NAME itkVotingBinaryHoleFillingImageFilterTest
      COMMAND ITK-LabelVotingTestDriver itkVotingBinaryHoleFillingImageFilterTest)

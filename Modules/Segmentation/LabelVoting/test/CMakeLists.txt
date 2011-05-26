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

add_test(NAME itkLabelVotingHeaderTest
      COMMAND ITK-LabelVotingTestDriver itkLabelVotingHeaderTest)
add_test(NAME itkVotingBinaryImageFilterTest
      COMMAND ITK-LabelVotingTestDriver itkVotingBinaryImageFilterTest)
add_test(NAME itkLabelVotingImageFilterTest
      COMMAND ITK-LabelVotingTestDriver itkLabelVotingImageFilterTest)
add_test(NAME itkVotingBinaryIterativeHoleFillingImageFilterTest
      COMMAND ITK-LabelVotingTestDriver itkVotingBinaryIterativeHoleFillingImageFilterTest)
add_test(NAME itkBinaryMedianImageFilterTest
      COMMAND ITK-LabelVotingTestDriver itkBinaryMedianImageFilterTest)
add_test(NAME itkVotingBinaryHoleFillingImageFilterTest
      COMMAND ITK-LabelVotingTestDriver itkVotingBinaryHoleFillingImageFilterTest)

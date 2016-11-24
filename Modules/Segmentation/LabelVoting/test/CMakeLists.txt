itk_module_test()
set(ITKLabelVotingTests
itkVotingBinaryImageFilterTest.cxx
itkLabelVotingImageFilterTest.cxx
itkMultiLabelSTAPLEImageFilterTest.cxx
itkVotingBinaryIterativeHoleFillingImageFilterTest.cxx
itkBinaryMedianImageFilterTest.cxx
itkVotingBinaryHoleFillingImageFilterTest.cxx
)

CreateTestDriver(ITKLabelVoting  "${ITKLabelVoting-Test_LIBRARIES}" "${ITKLabelVotingTests}")

itk_add_test(NAME itkVotingBinaryImageFilterTest0
      COMMAND ITKLabelVotingTestDriver
        --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkVotingBinaryImageFilterTest0.mha "5a20f8e1f4f15d3c6059842550d67d2f"
      itkVotingBinaryImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1-bin.png} ${ITK_TEST_OUTPUT_DIR}/itkVotingBinaryImageFilterTest0.mha 5 255 0 )
itk_add_test(NAME itkVotingBinaryImageFilterTest1
      COMMAND ITKLabelVotingTestDriver
        --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkVotingBinaryImageFilterTest1.mha "5afdcc3ca5e4c6295a15391ca7cbfec0"
      itkVotingBinaryImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkVotingBinaryImageFilterTest1.mha 5 100 0 )
itk_add_test(NAME itkLabelVotingImageFilterTest
      COMMAND ITKLabelVotingTestDriver itkLabelVotingImageFilterTest)
itk_add_test(NAME itkVotingBinaryIterativeHoleFillingImageFilterTest
      COMMAND ITKLabelVotingTestDriver itkVotingBinaryIterativeHoleFillingImageFilterTest)
itk_add_test(NAME itkBinaryMedianImageFilterTest
      COMMAND ITKLabelVotingTestDriver itkBinaryMedianImageFilterTest)
itk_add_test(NAME itkVotingBinaryHoleFillingImageFilterTest
      COMMAND ITKLabelVotingTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkVotingBinaryHoleFillingImageFilterTest.png
              fd3bd75fc6270e3c416a3f0807a6e24b
    itkVotingBinaryHoleFillingImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkVotingBinaryHoleFillingImageFilterTest.png)
itk_add_test( NAME itkMultiLabelSTAPLEImageFilterTest
      COMMAND ITKLabelVotingTestDriver itkMultiLabelSTAPLEImageFilterTest )

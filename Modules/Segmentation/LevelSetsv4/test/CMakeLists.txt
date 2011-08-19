itk_module_test()
set(ITKLevelSetsv4Tests
itkLevelSetImageBaseTest.cxx
itkWhitakerSparseLevelSetBaseTest.cxx
itkShiSparseLevelSetBaseTest.cxx
itkMalcolmSparseLevelSetBaseTest.cxx
# binary image to sparse level set adaptors
itkBinaryImageToWhitakerSparseLevelSetAdaptorTest.cxx
itkBinaryImageToMalcolmSparseLevelSetAdaptorTest.cxx
itkBinaryImageToShiSparseLevelSetAdaptorTest.cxx
itkLevelSetEquationChanAndVeseExternalTermTest.cxx
itkLevelSetEquationChanAndVeseInternalTermTest.cxx
itkLevelSetEquationCurvatureTermTest.cxx
itkLevelSetEquationLaplacianTermTest.cxx
itkLevelSetEquationPropagationTermTest.cxx
itkLevelSetEquationRegionTermBaseTest.cxx
itkLevelSetEquationTermBaseTest.cxx
itkLevelSetEquationTermContainerBaseTest.cxx
# domain partition classes
itkLevelSetDomainPartitionBaseTest.cxx
itkLevelSetDomainMapImageFilterTest.cxx
# level set container
itkSparseLevelSetContainerTest.cxx
# single level set
itkSingleLevelSetDense2DTest.cxx
itkSingleLevelSetSparse2DTest.cxx
itkSingleLevelSetMalcolm2DTest.cxx
itkSingleLevelSetShi2DTest.cxx
# two level set
itkTwoLevelSetDense2DTest.cxx
itkTwoLevelSetSparse2DTest.cxx
itkTwoLevelSetMalcolm2DTest.cxx
itkTwoLevelSetShiSparse2DTest.cxx
# multi level set
itkMultiLevelSetImageTest.cxx
itkMultiLevelSetChanAndVeseInternalTermTest.cxx
itkMultiLevelSetEvolutionTest.cxx
# stopping criterion
itkLevelSetEvolutionNumberOfIterationsStoppingCriterionTest.cxx
)

CreateTestDriver(ITKLevelSetsv4  "${ITKLevelSetsv4-Test_LIBRARIES}" "${ITKLevelSetsv4Tests}")

itk_add_test(NAME itkLevelSetsv4ImageBaseTest
      COMMAND ITKLevelSetsv4TestDriver itkLevelSetImageBaseTest)
itk_add_test(NAME itkWhitakerSparseLevelSetsv4BaseTest
      COMMAND ITKLevelSetsv4TestDriver itkWhitakerSparseLevelSetBaseTest)
itk_add_test(NAME itkShiSparseLevelSetsv4BaseTest
      COMMAND ITKLevelSetsv4TestDriver itkShiSparseLevelSetBaseTest)
itk_add_test(NAME itkMalcolmSparseLevelSetsv4BaseTest
      COMMAND ITKLevelSetsv4TestDriver itkMalcolmSparseLevelSetBaseTest)
# binary image to sparse level set adaptors
itk_add_test(NAME itkBinaryImageToWhitakerSparseLevelSetsv4AdaptorTest
      COMMAND ITKLevelSetsv4TestDriver
      itkBinaryImageToWhitakerSparseLevelSetAdaptorTest
      ${ITK_DATA_ROOT}/Input/circle.png
      ${ITK_TEST_OUTPUT_DIR}/output_binary_whitaker_adaptor.mha
      ${ITK_TEST_OUTPUT_DIR}/status_binary_whitaker_adaptor.mha
      )
itk_add_test(NAME itkBinaryImageToMalcolmSparseLevelSetsv4AdaptorTest
      COMMAND ITKLevelSetsv4TestDriver
      itkBinaryImageToMalcolmSparseLevelSetAdaptorTest
      ${ITK_DATA_ROOT}/Input/circle.png
      ${ITK_TEST_OUTPUT_DIR}/output_binary_malcolm_adaptor.mha
      )
itk_add_test(NAME itkBinaryImageToShiSparseLevelSetsv4AdaptorTest
      COMMAND ITKLevelSetsv4TestDriver
      itkBinaryImageToShiSparseLevelSetAdaptorTest
      ${ITK_DATA_ROOT}/Input/circle.png
      ${ITK_TEST_OUTPUT_DIR}/output_binary_shi_adaptor.mha
      )
# domain partition classes
itk_add_test(NAME itkLevelSetsv4DomainPartitionBaseTest
      COMMAND ITKLevelSetsv4TestDriver itkLevelSetDomainPartitionBaseTest)
itk_add_test(NAME itkLevelSetsv4DomainMapImageFilterTest
      COMMAND ITKLevelSetsv4TestDriver itkLevelSetDomainMapImageFilterTest)
# level set container
itk_add_test(NAME itkLevelSetsv4SparseLevelSetContainerTest
      COMMAND ITKLevelSetsv4TestDriver itkSparseLevelSetContainerTest)
# single level set
itk_add_test(NAME itkSingleLevelSetsv4Dense2DTest
      COMMAND ITKLevelSetsv4TestDriver
      --compare DATA{Baseline/solution_whiteSpot_output_dense_single.mha}
                ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_dense_single.mha
      itkSingleLevelSetDense2DTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png 25 25 25
      ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_dense_single.mha
)

itk_add_test(NAME itkSingleLevelSetsv4Sparse2DTest
      COMMAND ITKLevelSetsv4TestDriver
      --compare DATA{Baseline/solution_whiteSpot_output_sparse_single.mha}
                ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_sparse_single.mha
      itkSingleLevelSetSparse2DTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png
      50
      ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_sparse_single.mha
)
itk_add_test(NAME itkSingleLevelSetsv4Malcolm2DTest
      COMMAND ITKLevelSetsv4TestDriver
      --compare DATA{Baseline/solution_whiteSpot_output_malcolm_single.mha}
                ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_malcolm_single.mha
      itkSingleLevelSetMalcolm2DTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png
      15
      ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_malcolm_single.mha
 )
itk_add_test(NAME itkSingleLevelSetsv4Shi2DTest
      COMMAND ITKLevelSetsv4TestDriver
      --compare DATA{Baseline/solution_whiteSpot_output_shi_single.mha}
                ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_shi_single.mha
      itkSingleLevelSetShi2DTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png
      15
      ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_shi_single.mha)

itk_add_test(NAME LevelSetEquationCurvatureTermTest
      COMMAND ITKLevelSetsv4TestDriver itkLevelSetEquationCurvatureTermTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png )

itk_add_test(NAME itkLevelSetEquationPropagationTermTest
      COMMAND ITKLevelSetsv4TestDriver itkLevelSetEquationPropagationTermTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png )

itk_add_test(NAME itkLevelSetEquationLaplacianTermTest
      COMMAND ITKLevelSetsv4TestDriver itkLevelSetEquationLaplacianTermTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png )

# two level sets
itk_add_test(NAME itkTwoLevelSetsv4Dense2DTest
      COMMAND ITKLevelSetsv4TestDriver
      itkTwoLevelSetDense2DTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png 25 25 10
      ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_dense_two.mha
)

itk_add_test(NAME itkTwoLevelSetsv4Sparse2DTest
      COMMAND ITKLevelSetsv4TestDriver
      --compare DATA{Baseline/solution_whiteSpot_output_sparse_two.mha}
                ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_sparse_two.mha
      itkTwoLevelSetSparse2DTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png
      1
      ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_sparse_two.mha
)

itk_add_test(NAME itkTwoLevelSetsv4Malcolm2DTest
      COMMAND ITKLevelSetsv4TestDriver
      --compare DATA{Baseline/solution_whiteSpot_output_malcolm_two.mha}
                ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_malcolm_two.mha
      itkTwoLevelSetMalcolm2DTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png
      1
      ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_malcolm_two.mha
)

itk_add_test(NAME itkTwoLevelSetsv4ShiSparse2DTest
      COMMAND ITKLevelSetsv4TestDriver
      --compare DATA{Baseline/solution_whiteSpot_output_shi_two.mha}
                ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_shi_two.mha
      itkTwoLevelSetShiSparse2DTest
      ${ITK_DATA_ROOT}/Input/whiteSpot.png
      1
      ${ITK_TEST_OUTPUT_DIR}/whiteSpot_output_shi_two.mha
)

itk_add_test(NAME itkLevelSetEvolutionNumberOfIterationsStoppingCriterionTest
      COMMAND ITKLevelSetsv4TestDriver itkLevelSetEvolutionNumberOfIterationsStoppingCriterionTest)
itk_add_test(NAME itkMultiLevelSetImageTest
      COMMAND ITKLevelSetsv4TestDriver itkMultiLevelSetImageTest)
itk_add_test(NAME itkMultiLevelSetsv4ChanAndVeseInternalTermTest
      COMMAND ITKLevelSetsv4TestDriver itkMultiLevelSetChanAndVeseInternalTermTest)
itk_add_test(NAME itkMultiLevelsv4SetEvolutionTest
      COMMAND ITKLevelSetsv4TestDriver itkMultiLevelSetEvolutionTest)

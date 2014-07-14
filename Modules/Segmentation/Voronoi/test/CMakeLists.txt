itk_module_test()
set(ITKVoronoiTests
itkVoronoiSegmentationImageFilterTest.cxx
itkVoronoiSegmentationRGBImageFilterTest.cxx
itkVoronoiDiagram2DTest.cxx
itkVoronoiPartitioningImageFilterTest.cxx
)

CreateTestDriver(ITKVoronoi  "${ITKVoronoi-Test_LIBRARIES}" "${ITKVoronoiTests}")

itk_add_test(NAME itkVoronoiSegmentationImageFilterTest
      COMMAND ITKVoronoiTestDriver itkVoronoiSegmentationImageFilterTest)
itk_add_test(NAME itkVoronoiSegmentationRGBImageFilterTest
      COMMAND ITKVoronoiTestDriver itkVoronoiSegmentationRGBImageFilterTest)
itk_add_test(NAME itkVoronoiDiagram2DTest
      COMMAND ITKVoronoiTestDriver itkVoronoiDiagram2DTest ${ITK_TEST_OUTPUT_DIR}/VoronoiDiagram2DTest.vtk)
itk_add_test(NAME itkVoronoiPartitioningImageFilterTest1
      COMMAND ITKVoronoiTestDriver
    --compareNumberOfPixelsTolerance 20
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest1.png,:}
              ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png
    itkVoronoiPartitioningImageFilterTest DATA{${ITK_DATA_ROOT}/Input/sf4.png} ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png 1)
itk_add_test(NAME itkVoronoiPartitioningImageFilterTest2
      COMMAND ITKVoronoiTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest2.png,:}
              ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png
    itkVoronoiPartitioningImageFilterTest DATA{${ITK_DATA_ROOT}/Input/sf4.png} ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png 0)

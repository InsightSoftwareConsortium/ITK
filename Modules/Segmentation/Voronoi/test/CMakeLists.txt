itk_module_test()
set(ITK-VoronoiTests
itkVoronoiHeaderTest.cxx
itkVoronoiSegmentationImageFilterTest.cxx
itkVoronoiSegmentationRGBImageFilterTest.cxx
itkVoronoiDiagram2DTest.cxx
itkVoronoiPartitioningImageFilterTest.cxx
)

CreateTestDriver(ITK-Voronoi  "${ITK-Voronoi-Test_LIBRARIES}" "${ITK-VoronoiTests}")

itk_add_test(NAME itkVoronoiHeaderTest
      COMMAND ITK-VoronoiTestDriver itkVoronoiHeaderTest)
itk_add_test(NAME itkVoronoiSegmentationImageFilterTest
      COMMAND ITK-VoronoiTestDriver itkVoronoiSegmentationImageFilterTest)
itk_add_test(NAME itkVoronoiSegmentationRGBImageFilterTest
      COMMAND ITK-VoronoiTestDriver itkVoronoiSegmentationRGBImageFilterTest)
itk_add_test(NAME itkVoronoiDiagram2DTest
      COMMAND ITK-VoronoiTestDriver itkVoronoiDiagram2DTest ${ITK_TEST_OUTPUT_DIR}/VoronoiDiagram2DTest.vtk)
itk_add_test(NAME itkVoronoiPartitioningImageFilterTest1
      COMMAND ITK-VoronoiTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png
    itkVoronoiPartitioningImageFilterTest ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png 1)
itk_add_test(NAME itkVoronoiPartitioningImageFilterTest2
      COMMAND ITK-VoronoiTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png
    itkVoronoiPartitioningImageFilterTest ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png 0)

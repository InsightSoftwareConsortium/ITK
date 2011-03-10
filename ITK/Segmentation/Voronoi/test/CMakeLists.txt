itk_module_test()
set(ITK-VoronoiTests
itkVoronoiHeaderTest.cxx
itkVoronoiSegmentationImageFilterTest.cxx
itkVoronoiSegmentationRGBImageFilterTest.cxx
itkVoronoiDiagram2DTest.cxx
)

CreateTestDriver(ITK-Voronoi  "${ITK-Voronoi-Test_LIBRARIES}" "${ITK-VoronoiTests}")

add_test(NAME itkVoronoiHeaderTest
      COMMAND ITK-VoronoiTestDriver itkVoronoiHeaderTest)
add_test(NAME itkVoronoiSegmentationImageFilterTest
      COMMAND ITK-VoronoiTestDriver itkVoronoiSegmentationImageFilterTest)
add_test(NAME itkVoronoiSegmentationRGBImageFilterTest
      COMMAND ITK-VoronoiTestDriver itkVoronoiSegmentationRGBImageFilterTest)
add_test(NAME itkVoronoiDiagram2DTest
      COMMAND ITK-VoronoiTestDriver itkVoronoiDiagram2DTest)

itk_module_test()
set(ITKLevelSetsv4VisualizationTests
itkVTKVisualizeLevelSetsInteractivePauseTest
itkVTKVisualize2DDenseImageLevelSetTest
vtkVisualize2DWhitakerLevelSetTest
itkImageToRGBVTKImageFilterTest
vtkVisualize2DCellsLevelSetTest
vtkVisualize2DCellsLevelSetSurfaceTest
vtkVisualize2DCellsLevelSetLayersTest
vtkVisualize2DWhitakerLevelSetLayersTest
vtkVisualize2DShiLevelSetLayersTest
vtkVisualize2DMalcolmLevelSetLayersTest
)

set(ITKLevelSetsv4Visualization-Test_LIBRARIES ${ITKLevelSetsv4Visualization_LIBRARIES} )
CreateTestDriver(ITKLevelSetsv4Visualization  "${ITKLevelSetsv4Visualization-Test_LIBRARIES}" "${ITKLevelSetsv4VisualizationTests}")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkVTKVisualize2DDenseImageLevelSetTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver itkVTKVisualize2DDenseImageLevelSetTest)

itk_add_test(NAME vtkVisualize2DWhitakerLevelSetTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DWhitakerLevelSetTest)

itk_add_test(NAME itkImageToRGBVTKImageFilterTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver itkImageToRGBVTKImageFilterTest)

itk_add_test(NAME vtkVisualize2DWhitakerLevelSetLayersTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DWhitakerLevelSetLayersTest)

itk_add_test(NAME vtkVisualize2DShiLevelSetLayersTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DShiLevelSetLayersTest)

itk_add_test(NAME vtkVisualize2DMalcolmLevelSetLayersTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DMalcolmLevelSetLayersTest)

itk_add_test(NAME vtkVisualize2DCellsDenseLevelSetTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetTest
  DATA{Input/cells.png} 60 Dense
  ${TEMP}/vtkVisualize2DCellsDenseLevelSetTest.png
  )
itk_add_test(NAME vtkVisualize2DCellsDenseLevelSetSurfaceTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetSurfaceTest
  DATA{Input/cells.png} 60 Dense
  ${TEMP}/vtkVisualize2DCellsDenseLevelSetSurfaceTest.png
  )

itk_add_test(NAME vtkVisualize2DCellsWhitakerLevelSetTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetTest
  DATA{Input/cells.png} 200 Whitaker
  ${TEMP}/vtkVisualize2DCellsWhitakerLevelSetTest.png
  )
itk_add_test(NAME vtkVisualize2DCellsWhitakerLevelSetSurfaceTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetSurfaceTest
  DATA{Input/cells.png} 200 Whitaker
  ${TEMP}/vtkVisualize2DCellsWhitakerLevelSetSurfaceTest.png
  )
itk_add_test(NAME vtkVisualize2DCellsWhitakerLevelSetLayersTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetLayersTest
  DATA{Input/cells.png} 200 Whitaker
  )

itk_add_test(NAME vtkVisualize2DCellsShiLevelSetTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetTest
  DATA{Input/cells.png} 200 Shi
  ${TEMP}/vtkVisualize2DCellsShiLevelSetTest.png
  )
itk_add_test(NAME vtkVisualize2DCellsShiLevelSetSurfaceTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetSurfaceTest
  DATA{Input/cells.png} 200 Shi
  ${TEMP}/vtkVisualize2DCellsShiLevelSetSurfaceTest.png
  )
itk_add_test(NAME vtkVisualize2DCellsShiLevelSetLayersTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetLayersTest
  DATA{Input/cells.png} 200 Shi
  )

itk_add_test(NAME vtkVisualize2DCellsMalcolmLevelSetTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetTest
  DATA{Input/cells.png} 60 Malcolm
  ${TEMP}/vtkVisualize2DCellsMalcolmLevelSetTest.png
  )
itk_add_test(NAME vtkVisualize2DCellsMalcolmLevelSetSurfaceTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetSurfaceTest
  DATA{Input/cells.png} 60 Malcolm
  ${TEMP}/vtkVisualize2DCellsMalcolmLevelSetSurfaceTest.png
  )
itk_add_test(NAME vtkVisualize2DCellsMalcolmLevelSetLayersTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver vtkVisualize2DCellsLevelSetLayersTest
  DATA{Input/cells.png} 60 Malcolm
  )

itk_add_test(NAME itkVTKVisualizeLevelSetsInteractivePauseTest
  COMMAND ITKLevelSetsv4VisualizationTestDriver itkVTKVisualizeLevelSetsInteractivePauseTest
  DATA{Input/cells.png} 30
  )

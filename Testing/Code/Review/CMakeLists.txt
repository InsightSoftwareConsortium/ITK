IF( NOT ITK_DISABLE_CXX_TESTING )
IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Review ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Review)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)


SET(REVIEW_TESTS ${CXX_TEST_PATH}/itkReviewTests)
SET(REVIEW_PRINT_TEST ${CXX_TEST_PATH}/itkReviewPrintTest)
SET(REVIEW_HEADER_TESTS ${CXX_TEST_PATH}/itkReviewHeaderTest)

ADD_TEST(itkReviewPrintTest ${REVIEW_PRINT_TEST})

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Review)
SET(INPUTDATA ${ITK_DATA_ROOT}/Input)
SET(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

#
#  List of Tests in this directory
#

SET(ReviewTest_SRCS
  itkFlatStructuringElementTest.cxx

  itkLabelToRGBImageFilterTest.cxx
  itkLabelOverlayImageFilterTest.cxx

  itkQuadEdgeTest1.cxx
  itkGeometricalQuadEdgeTest1.cxx
  itkQuadEdgeMeshAddFaceTest1.cxx
  itkQuadEdgeMeshAddFaceTest2.cxx
  itkQuadEdgeMeshBasicLayerTest.cxx
  itkQuadEdgeMeshDeleteEdgeTest.cxx
  itkQuadEdgeMeshFrontIteratorTest.cxx
  itkQuadEdgeMeshPointTest1.cxx
  itkQuadEdgeMeshTest1.cxx
  itkQuadEdgeMeshTest2.cxx
  itkQuadEdgeMeshTest3.cxx
  itkQuadEdgeMeshEulerOperatorsTest.cxx
  itkQuadEdgeMeshCellInterfaceTest.cxx
  itkQuadEdgeMeshIteratorTest.cxx
  itkQuadEdgeMeshPolygonCellTest.cxx
  itkDynamicQuadEdgeMeshTest.cxx
  itkAutomaticTopologyQuadEdgeMeshSourceTest.cxx
  itkRegularSphereQuadEdgeMeshSourceTest.cxx

  itkContourExtractor2DImageFilterTest.cxx

  itkValuedRegionalMinimaImageFilterTest.cxx
  itkValuedRegionalMaximaImageFilterTest.cxx
  itkRegionalMaximaImageFilterTest.cxx
  itkRegionalMaximaImageFilterTest2.cxx
  itkRegionalMinimaImageFilterTest.cxx
  itkRegionalMinimaImageFilterTest2.cxx

  itkNeuralNetworkIOTest.cxx

  itkConformalFlatteningMeshFilterTest.cxx

  itkVTKPolyDataIOQuadEdgeMeshTest.cxx
  itkVTKPolyDataReaderQuadEdgeMeshTest.cxx
  itkVTKPolyDataReaderTest.cxx
  itkVTKPolyDataWriterTest.cxx

  itkMorphologicalWatershedImageFilterTest.cxx
  itkMorphologicalWatershedFromMarkersImageFilterTest.cxx

  itkBinaryMorphologicalClosingImageFilterTest.cxx
  itkBinaryMorphologicalOpeningImageFilterTest.cxx

  itkOptImageToImageMetricsTest.cxx

  itkTimeAndMemoryProbeTest.cxx
)

IF(ITK_USE_TRANSFORM_IO_FACTORIES)
  SET(ReviewTest_SRCS ${ReviewTest_SRCS} itkTransformFileReaderWriterTest.cxx)
ENDIF(ITK_USE_TRANSFORM_IO_FACTORIES)


ADD_TEST(itkOptImageToImageMetricsTest01 
  ${REVIEW_TESTS} itkOptImageToImageMetricsTest 
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png 
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

ADD_TEST(itkOptImageToImageMetricsTest02 
  ${REVIEW_TESTS} 
  --with-threads 2
  itkOptImageToImageMetricsTest 
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png 
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

ADD_TEST(itkOptImageToImageMetricsTest03 
  ${REVIEW_TESTS} 
  --without-threads
  itkOptImageToImageMetricsTest 
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png 
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

IF(ITK_USE_MINC2 AND MINC2_FOUND)
  SET(ReviewTest_SRCS ${ReviewTest_SRCS} itkMINC2ImageIOTest.cxx)
ENDIF(ITK_USE_MINC2 AND MINC2_FOUND)

ADD_TEST(itkFlatStructuringElementTest ${REVIEW_TESTS}
  itkFlatStructuringElementTest
  )
ADD_TEST(itkQuadEdgeTest1 ${REVIEW_TESTS} itkQuadEdgeTest1)
ADD_TEST(itkGeometricalQuadEdgeTest1 ${REVIEW_TESTS}
  itkGeometricalQuadEdgeTest1)
ADD_TEST(itkQuadEdgeMeshPointTest1 ${REVIEW_TESTS}
  itkQuadEdgeMeshPointTest1)
ADD_TEST(itkQuadEdgeMeshAddFaceTest1 ${REVIEW_TESTS}
  itkQuadEdgeMeshAddFaceTest1)
ADD_TEST(itkQuadEdgeMeshAddFaceTest2 ${REVIEW_TESTS}
  itkQuadEdgeMeshAddFaceTest2)
ADD_TEST(itkQuadEdgeMeshDeleteEdgeTest ${REVIEW_TESTS}
  itkQuadEdgeMeshDeleteEdgeTest)
ADD_TEST(itkQuadEdgeMeshFrontIteratorTest ${REVIEW_TESTS}
  itkQuadEdgeMeshFrontIteratorTest)
ADD_TEST(itkQuadEdgeMeshTest1 ${REVIEW_TESTS} itkQuadEdgeMeshTest1)
ADD_TEST(itkQuadEdgeMeshTest2 ${REVIEW_TESTS} itkQuadEdgeMeshTest2)
ADD_TEST(itkQuadEdgeMeshTest3 ${REVIEW_TESTS} itkQuadEdgeMeshTest3)
ADD_TEST(itkQuadEdgeMeshEulerOperatorsTest ${REVIEW_TESTS}
  itkQuadEdgeMeshEulerOperatorsTest)
ADD_TEST(itkQuadEdgeMeshCellInterfaceTest ${REVIEW_TESTS}
  itkQuadEdgeMeshCellInterfaceTest)
ADD_TEST(itkQuadEdgeMeshIteratorTest ${REVIEW_TESTS}
  itkQuadEdgeMeshIteratorTest)
ADD_TEST(itkQuadEdgeMeshPolygonCellTest ${REVIEW_TESTS}
  itkQuadEdgeMeshPolygonCellTest)
ADD_TEST(itkDynamicQuadEdgeMeshTest ${REVIEW_TESTS}
  itkDynamicQuadEdgeMeshTest)
ADD_TEST(itkAutomaticTopologyQuadEdgeMeshSourceTest.cxx ${REVIEW_TESTS}
  itkAutomaticTopologyQuadEdgeMeshSourceTest)
ADD_TEST(itkRegularSphereQuadEdgeMeshSourceTest.cxx ${REVIEW_TESTS}
  itkRegularSphereQuadEdgeMeshSourceTest)

FOREACH(opacity 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
  STRING(REGEX REPLACE "\\." "_" op ${opacity})
  ADD_TEST(itkLabelOverlayImageFilterTest-Opacity-${op} ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1-${op}Overlay.png
                  ${TEMP}/cthead1-${op}Overlay.png
                  itkLabelOverlayImageFilterTest
                  ${INPUTDATA}/cthead1.png
                  ${INPUTDATA}/cthead1Label.png
                  ${opacity}
                  ${TEMP}/cthead1-${op}Overlay.png)
ENDFOREACH(opacity)

ADD_TEST(itkLabelToRGBImageFilterTest ${REVIEW_TESTS}
      --compare ${BASELINE}/cthead1Label-color.png
                ${TEMP}/cthead1Label-color.png
                itkLabelToRGBImageFilterTest
                ${INPUTDATA}/cthead1Label.png
                ${TEMP}/cthead1Label-color.png)

ADD_TEST(itkContourExtractor2DImageFilterTest ${REVIEW_TESTS}
                                  itkContourExtractor2DImageFilterTest
                                  ${INPUTDATA}/ContourExtractor2DTest.tif  )




ADD_TEST(itkNeuralNetworkIOTest ${REVIEW_TESTS}
                  itkNeuralNetworkIOTest
                  ${INPUTDATA}/xornet.txt
                  ${INPUTDATA}/xortest.txt
                  ${TEMP}
                  )

ADD_TEST(itkConformalFlatteningMeshFilterTest1 ${REVIEW_TESTS}
                 itkConformalFlatteningMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroMesh01ConformalFlattenedToSphere.vtk
                 319
                 100.0
                 1
                 )

ADD_TEST(itkConformalFlatteningMeshFilterTest2 ${REVIEW_TESTS}
                 itkConformalFlatteningMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroMesh01ConformalFlattenedToPlane.vtk
                 319
                 100.0
                 0
                 )

ADD_TEST(itkValuedRegionalMinimaImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMinimal-ref.png
                  ${TEMP}/cthead1ValuedRegionalMinimal.png
                  itkValuedRegionalMinimaImageFilterTest
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMinimal.png
                  ${TEMP}/cthead1ValuedRegionalMinimal-ref.png
                  )

ADD_TEST(itkValuedRegionalMinimaImageFilterTest2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMinimal-ref2.png
                  ${TEMP}/cthead1ValuedRegionalMinimal2.png
                  itkValuedRegionalMinimaImageFilterTest
                  0
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMinimal2.png
                  ${TEMP}/cthead1ValuedRegionalMinimal-ref2.png
                  )

ADD_TEST(itkValuedRegionalMaximaImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMaximal-ref.png
                  ${TEMP}/cthead1ValuedRegionalMaximal.png
                  itkValuedRegionalMaximaImageFilterTest
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMaximal.png
                  ${TEMP}/cthead1ValuedRegionalMaximal-ref.png
                  )

ADD_TEST(itkValuedRegionalMaximaImageFilterTest2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMaximal-ref2.png
                  ${TEMP}/cthead1ValuedRegionalMaximal2.png
                  itkValuedRegionalMaximaImageFilterTest
                  0
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMaximal2.png
                  ${TEMP}/cthead1ValuedRegionalMaximal-ref2.png
                  )

ADD_TEST(itkRegionalMinimaImageFilterTest2_1 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMinimal-ref2_1.png
                  ${TEMP}/cthead1RegionalMinimal2_1.png
                  itkRegionalMaximaImageFilterTest2
                  1
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMinimal2_1.png
                  ${TEMP}/cthead1RegionalMinimal-ref2_1.png
                  )
ADD_TEST(itkRegionalMinimaImageFilterTest2_2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMinimal-ref2_2.png
                  ${TEMP}/cthead1RegionalMinimal2_2.png
                  itkRegionalMaximaImageFilterTest2
                  0
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMinimal2_2.png
                  ${TEMP}/cthead1RegionalMinimal-ref2_2.png
                  )

ADD_TEST(itkRegionalMaximaImageFilterTest2_1 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMaximal-ref2_1.png
                  ${TEMP}/cthead1RegionalMaximal2_1.png
                  itkRegionalMaximaImageFilterTest2
                  1
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMaximal2_1.png
                  ${TEMP}/cthead1RegionalMaximal-ref2_1.png
                  )
ADD_TEST(itkRegionalMaximaImageFilterTest2_2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMaximal-ref2_2.png
                  ${TEMP}/cthead1RegionalMaximal2_2.png
                  itkRegionalMaximaImageFilterTest2
                  0
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMaximal2_2.png
                  ${TEMP}/cthead1RegionalMaximal-ref2_2.png
                  )

ADD_TEST(itkVTKPolyDataIOQuadEdgeMeshTest ${REVIEW_TESTS}
  itkVTKPolyDataIOQuadEdgeMeshTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  ${TEMP}/VTKPolyDataIOTest.vtk)

ADD_TEST(itkVTKPolyDataReaderQuadEdgeMeshTest ${REVIEW_TESTS}
  itkVTKPolyDataReaderQuadEdgeMeshTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  )

ADD_TEST(itkVTKPolyDataReaderTest ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  )

ADD_TEST(itkVTKPolyDataWriterTest01 ${REVIEW_TESTS}
  itkVTKPolyDataWriterTest
  ${TEMP}/VTKPolyDataWriterTest01.vtk
  )

FOREACH(M 0 1)
  FOREACH(F 0 1)
    ADD_TEST(itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/button-hole.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      ${M} ${F} 0
                      )
    
    ADD_TEST(itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/pass-values.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      ${M} ${F} 0
                      )
    
    ADD_TEST(itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/plateau.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      ${M} ${F} 0
                      )
    
    ADD_TEST(itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/thick-lines.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      ${M} ${F} 0
                      )
    
    ADD_TEST(itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      itkMorphologicalWatershedFromMarkersImageFilterTest
                      ${INPUTDATA}/cthead1.png
                      ${INPUTDATA}/cthead1-markers.png
                      ${TEMP}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      ${M} ${F}
                      )
  ENDFOREACH(F)
ENDFOREACH(M)

FOREACH(level 00 10 20 30 40 50)
  ADD_TEST(itkMorphologicalWatershedImageFilterTestLevel${level} ${REVIEW_TESTS}
          --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    ${TEMP}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    itkMorphologicalWatershedImageFilterTest
                    ${INPUTDATA}/level.png
                    ${TEMP}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    1 0 ${level}
                    )
ENDFOREACH(level)

ADD_TEST(itkBinaryMorphologicalClosingImageFilterTest$ ${REVIEW_TESTS}
        --compare ${BASELINE}/itkBinaryMorphologicalClosingImageFilterTest.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTest.png
                  itkBinaryMorphologicalClosingImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTest.png
                  40 1 200
                  )

ADD_TEST(itkBinaryMorphologicalClosingImageFilterTestUnsafe$ ${REVIEW_TESTS}
        --compare ${BASELINE}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  itkBinaryMorphologicalClosingImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  40 0 200
                  )

ADD_TEST(itkBinaryMorphologicalOpeningImageFilterTest$ ${REVIEW_TESTS}
        --compare ${BASELINE}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  ${TEMP}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  itkBinaryMorphologicalOpeningImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  8 150 200
                  )
ADD_TEST(itkTimeAndMemoryProbeTest1 ${REVIEW_TESTS} 
        itkTimeAndMemoryProbeTest
        )

ADD_EXECUTABLE(itkReviewPrintTest itkReviewPrintTest.cxx)
TARGET_LINK_LIBRARIES(itkReviewPrintTest
  ITKAlgorithms
  ITKFEM
  ITKQuadEdgeMesh
  )

ADD_EXECUTABLE(itkReviewHeaderTest itkReviewHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkReviewHeaderTest
  ITKAlgorithms
  ITKFEM
  ITKQuadEdgeMesh
  )

ADD_TEST(itkReviewHeaderTest ${REVIEW_HEADER_TESTS})

ADD_EXECUTABLE(itkReviewTests itkReviewTests.cxx ${ReviewTest_SRCS})

SET(ReviewTest_LIBS 
  ITKAlgorithms
  ITKFEM
  ITKStatistics
  ITKQuadEdgeMesh
  )

IF(ITK_USE_MINC2 AND MINC2_FOUND)
  SET(ReviewTest_LIBS ${ReviewTest_LIBS} ITKIOMINC2)
ENDIF(ITK_USE_MINC2 AND MINC2_FOUND)


TARGET_LINK_LIBRARIES(itkReviewTests ${ReviewTest_LIBS})

ENDIF(NOT ITK_DISABLE_CXX_TESTING)

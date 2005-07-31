PROJECT(MeshExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(PointSet1 PointSet1.cxx )
TARGET_LINK_LIBRARIES(PointSet1 ITKCommon)

ADD_EXECUTABLE(PointSet2 PointSet2.cxx )
TARGET_LINK_LIBRARIES(PointSet2 ITKCommon)

ADD_EXECUTABLE(PointSet3 PointSet3.cxx )
TARGET_LINK_LIBRARIES(PointSet3 ITKCommon)

ADD_EXECUTABLE(RGBPointSet RGBPointSet.cxx )
TARGET_LINK_LIBRARIES(RGBPointSet ITKCommon)

ADD_EXECUTABLE(PointSetWithVectors PointSetWithVectors.cxx )
TARGET_LINK_LIBRARIES(PointSetWithVectors ITKCommon)

ADD_EXECUTABLE(PointSetWithCovariantVectors PointSetWithCovariantVectors.cxx )
TARGET_LINK_LIBRARIES(PointSetWithCovariantVectors ITKCommon)

ADD_EXECUTABLE(Mesh1 Mesh1.cxx )
TARGET_LINK_LIBRARIES(Mesh1 ITKCommon)

ADD_EXECUTABLE(Mesh2 Mesh2.cxx )
TARGET_LINK_LIBRARIES(Mesh2 ITKCommon)

ADD_EXECUTABLE(Mesh3 Mesh3.cxx )
TARGET_LINK_LIBRARIES(Mesh3 ITKCommon)

ADD_EXECUTABLE(MeshTraits MeshTraits.cxx )
TARGET_LINK_LIBRARIES(MeshTraits ITKCommon)

ADD_EXECUTABLE(MeshKComplex MeshKComplex.cxx )
TARGET_LINK_LIBRARIES(MeshKComplex ITKCommon)

ADD_EXECUTABLE(MeshPolyLine MeshPolyLine.cxx )
TARGET_LINK_LIBRARIES(MeshPolyLine ITKCommon)

ADD_EXECUTABLE(MeshCellsIteration MeshCellsIteration.cxx )
TARGET_LINK_LIBRARIES(MeshCellsIteration ITKCommon)

ADD_EXECUTABLE(MeshCellVisitor MeshCellVisitor.cxx )
TARGET_LINK_LIBRARIES(MeshCellVisitor ITKCommon)

ADD_EXECUTABLE(MeshCellVisitor2 MeshCellVisitor2.cxx )
TARGET_LINK_LIBRARIES(MeshCellVisitor2 ITKCommon)

ADD_EXECUTABLE(AutomaticMesh AutomaticMesh.cxx )
TARGET_LINK_LIBRARIES(AutomaticMesh ITKCommon ITKAlgorithms)

ADD_EXECUTABLE(ImageToPointSet ImageToPointSet.cxx )
TARGET_LINK_LIBRARIES(ImageToPointSet ITKCommon ITKAlgorithms ITKIO)

SET(MESH_EXAMPLES ${CXX_TEST_PATH}/MeshExamples)

ADD_EXECUTABLE(MeshExamples MeshExamples.cxx )
TARGET_LINK_LIBRARIES(MeshExamples ITKIO ITKCommon)

IF( NOT ITK_DISABLE_CXX_TESTING )

ADD_TEST(AutomaticMeshTest ${CXX_TEST_PATH}/MeshExamples AutomaticMeshTest)

ADD_TEST(ImageToPointSetTest ${CXX_TEST_PATH}/MeshExamples
        ImageToPointSetTest ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png)

ADD_TEST(Mesh1Test ${CXX_TEST_PATH}/MeshExamples Mesh1Test)

ADD_TEST(Mesh2Test ${CXX_TEST_PATH}/MeshExamples Mesh2Test)

ADD_TEST(Mesh3Test ${CXX_TEST_PATH}/MeshExamples Mesh3Test)

ADD_TEST(MeshCellsIterationTest ${CXX_TEST_PATH}/MeshExamples MeshCellsIterationTest)

ADD_TEST(MeshCellVisitorTest ${CXX_TEST_PATH}/MeshExamples MeshCellVisitorTest)

ADD_TEST(MeshCellVisitor2Test ${CXX_TEST_PATH}/MeshExamples MeshCellVisitor2Test)

ADD_TEST(MeshKComplexTest ${CXX_TEST_PATH}/MeshExamples MeshKComplexTest)

ADD_TEST(MeshPolyLineTest ${CXX_TEST_PATH}/MeshExamples MeshPolyLineTest)

ADD_TEST(MeshTraitsTest ${CXX_TEST_PATH}/MeshExamples MeshTraitsTest)

ADD_TEST(PointSet1Test ${CXX_TEST_PATH}/MeshExamples PointSet1Test)

ADD_TEST(PointSet2Test ${CXX_TEST_PATH}/MeshExamples PointSet2Test)

ADD_TEST(PointSet3Test ${CXX_TEST_PATH}/MeshExamples PointSet3Test)

ADD_TEST(PointSetWithCovariantVectorsTest ${CXX_TEST_PATH}/MeshExamples PointSetWithCovariantVectorsTest)

ADD_TEST(PointSetWithVectorsTest ${CXX_TEST_PATH}/MeshExamples PointSetWithVectorsTest)

ADD_TEST(RGBPointSetTest ${CXX_TEST_PATH}/MeshExamples RGBPointSetTest)


ENDIF( NOT ITK_DISABLE_CXX_TESTING )




PROJECT(MeshExamples)

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

ADD_EXECUTABLE(MeshCellVisitor MeshCellVisitor.cxx )
TARGET_LINK_LIBRARIES(MeshCellVisitor ITKCommon)


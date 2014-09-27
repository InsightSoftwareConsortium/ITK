project(MeshExamples)

add_executable(PointSet1 PointSet1.cxx )
target_link_libraries(PointSet1 ${ITK_LIBRARIES})

add_executable(PointSet2 PointSet2.cxx )
target_link_libraries(PointSet2 ${ITK_LIBRARIES})

add_executable(PointSet3 PointSet3.cxx )
target_link_libraries(PointSet3 ${ITK_LIBRARIES})

add_executable(RGBPointSet RGBPointSet.cxx )
target_link_libraries(RGBPointSet ${ITK_LIBRARIES})

add_executable(PointSetWithVectors PointSetWithVectors.cxx )
target_link_libraries(PointSetWithVectors ${ITK_LIBRARIES})

add_executable(PointSetWithCovariantVectors PointSetWithCovariantVectors.cxx )
target_link_libraries(PointSetWithCovariantVectors ${ITK_LIBRARIES})

add_executable(Mesh1 Mesh1.cxx )
target_link_libraries(Mesh1 ${ITK_LIBRARIES})

add_executable(Mesh2 Mesh2.cxx )
target_link_libraries(Mesh2 ${ITK_LIBRARIES})

add_executable(Mesh3 Mesh3.cxx )
target_link_libraries(Mesh3 ${ITK_LIBRARIES})

add_executable(MeshTraits MeshTraits.cxx )
target_link_libraries(MeshTraits ${ITK_LIBRARIES})

add_executable(MeshKComplex MeshKComplex.cxx )
target_link_libraries(MeshKComplex ${ITK_LIBRARIES})

add_executable(MeshPolyLine MeshPolyLine.cxx )
target_link_libraries(MeshPolyLine ${ITK_LIBRARIES})

add_executable(MeshCellsIteration MeshCellsIteration.cxx )
target_link_libraries(MeshCellsIteration ${ITK_LIBRARIES})

add_executable(MeshCellVisitor MeshCellVisitor.cxx )
target_link_libraries(MeshCellVisitor ${ITK_LIBRARIES})

add_executable(MeshCellVisitor2 MeshCellVisitor2.cxx )
target_link_libraries(MeshCellVisitor2 ${ITK_LIBRARIES})

add_executable(AutomaticMesh AutomaticMesh.cxx )
target_link_libraries(AutomaticMesh ${ITK_LIBRARIES})

add_executable(ImageToPointSet ImageToPointSet.cxx )
target_link_libraries(ImageToPointSet ${ITK_LIBRARIES})


if(BUILD_TESTING)
  add_subdirectory(test)
endif()

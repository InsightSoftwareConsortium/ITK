cmake_minimum_required(VERSION 3.16)
project(mesh-to-polydata)

set(CMAKE_CXX_STANDARD 17)

find_package(ITK REQUIRED
  COMPONENTS
    WebAssemblyInterface
    MeshToPolyData
    )
include(${ITK_USE_FILE})

add_executable(mesh-to-polydata mesh-to-polydata.cxx)
target_link_libraries(mesh-to-polydata PUBLIC ${ITK_LIBRARIES})

add_executable(polydata-to-mesh polydata-to-mesh.cxx)
target_link_libraries(polydata-to-mesh PUBLIC ${ITK_LIBRARIES})

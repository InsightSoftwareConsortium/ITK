cmake_minimum_required(VERSION 3.16)
project(mesh-to-poly-data)

set(CMAKE_CXX_STANDARD 17)

find_package(ITK REQUIRED
  COMPONENTS
    WebAssemblyInterface
    MeshToPolyData
    )
include(${ITK_USE_FILE})

add_executable(mesh-to-poly-data mesh-to-poly-data.cxx)
target_link_libraries(mesh-to-poly-data PUBLIC ${ITK_LIBRARIES})

add_executable(poly-data-to-mesh poly-data-to-mesh.cxx)
target_link_libraries(poly-data-to-mesh PUBLIC ${ITK_LIBRARIES})

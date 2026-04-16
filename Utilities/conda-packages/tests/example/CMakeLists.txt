cmake_minimum_required(VERSION 3.16)
project(ITKCondaTest CXX)

find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

add_executable(itkCondaTest main.cxx)
target_link_libraries(itkCondaTest ${ITK_LIBRARIES})

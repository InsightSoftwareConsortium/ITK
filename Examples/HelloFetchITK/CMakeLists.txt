cmake_minimum_required(VERSION 3.22.1...3.29.0 FATAL_ERROR)

# This project is designed to be built outside the Insight source tree.
project(HelloFetchITK)

# Use an existing ITK installation if specified with ITK_DIR, or fetch and configure ITK.
include(ITKFetchContents.cmake)

itk_generate_factory_registration()

add_executable(HelloFetchITK FetchWorld.cxx)

# If ITK was fetched and configure, only the required modules are built.
target_link_libraries(HelloFetchITK ITK::ITKCommonModule)

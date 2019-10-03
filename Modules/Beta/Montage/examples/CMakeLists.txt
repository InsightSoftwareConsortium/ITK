cmake_minimum_required(VERSION 3.10.2)
project(ITKMontageExamples)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED COMPONENTS ITKImageIO ITKTransformIO Montage)
else()
  # when being built as part of ITK, ITKImageIO and ITKTransformIO
  # lists of modules are not yet ready, causing a configure error
  find_package(ITK REQUIRED COMPONENTS
    Montage
    ITKTestKernel # includes many I/O modules
    ITKIOTransformInsightLegacy
    ITKIOHDF5
    )
endif()
include(${ITK_USE_FILE})

add_executable(PhaseCorrelationImageRegistration PhaseCorrelationImageRegistration.cxx)
target_link_libraries(PhaseCorrelationImageRegistration ${ITK_LIBRARIES})

add_executable(CompleteMontage CompleteMontage.cxx)
target_link_libraries(CompleteMontage ${ITK_LIBRARIES})

add_executable(RefineMontage RefineMontage.cxx)
target_link_libraries(RefineMontage ${ITK_LIBRARIES})

add_executable(ResampleMontage ResampleMontage.cxx)
target_link_libraries(ResampleMontage ${ITK_LIBRARIES})

add_executable(CompareTileConfigurations CompareTileConfigurations.cxx)
target_link_libraries(CompareTileConfigurations ${ITK_LIBRARIES})

add_executable(ImageCompareCommand ImageCompareCommand.cxx)
target_link_libraries(ImageCompareCommand ${ITK_LIBRARIES})

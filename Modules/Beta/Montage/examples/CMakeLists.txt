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

add_executable(Montage2D Montage2D.cxx)
target_link_libraries(Montage2D ${ITK_LIBRARIES})

add_executable(RefineMontage2D RefineMontage2D.cxx)
target_link_libraries(RefineMontage2D ${ITK_LIBRARIES})

add_executable(ResampleMontage2D ResampleMontage2D.cxx)
target_link_libraries(ResampleMontage2D ${ITK_LIBRARIES})

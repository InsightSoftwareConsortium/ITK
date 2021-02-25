cmake_minimum_required(VERSION 3.10.2)
project(ITKMontageExamples)

set(ExampleSpecificComponents
  ITKBiasCorrection
  ITKImageGrid
  ITKImageFeature
  )

if(NOT ITK_SOURCE_DIR)
  find_package(ITK 5.2 REQUIRED COMPONENTS Montage ITKImageIO ITKTransformIO ITKTestKernel ${ExampleSpecificComponents})
else()
  # when being built as part of ITK, ITKImageIO and ITKTransformIO
  # lists of modules are not yet ready, causing a configure error
  find_package(ITK REQUIRED COMPONENTS Montage ITKIOTransformInsightLegacy ITKIONRRD ITKIOPNG ITKTestKernel ${ExampleSpecificComponents})
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

add_executable(MontageImageCompareCommand MontageImageCompareCommand.cxx)
target_link_libraries(MontageImageCompareCommand ${ITK_LIBRARIES})


# add some regression tests
enable_testing()
set(TESTING_OUTPUT_PATH "${CMAKE_BINARY_DIR}/Testing/Temporary")

add_test(NAME ResampleMontageBlendingExplanationFromPaper
  COMMAND ResampleMontage
    ${CMAKE_CURRENT_LIST_DIR}/BlendingExplanation/TileConfiguration.registered.txt
    ${TESTING_OUTPUT_PATH}/BlendingExplanation.png)

add_test(NAME RefineMontage2D
  COMMAND RefineMontage
    ${CMAKE_CURRENT_LIST_DIR}/SampleData_CMUrun2/TileConfiguration.txt
    ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2.txt)
add_test(NAME RefineMontage2DCompare
  COMMAND CompareTileConfigurations
    ${CMAKE_CURRENT_LIST_DIR}/SampleData_CMUrun2/TileConfiguration.registered.txt
    ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2.txt)
set_tests_properties(RefineMontage2DCompare PROPERTIES DEPENDS RefineMontage2D)

add_test(NAME ResampleMontage2D
  COMMAND ResampleMontage
    ${CMAKE_CURRENT_LIST_DIR}/SampleData_CMUrun2/TileConfiguration.registered.txt
    ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2.nrrd)
add_test(NAME ResampleMontage2DCompare
  COMMAND MontageImageCompareCommand
    # a lot of tolerances are required because the reconstruction is not perfect
    --tolerance-radius 1 --tolerance-intensity 16 --tolerance-number-of-pixels 200
    --baseline-image ${CMAKE_CURRENT_LIST_DIR}/SampleData_CMUrun2/groundTruth.png
    --test-image ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2.nrrd)
set_tests_properties(ResampleMontage2DCompare PROPERTIES DEPENDS ResampleMontage2D)


add_test(NAME RefineMontage3D
  COMMAND RefineMontage
    ${CMAKE_CURRENT_LIST_DIR}/SampleData_DzZ_T1/TileConfiguration.txt
    ${TESTING_OUTPUT_PATH}/SampleData_DzZ_T1.txt)
add_test(NAME RefineMontage3DCompare
  COMMAND CompareTileConfigurations
    ${CMAKE_CURRENT_LIST_DIR}/SampleData_DzZ_T1/TileConfiguration.registered.txt
    ${TESTING_OUTPUT_PATH}/SampleData_DzZ_T1.txt)
set_tests_properties(RefineMontage3DCompare PROPERTIES DEPENDS RefineMontage3D)

add_test(NAME ResampleMontage3D
  COMMAND ResampleMontage
    ${CMAKE_CURRENT_LIST_DIR}/SampleData_DzZ_T1/TileConfiguration.registered.txt
    ${TESTING_OUTPUT_PATH}/SampleData_DzZ_T1.nrrd)
add_test(NAME ResampleMontage3DCompare
  COMMAND MontageImageCompareCommand
    # a lot of tolerances are required because the reconstruction is not perfect
    --tolerance-radius 1 --tolerance-intensity 64 --tolerance-number-of-pixels 300
    --baseline-image ${CMAKE_CURRENT_LIST_DIR}/SampleData_DzZ_T1/DzZ_T1_orig.nhdr
    --test-image ${TESTING_OUTPUT_PATH}/SampleData_DzZ_T1.nrrd)
set_tests_properties(ResampleMontage3DCompare PROPERTIES DEPENDS ResampleMontage3D)


file(MAKE_DIRECTORY ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2/)
add_test(NAME CompleteMontage2D
  COMMAND CompleteMontage
    ${CMAKE_CURRENT_LIST_DIR}/SampleData_CMUrun2/
    ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2
    CompleteMontage2D.nrrd
    0 0
    )
add_test(NAME CompleteMontage2DComparePositions
  COMMAND CompareTileConfigurations
    ${CMAKE_CURRENT_LIST_DIR}/SampleData_CMUrun2/TileConfiguration.registered.txt
    ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2/TileConfiguration.registered.txt)
set_tests_properties(CompleteMontage2DComparePositions PROPERTIES DEPENDS CompleteMontage2D)
add_test(NAME CompleteMontage2DCompareImage
  COMMAND MontageImageCompareCommand
    # a lot of tolerances are required because the reconstruction is not perfect
    --tolerance-radius 1 --tolerance-intensity 16 --tolerance-number-of-pixels 1000
    --baseline-image ${CMAKE_CURRENT_LIST_DIR}/SampleData_CMUrun2/groundTruth.png
    --test-image ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2/CompleteMontage2D.nrrd)
set_tests_properties(CompleteMontage2DCompareImage PROPERTIES DEPENDS CompleteMontage2D)


file(MAKE_DIRECTORY ${TESTING_OUTPUT_PATH}/SampleData_DzZ_T1/)
add_test(NAME CompleteMontage3D
  COMMAND CompleteMontage
    ${CMAKE_CURRENT_LIST_DIR}/SampleData_DzZ_T1/
    ${TESTING_OUTPUT_PATH}/SampleData_DzZ_T1
    CompleteMontage3D.nrrd
    0 0
    )
add_test(NAME CompleteMontage3DComparePositions
  COMMAND CompareTileConfigurations
    ${CMAKE_CURRENT_LIST_DIR}/SampleData_DzZ_T1/TileConfiguration.registered.txt
    ${TESTING_OUTPUT_PATH}/SampleData_DzZ_T1/TileConfiguration.registered.txt)
set_tests_properties(CompleteMontage3DComparePositions PROPERTIES DEPENDS CompleteMontage3D)
# add_test(NAME CompleteMontage3DCompareImage
#   COMMAND MontageImageCompareCommand
#     # a lot of tolerances are required because the reconstruction is not perfect
#     --tolerance-radius 1 --tolerance-intensity 16 --tolerance-number-of-pixels 1000
#     --baseline-image ${CMAKE_CURRENT_LIST_DIR}/SampleData_DzZ_T1/DzZ_T1_orig.nhdr
#     --test-image ${TESTING_OUTPUT_PATH}/SampleData_DzZ_T1/CompleteMontage3D.nrrd)
# set_tests_properties(CompleteMontage3DCompareImage PROPERTIES DEPENDS CompleteMontage3D)

if(ITK_WRAP_PYTHON)
  find_package(Python3 COMPONENTS Interpreter)
  file(MAKE_DIRECTORY ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2Python/)

  add_test(NAME CompleteMontage2DPython
    COMMAND ${Python3_EXECUTABLE}
      ${CMAKE_CURRENT_LIST_DIR}/SimpleMontage.py
      ${CMAKE_CURRENT_LIST_DIR}/SampleData_CMUrun2/
      ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2Python
      CompleteMontage2D.nrrd
      )

  add_test(NAME CompleteMontage2DComparePositionsPython
    COMMAND CompareTileConfigurations
      ${CMAKE_CURRENT_LIST_DIR}/SampleData_CMUrun2/TileConfiguration.registered.txt
      ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2Python/TileConfiguration.registered.txt)
  set_tests_properties(CompleteMontage2DComparePositions PROPERTIES DEPENDS CompleteMontage2D)
  add_test(NAME CompleteMontage2DCompareImagePython
    COMMAND MontageImageCompareCommand
      # a lot of tolerances are required because the reconstruction is not perfect
      --tolerance-radius 1 --tolerance-intensity 16 --tolerance-number-of-pixels 1000
      --baseline-image ${CMAKE_CURRENT_LIST_DIR}/SampleData_CMUrun2/groundTruth.png
      --test-image ${TESTING_OUTPUT_PATH}/SampleData_CMUrun2Python/CompleteMontage2D.nrrd)
  set_tests_properties(CompleteMontage2DCompareImage PROPERTIES DEPENDS CompleteMontage2D)
endif()

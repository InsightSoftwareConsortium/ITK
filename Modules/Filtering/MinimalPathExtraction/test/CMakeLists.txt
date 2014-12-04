#Set output path
set(TESTING_OUTPUT_PATH "${MinimalPath_BINARY_DIR}/Testing/Temporary")
make_directory(${TESTING_OUTPUT_PATH})

set(DATA_DIR ${CMAKE_SOURCE_DIR}/data)

set( _test
Test_SpeedToPath_GradientDescent_2D
Test_SpeedToPath_GradientDescent_3D
Test_SpeedToPath_IterateNeighborhood_2D
Test_SpeedToPath_IterateNeighborhood_3D
Test_SpeedToPath_RegularStepGradientDescent_2D
Test_SpeedToPath_RegularStepGradientDescent_3D
)

foreach( _t ${_test} )
  add_executable( ${_t} ${_t}.cxx )
  target_link_libraries( ${_t} ${ITK_LIBRARIES} )
endforeach()

#Add tests for the source images
#Noise-01
add_test(
  NAME Noise_01_NeighborhoodIterate
  COMMAND Test_SpeedToPath_IterateNeighborhood_2D
    ${TESTING_OUTPUT_PATH}/Noise-01-IterateNeighborhood.png
    ${DATA_DIR}/Noise-01-Speed.mhd
    ${DATA_DIR}/Noise-01.path
    1.0
    1.0
)
add_test(
  NAME Noise_01_GradientDescent
  COMMAND Test_SpeedToPath_GradientDescent_2D
    ${TESTING_OUTPUT_PATH}/Noise-01-GradientDescent.png
    ${DATA_DIR}/Noise-01-Speed.mhd
    ${DATA_DIR}/Noise-01.path
    1.0
    1000
)
add_test(
  NAME Noise_01_RegularStepGradientDescent
  COMMAND Test_SpeedToPath_RegularStepGradientDescent_2D
    ${TESTING_OUTPUT_PATH}/Noise-01-RegularStepGradientDescent.png
    ${DATA_DIR}/Noise-01-Speed.mhd
    ${DATA_DIR}/Noise-01.path
    1.0
    1000
    1.0
    0.5
)
#Synthetic-01
add_test(
  NAME Synthetic_01_NeighborhoodIterate
  COMMAND Test_SpeedToPath_IterateNeighborhood_2D
    ${TESTING_OUTPUT_PATH}/Synthetic-01-IterateNeighborhood.png
    ${DATA_DIR}/Synthetic-01-Speed.mhd
    ${DATA_DIR}/Synthetic-01.path
    1.0
    1.0
)
add_test(
  NAME Synthetic_01_GradientDescent
  COMMAND Test_SpeedToPath_GradientDescent_2D
    ${TESTING_OUTPUT_PATH}/Synthetic-01-GradientDescent.png
    ${DATA_DIR}/Synthetic-01-Speed.mhd
    ${DATA_DIR}/Synthetic-01.path
    1.0
    1000
)
add_test(
  NAME Synthetic_01_RegularStepGradientDescent
  COMMAND Test_SpeedToPath_RegularStepGradientDescent_2D
    ${TESTING_OUTPUT_PATH}/Synthetic-01-RegularStepGradientDescent.png
    ${DATA_DIR}/Synthetic-01-Speed.mhd
    ${DATA_DIR}/Synthetic-01.path
    1.0
    1000
    1.0
    0.5
)
#Synthetic-02
add_test(
  NAME Synthetic_02_NeighborhoodIterate
  COMMAND Test_SpeedToPath_IterateNeighborhood_2D
    ${TESTING_OUTPUT_PATH}/Synthetic-02-IterateNeighborhood.png
    ${DATA_DIR}/Synthetic-02-Speed.mhd
    ${DATA_DIR}/Synthetic-02.path
    1.0
    1.0
)
add_test(
  NAME Synthetic_02_GradientDescent
  COMMAND Test_SpeedToPath_GradientDescent_2D
    ${TESTING_OUTPUT_PATH}/Synthetic-02-GradientDescent.png
    ${DATA_DIR}/Synthetic-02-Speed.mhd
    ${DATA_DIR}/Synthetic-02.path
    1.0
    3000
)
add_test(
  NAME Synthetic_02_RegularStepGradientDescent
  COMMAND Test_SpeedToPath_RegularStepGradientDescent_2D
    ${TESTING_OUTPUT_PATH}/Synthetic-02-RegularStepGradientDescent.png
    ${DATA_DIR}/Synthetic-02-Speed.mhd
    ${DATA_DIR}/Synthetic-02.path
    1.0
    3000
    1.0
    0.5
)
#Synthetic-03
add_test(
  NAME Synthetic_03_NeighborhoodIterate
  COMMAND Test_SpeedToPath_IterateNeighborhood_2D
    ${TESTING_OUTPUT_PATH}/Synthetic-03-IterateNeighborhood.png
    ${DATA_DIR}/Synthetic-03-Speed.mhd
    ${DATA_DIR}/Synthetic-03.path
    1.0
    1.0
)
#Synthetic-04
add_test(
  NAME Synthetic_04_NeighborhoodIterate
  COMMAND Test_SpeedToPath_IterateNeighborhood_3D
    ${TESTING_OUTPUT_PATH}/Synthetic-04-IterateNeighborhood.mhd
    ${DATA_DIR}/Synthetic-04-Speed.mhd
    ${DATA_DIR}/Synthetic-04.path
    1.0
    1.0
)
add_test(
  NAME Synthetic_04_GradientDescent
  COMMAND Test_SpeedToPath_GradientDescent_3D
    ${TESTING_OUTPUT_PATH}/Synthetic-04-GradientDescent.mhd
    ${DATA_DIR}/Synthetic-04-Speed.mhd
    ${DATA_DIR}/Synthetic-04.path
    1.0
    3000
)
add_test(
  NAME Synthetic_04_RegularStepGradientDescent
  COMMAND Test_SpeedToPath_RegularStepGradientDescent_3D
    ${TESTING_OUTPUT_PATH}/Synthetic-04-RegularStepGradientDescent.mhd
    ${DATA_DIR}/Synthetic-04-Speed.mhd
    ${DATA_DIR}/Synthetic-04.path
    1.0
    3000
    1.0
    0.5
)
#Real-DSA-01-01
add_test(
  NAME Real_DSA_01_01_NeighborhoodIterate
  COMMAND Test_SpeedToPath_IterateNeighborhood_2D
    ${TESTING_OUTPUT_PATH}/Real-DSA-01-01-IterateNeighborhood.png
    ${DATA_DIR}/Real-DSA-01-Speed-01.mhd
    ${DATA_DIR}/Real-DSA-01.path
    1.0
    0.05
)
add_test(
  NAME Real_DSA_01_01_RegularStepGradientDescent
  COMMAND Test_SpeedToPath_RegularStepGradientDescent_2D
    ${TESTING_OUTPUT_PATH}/Real-DSA-01-01-RegularStepGradientDescent.png
    ${DATA_DIR}/Real-DSA-01-Speed-01.mhd
    ${DATA_DIR}/Real-DSA-01.path
    3.0
    4000
    0.25
    0.5
)
#Real-DSA-01-02
add_test(
  NAME Real_DSA_01_02_NeighborhoodIterate
  COMMAND Test_SpeedToPath_IterateNeighborhood_2D
    ${TESTING_OUTPUT_PATH}/Real-DSA-01-02-IterateNeighborhood.png
    ${DATA_DIR}/Real-DSA-01-Speed-02.mhd
    ${DATA_DIR}/Real-DSA-01.path
    1.0
    0.05
)
add_test(
  NAME Real_DSA_01_02_RegularStepGradientDescent
  COMMAND Test_SpeedToPath_RegularStepGradientDescent_2D
    ${TESTING_OUTPUT_PATH}/Real-DSA-01-02-RegularStepGradientDescent.png
    ${DATA_DIR}/Real-DSA-01-Speed-02.mhd
    ${DATA_DIR}/Real-DSA-01.path
    3.0
    4000
    0.25
    0.5
)

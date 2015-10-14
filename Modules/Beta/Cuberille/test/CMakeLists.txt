itk_module_test()

set(TESTING_OUTPUT_PATH "${Cuberille_BINARY_DIR}/Testing/Temporary")
set(DATA_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Input)

set(CuberilleTests
  CuberilleTest01.cxx
  )

CreateTestDriver(Cuberille "${Cuberille-Test_LIBRARIES}" "${CuberilleTests}")

# Configure the Tests
add_test(
  NAME Cuberille_Blob0_00
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/blob0.mha
    ${TESTING_OUTPUT_PATH}/blob0-01.vtk
    200   # Iso-surface value
    8     # Expected number of points
    6     # Expected number of cells
    0     # Generate triangle faces
    0     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Blob1_01
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/blob1.mha
    ${TESTING_OUTPUT_PATH}/blob1-01.vtk
    200   # Iso-surface value
    12    # Expected number of points
    10    # Expected number of cells
    0     # Generate triangle faces
    0     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Blob2_01
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/blob2.mha
    ${TESTING_OUTPUT_PATH}/blob2-01.vtk
    200   # Iso-surface value
    14    # Expected number of points
    12    # Expected number of cells
    0     # Generate triangle faces
    0     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Blob3_01
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/blob3.mha
    ${TESTING_OUTPUT_PATH}/blob3-01.vtk
    200   # Iso-surface value
    122   # Expected number of points
    180   # Expected number of cells
    0     # Generate triangle faces
    0     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Blob4_01
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/blob4.mha
    ${TESTING_OUTPUT_PATH}/blob4-01.vtk
    200   # Iso-surface value
    2124  # Expected number of points
    2122  # Expected number of cells
    0     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_MarschnerLobb_01
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/marschnerlobb.mha
    ${TESTING_OUTPUT_PATH}/marschnerlobb-01.vtk
    55    # Iso-surface value
    20524 # Expected number of points
    22104 # Expected number of cells
    0     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    200   # Maximum number of steps
)

add_test(
  NAME Cuberille_Fuel_01
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/fuel.mha
    ${TESTING_OUTPUT_PATH}/fuel-01.vtk
    15    # Iso-surface value
    5302  # Expected number of points
    5316  # Expected number of cells
    0     # Generate triangle faces
    0     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Fuel_02
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/fuel.mha
    ${TESTING_OUTPUT_PATH}/fuel-02.vtk
    15    # Iso-surface value
    5302  # Expected number of points
    5316  # Expected number of cells
    0     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Fuel_03
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/fuel.mha
    ${TESTING_OUTPUT_PATH}/fuel-03.vtk
    15    # Iso-surface value
    5302  # Expected number of points
    10632 # Expected number of cells
    1     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_HydrogenAtom_01
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/hydrogenAtom.mha
    ${TESTING_OUTPUT_PATH}/hydrogenAtom-01.vtk
    15    # Iso-surface value
    29880 # Expected number of points
    29874 # Expected number of cells
    0     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Neghip_01
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/neghip.mha
    ${TESTING_OUTPUT_PATH}/neghip-01.vtk
    55    # Iso-surface value
    15146 # Expected number of points
    15136 # Expected number of cells
    0     # Generate triangle faces
    0     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Neghip_02
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/neghip.mha
    ${TESTING_OUTPUT_PATH}/neghip-02.vtk
    55    # Iso-surface value
    15146 # Expected number of points
    15136 # Expected number of cells
    0     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Neghip_03
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/neghip.mha
    ${TESTING_OUTPUT_PATH}/neghip-03.vtk
    55    # Iso-surface value
    15146 # Expected number of points
    30272 # Expected number of cells
    1     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Nucleon_01
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/nucleon.mha
    ${TESTING_OUTPUT_PATH}/nucleon-01.vtk
    140   # Iso-surface value
    3504  # Expected number of points
    3500  # Expected number of cells
    0     # Generate triangle faces
    0     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Nucleon_02
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/nucleon.mha
    ${TESTING_OUTPUT_PATH}/nucleon-02.vtk
    140   # Iso-surface value
    3504  # Expected number of points
    3500  # Expected number of cells
    0     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Nucleon_03
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/nucleon.mha
    ${TESTING_OUTPUT_PATH}/nucleon-03.vtk
    140   # Iso-surface value
    3504  # Expected number of points
    7000  # Expected number of cells
    1     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Silicium_01
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/silicium.mha
    ${TESTING_OUTPUT_PATH}/silicium-01.vtk
    85    # Iso-surface value
    20036 # Expected number of points
    20024 # Expected number of cells
    0     # Generate triangle faces
    0     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Silicium_02
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/silicium.mha
    ${TESTING_OUTPUT_PATH}/silicium-02.vtk
    85    # Iso-surface value
    20036 # Expected number of points
    20024 # Expected number of cells
    0     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

add_test(
  NAME Cuberille_Silicium_03
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/silicium.mha
    ${TESTING_OUTPUT_PATH}/silicium-03.vtk
    85    # Iso-surface value
    20036 # Expected number of points
    40048 # Expected number of cells
    1     # Generate triangle faces
    1     # Project vertices to iso-surface
    0.2   # Surface distance threshold
    0.24  # Step length
    0.95  # Step length relaxation factor
    100   # Maximum number of steps
)

#add_test(
#  Cuberille_Engine_01
#  CuberilleTest01
#  Test01
#  ${DATA_DIR}/engine2.mha
#  ${TESTING_OUTPUT_PATH}/engine2-01.vtk
#  115    # Iso-surface value
#  319918 # Expected number of points
#  640112 # Expected number of cells
#  0      # Generate triangle faces
#  1      # Project vertices to iso-surface
#  0.2    # Surface distance threshold
#  0.24   # Step length
#  0.95   # Step length relaxation factor
#  100    # Maximum number of steps
#)

#add_test(
#  Cuberille_Bunny_01
#  CuberilleTest01
#  Test01
#  ${DATA_DIR}/bunny3.mha
#  ${TESTING_OUTPUT_PATH}/bunny3-01.vtk
#  155     # unsigned char
#  #1550  # signed short
#  1021438 # Expected number of points
#  2042892 # Expected number of cells
#  1       # Generate triangle faces
#  1       # Project vertices to iso-surface
#  0.10    # Surface distance threshold
#  0.15    # Step length
#  0.85    # Step length relaxation factor
#  25      # Maximum number of steps
#)

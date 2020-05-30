itk_module_test()

set(DATA_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Input)

set(CuberilleTests
  CuberilleTest01.cxx
  CuberilleTest02.cxx
  CuberilleTest03.cxx
  CuberilleTest04.cxx
  )

CreateTestDriver(Cuberille "${Cuberille-Test_LIBRARIES}" "${CuberilleTests}")

# Configure the Tests
add_test(
  NAME Cuberille_Blob0_00
  COMMAND CuberilleTestDriver CuberilleTest01
    ${DATA_DIR}/blob0.mha
    ${ITK_TEST_OUTPUT_DIR}/blob0-01.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/blob1-01.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/blob2-01.vtk
    200   # Iso-surface value
    16    # Expected number of points
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
    ${ITK_TEST_OUTPUT_DIR}/blob3-01.vtk
    200   # Iso-surface value
    240   # Expected number of points
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
    ${ITK_TEST_OUTPUT_DIR}/blob4-01.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/marschnerlobb-01.vtk
    55    # Iso-surface value
    22106 # Expected number of points
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
    ${ITK_TEST_OUTPUT_DIR}/fuel-01.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/fuel-02.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/fuel-03.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/hydrogenAtom-01.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/neghip-01.vtk
    55    # Iso-surface value
    15164 # Expected number of points
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
    ${ITK_TEST_OUTPUT_DIR}/neghip-02.vtk
    55    # Iso-surface value
    15164 # Expected number of points
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
    ${ITK_TEST_OUTPUT_DIR}/neghip-03.vtk
    55    # Iso-surface value
    15164 # Expected number of points
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
    ${ITK_TEST_OUTPUT_DIR}/nucleon-01.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/nucleon-02.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/nucleon-03.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/silicium-01.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/silicium-02.vtk
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
    ${ITK_TEST_OUTPUT_DIR}/silicium-03.vtk
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
#  ${ITK_TEST_OUTPUT_DIR}/engine2-01.vtk
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
#  ${ITK_TEST_OUTPUT_DIR}/bunny3-01.vtk
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

itk_add_test(NAME CuberilleTestDirectionMatrix
  COMMAND ${itk-module}TestDriver CuberilleTest02 )

itk_add_test(NAME CuberilleTestCellData
  COMMAND ${itk-module}TestDriver CuberilleTest03 )

itk_add_test(NAME CuberilleTestNonManifoldGeometry
  COMMAND ${itk-module}TestDriver CuberilleTest04 )

#Set output path
SET(TESTING_OUTPUT_PATH "${Cuberille_BINARY_DIR}/Testing/Temporary")
MAKE_DIRECTORY(${TESTING_OUTPUT_PATH})

# Configure the Executables
ADD_EXECUTABLE( CuberilleTest01 CuberilleTest01.cxx )
TARGET_LINK_LIBRARIES( CuberilleTest01 ${ITK_LIBRARIES} ITKQuadEdgeMesh )

# Configure the Tests
ADD_TEST(
  Cuberille_Blob0_00
  CuberilleTest01
  Test01
  ${DATA_PATH}/blob0.mha
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

ADD_TEST(
  Cuberille_Blob1_01
  CuberilleTest01
  Test01
  ${DATA_PATH}/blob1.mha
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

ADD_TEST(
  Cuberille_Blob2_01
  CuberilleTest01
  Test01
  ${DATA_PATH}/blob2.mha
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

ADD_TEST(
  Cuberille_Blob3_01
  CuberilleTest01
  Test01
  ${DATA_PATH}/blob3.mha
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

ADD_TEST(
  Cuberille_Blob4_01
  CuberilleTest01
  Test01
  ${DATA_PATH}/blob4.mha
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

ADD_TEST(
  Cuberille_MarschnerLobb_01
  CuberilleTest01
  Test01
  ${DATA_PATH}/marschnerlobb.mha
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

ADD_TEST(
  Cuberille_Fuel_01
  CuberilleTest01
  Test01
  ${DATA_PATH}/fuel.mha
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

ADD_TEST(
  Cuberille_Fuel_02
  CuberilleTest01
  Test01
  ${DATA_PATH}/fuel.mha
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

ADD_TEST(
  Cuberille_Fuel_03
  CuberilleTest01
  Test01
  ${DATA_PATH}/fuel.mha
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

ADD_TEST(
  Cuberille_HydrogenAtom_01
  CuberilleTest01
  Test01
  ${DATA_PATH}/hydrogenAtom.mha
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

ADD_TEST(
  Cuberille_Neghip_01
  CuberilleTest01
  Test01
  ${DATA_PATH}/neghip.mha
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

ADD_TEST(
  Cuberille_Neghip_02
  CuberilleTest01
  Test01
  ${DATA_PATH}/neghip.mha
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

ADD_TEST(
  Cuberille_Neghip_03
  CuberilleTest01
  Test01
  ${DATA_PATH}/neghip.mha
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

ADD_TEST(
  Cuberille_Nucleon_01
  CuberilleTest01
  Test01
  ${DATA_PATH}/nucleon.mha
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

ADD_TEST(
  Cuberille_Nucleon_02
  CuberilleTest01
  Test01
  ${DATA_PATH}/nucleon.mha
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

ADD_TEST(
  Cuberille_Nucleon_03
  CuberilleTest01
  Test01
  ${DATA_PATH}/nucleon.mha
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

ADD_TEST(
  Cuberille_Silicium_01
  CuberilleTest01
  Test01
  ${DATA_PATH}/silicium.mha
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

ADD_TEST(
  Cuberille_Silicium_02
  CuberilleTest01
  Test01
  ${DATA_PATH}/silicium.mha
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

ADD_TEST(
  Cuberille_Silicium_03
  CuberilleTest01
  Test01
  ${DATA_PATH}/silicium.mha
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

#ADD_TEST(
#  Cuberille_Engine_01
#  CuberilleTest01
#  Test01
#  ${DATA_PATH}/engine2.mha
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

#ADD_TEST(
#  Cuberille_Bunny_01
#  CuberilleTest01
#  Test01
#  ${DATA_PATH}/bunny3.mha
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

itk_module_test()

set(TEST_DATA_ROOT "${${itk-module}_SOURCE_DIR}/Data")

createtestdriver(${itk-module} "${${itk-module}-Test_LIBRARIES}" "${ITK${itk-module}Tests}")

#####################################
#
# ADD TESTS
#
#####################################
set(BASELINE ${TEST_DATA_ROOT}/Baseline)
set(INPUTDIR ${TEST_DATA_ROOT}/Input)
set(TEMP ${CMAKE_BINARY_DIR}/Testing/Temporary)

#####################################
# 2D tests
#####################################
set(
  COMMON_PARAMS2D
  -F
  ${INPUTDIR}/img1.png
  -M
  ${INPUTDIR}/img2.png
  -l
  4
  -p
  1
  -g
  0.00001
)
set(EXT tif)

# Active Thirion forces and gaussian smoothing
set(TESTNAME VariationalRegistrationGaussian2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT}
    $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -v
    1.5
    -u
    0
    -W
    ${TEMP}/${TESTNAME}.${EXT}
)

# Active Thirion forces and diffusive regularization
set(TESTNAME VariationalRegistrationDiffusive2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT}
    $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -r
    1
    -a
    1.5
    -W
    ${TEMP}/${TESTNAME}.${EXT}
)

# Active Thirion forces and elastic regularization
set(TESTNAME VariationalRegistrationElastic2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT}
    $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -r
    2
    -m
    0.5
    -b
    1.0
    -W
    ${TEMP}/${TESTNAME}.${EXT}
)

# Passive Thirion forces and gaussian smoothing
set(TESTNAME VariationalRegistrationPassiveDemons2D)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT}
    $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -d
    1
    -v
    1.5
    -u
    0
    -W
    ${TEMP}/${TESTNAME}.${EXT}
)

# Symmetric Thirion forces and gaussian smoothing
set(TESTNAME VariationalRegistrationSymmetricDemons2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT}
    $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -d
    2
    -v
    1.5
    -u
    0
    -W
    ${TEMP}/${TESTNAME}.${EXT}
)

# SSD forces and gaussian smoothing
set(TESTNAME VariationalRegistrationSSD2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT}
    $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -f
    1
    -t
    0.001
    -v
    1.0
    -u
    0
    -W
    ${TEMP}/${TESTNAME}.${EXT}
)

# NCC forces and gaussian smoothing
set(TESTNAME VariationalRegistrationNCC2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT}
    $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -f
    2
    -t
    40
    -v
    1.5
    -u
    0
    -q
    3
    -p
    2
    -W
    ${TEMP}/${TESTNAME}.${EXT}
)

#####################################
# 3D tests
#####################################
set(
  COMMON_PARAMS3D
  -F
  ${INPUTDIR}/10-P_5x5x5.nii.gz
  -M
  ${INPUTDIR}/60-P_5x5x5.nii.gz
  -l
  3
  -p
  1
  -g
  0.00001
)
set(EXT nii.gz)

# Active Thirion forces and gaussian smoothing
set(TESTNAME VariationalRegistrationGaussian3DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT}
    $<TARGET_FILE:itkVariationalRegistration>
    ${COMMON_PARAMS3D}
    -v
    1.5
    -u
    0
    -W
    ${TEMP}/${TESTNAME}.${EXT}
    -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

# Active Thirion forces and diffusive regularization
set(TESTNAME VariationalRegistrationDiffusive3DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.nii.gz
    ${TEMP}/${TESTNAME}.nii.gz
    $<TARGET_FILE:itkVariationalRegistration>
    ${COMMON_PARAMS3D}
    -r
    1
    -a
    1
    -W
    ${TEMP}/${TESTNAME}.nii.gz
    -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

# Active Thirion forces and elastic regularization
set(TESTNAME VariationalRegistrationElastic3DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.nii.gz
    ${TEMP}/${TESTNAME}.nii.gz
    $<TARGET_FILE:itkVariationalRegistration>
    ${COMMON_PARAMS3D}
    -r
    2
    -m
    0.25
    -b
    0.25
    -W
    ${TEMP}/${TESTNAME}.nii.gz
    -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

# SSD forces and gaussian smoothing
set(TESTNAME VariationalRegistrationSSD3DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT}
    $<TARGET_FILE:itkVariationalRegistration>
    ${COMMON_PARAMS3D}
    -f
    1
    -t
    0.00001
    -v
    1
    -u
    0
    -W
    ${TEMP}/${TESTNAME}.${EXT}
    -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

# NCC forces and gaussian smoothing
set(TESTNAME VariationalRegistrationNCC3DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT}
    $<TARGET_FILE:itkVariationalRegistration>
    ${COMMON_PARAMS3D}
    -f
    2
    -t
    40
    -v
    1
    -u
    0
    -q
    2
    -p
    1
    -W
    ${TEMP}/${TESTNAME}.${EXT}
    -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

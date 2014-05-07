itk_module_test()

set(
  ${itk-module}Tests
  VariationalRegistrationFilterTest.cxx
  VariationalRegistrationMultiResolutionFilterTest.cxx
)

# list(APPEND ExternalData_URL_TEMPLATES "https://googledrive.com/host/0B-qP96NdUKVfTFVZYUNyNEJoTlk/ExternalData_%(algo)_%(hash)")
# list(APPEND ExternalData_URL_TEMPLATES "file:///vol/medic01/users/aschmidt/Dropbox/ITKExternalData/ExternalData_%(algo)_%(hash)")

createtestdriver(${itk-module} "${${itk-module}-Test_LIBRARIES}" "${${itk-module}Tests}")

#####################################
#
# ADD TESTS
#
#####################################
set(TEMP ${CMAKE_BINARY_DIR}/Testing/Temporary)

itk_add_test(
  NAME VariationalRegistrationFilterTest
  COMMAND
    ${itk-module}TestDriver
    VariationalRegistrationFilterTest
)

itk_add_test(
  NAME VariationalRegistrationMultiResolutionFilterTest
  COMMAND
    ${itk-module}TestDriver
    VariationalRegistrationMultiResolutionFilterTest
)

#####################################
# 2D tests
#####################################
set(
  COMMON_PARAMS2D
  -F
  DATA{Input/img1.png}
  -M
  DATA{Input/img2.png}
  -l
  4
  -p
  1
  -g
  0.00001
)

# Active Thirion forces and gaussian smoothing
set(TESTNAME VariationalRegistrationGaussian2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    DATA{Baseline/${TESTNAME}.tif}
    ${TEMP}/${TESTNAME}.tif
    $<TARGET_FILE:VariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -r
    0
    -v
    1.5
    -u
    0
    -W
    ${TEMP}/${TESTNAME}.tif
)

# Active Thirion forces and diffusive regularization
set(TESTNAME VariationalRegistrationDiffusive2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    DATA{Baseline/${TESTNAME}.tif}
    ${TEMP}/${TESTNAME}.tif
    $<TARGET_FILE:VariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -r
    1
    -a
    1.5
    -W
    ${TEMP}/${TESTNAME}.tif
)

# Active Thirion forces and elastic regularization
set(TESTNAME VariationalRegistrationElastic2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    DATA{Baseline/${TESTNAME}.tif}
    ${TEMP}/${TESTNAME}.tif
    $<TARGET_FILE:VariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -r
    2
    -m
    0.5
    -b
    1.0
    -W
    ${TEMP}/${TESTNAME}.tif
)

# Passive Thirion forces and gaussian smoothing
set(TESTNAME VariationalRegistrationPassiveDemons2D)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    DATA{Baseline/${TESTNAME}.tif}
    ${TEMP}/${TESTNAME}.tif
    $<TARGET_FILE:VariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -d
    1
    -a
    1.5
    -W
    ${TEMP}/${TESTNAME}.tif
)

# Symmetric Thirion forces and gaussian smoothing
set(TESTNAME VariationalRegistrationSymmetricDemons2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    DATA{Baseline/${TESTNAME}.tif}
    ${TEMP}/${TESTNAME}.tif
    $<TARGET_FILE:VariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -d
    2
    -a
    1.5
    -W
    ${TEMP}/${TESTNAME}.tif
)

# SSD forces and gaussian smoothing
set(TESTNAME VariationalRegistrationSSD2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    DATA{Baseline/${TESTNAME}.tif}
    ${TEMP}/${TESTNAME}.tif
    $<TARGET_FILE:VariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -f
    1
    -t
    0.00075
    -a
    0.5
    -W
    ${TEMP}/${TESTNAME}.tif
)

# NCC forces and gaussian smoothing
set(TESTNAME VariationalRegistrationNCC2DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    DATA{Baseline/${TESTNAME}.tif}
    ${TEMP}/${TESTNAME}.tif
    $<TARGET_FILE:VariationalRegistration2D>
    ${COMMON_PARAMS2D}
    -f
    2
    -t
    40
    -a
    1.5
    -q
    3
    -p
    2
    -W
    ${TEMP}/${TESTNAME}.tif
)

#####################################
# 3D tests
#####################################
set(
  COMMON_PARAMS3D
  -F
  DATA{Input/10-P_5x5x5.nii.gz}
  -M
  DATA{Input/60-P_5x5x5.nii.gz}
  -l
  3
  -p
  1
  -g
  0.00001
)

# Active Thirion forces and gaussian smoothing
set(TESTNAME VariationalRegistrationGaussian3DTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    --compare
    DATA{Baseline/${TESTNAME}.nii.gz}
    ${TEMP}/${TESTNAME}.nii.gz
    $<TARGET_FILE:VariationalRegistration>
    ${COMMON_PARAMS3D}
    -r
    0
    -v
    1.5
    -u
    0
    -W
    ${TEMP}/${TESTNAME}.nii.gz
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
    DATA{Baseline/${TESTNAME}.nii.gz}
    ${TEMP}/${TESTNAME}.nii.gz
    $<TARGET_FILE:VariationalRegistration>
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
    DATA{Baseline/${TESTNAME}.nii.gz}
    ${TEMP}/${TESTNAME}.nii.gz
    $<TARGET_FILE:VariationalRegistration>
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
    DATA{Baseline/${TESTNAME}.nii.gz}
    ${TEMP}/${TESTNAME}.nii.gz
    $<TARGET_FILE:VariationalRegistration>
    ${COMMON_PARAMS3D}
    -f
    1
    -t
    0.00001
    -a
    1
    -W
    ${TEMP}/${TESTNAME}.nii.gz
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
    DATA{Baseline/${TESTNAME}.nii.gz}
    ${TEMP}/${TESTNAME}.nii.gz
    $<TARGET_FILE:VariationalRegistration>
    ${COMMON_PARAMS3D}
    -f
    2
    -t
    40
    -r
    0
    -v
    1
    -u
    0
    -q
    2
    -p
    1
    -W
    ${TEMP}/${TESTNAME}.nii.gz
    -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

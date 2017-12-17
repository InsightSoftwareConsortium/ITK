itk_module_test()

set(
  ${itk-module}Tests
  VariationalRegistrationFilterTest.cxx
  VariationalRegistrationMultiResolutionFilterTest.cxx
)

# both approaches do not work
# list(APPEND ExternalData_URL_TEMPLATES "http://jehrhardt.bplaced.net/VariationalRegistration/%(algo)/%(hash)")
# set(ExternalData_URL_TEMPLATES "http://jehrhardt.bplaced.net/VariationalRegistration/%(algo)/%(hash)")

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

# Print help test
set(TESTNAME VariationalRegistration2DPrintHelpTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    $<TARGET_FILE:VariationalRegistration2D>
    -?
)

# Active Thirion forces and gaussian smoothing, test 3D output
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
    -3
    -O
    ${TEMP}/${TESTNAME}.mha
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
if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
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
endif()

# Active Thirion forces and curvature regularization
if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  set(TESTNAME VariationalRegistrationCurvature2DTest)
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
      3
      -a
      1
      -W
      ${TEMP}/${TESTNAME}.tif
  )
endif()

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

# Active Thirion forces, diffusive regularization, diffeomorphic transform
set(TESTNAME VariationalRegistrationDiffeomorph2DTest)
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
    -s
    1
    -e
    2
    -W
    ${TEMP}/${TESTNAME}.tif
)

# Active Thirion forces, diffusive regularization, symmetric diffeomorphic transform
set(TESTNAME VariationalRegistrationSymDiff2DTest)
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
    -s
    2
    -e
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

# Print help test
set(TESTNAME VariationalRegistration3DPrintHelpTest)
itk_add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver
    $<TARGET_FILE:VariationalRegistration>
    -?
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
if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
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
endif()

# Active Thirion forces and curvature regularization
if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  set(TESTNAME VariationalRegistrationCurvature3DTest)
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
      3
      -a
      1
      -W
      ${TEMP}/${TESTNAME}.nii.gz
      -O
      ${TEMP}/${TESTNAME}_displ.mhd
  )
endif()

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

# Active Thirion forces and diffusive regularization, diffeomorphic transform
set(TESTNAME VariationalRegistrationDiffeomorph3DTest)
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
    -s
    1
    -e
    2
    -W
    ${TEMP}/${TESTNAME}.nii.gz
    -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

# Active Thirion forces and diffusive regularization, symmetric diffeomorphic transform
set(TESTNAME VariationalRegistrationSymDiff3DTest)
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
    -s
    2
    -e
    2
    -W
    ${TEMP}/${TESTNAME}.nii.gz
    -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

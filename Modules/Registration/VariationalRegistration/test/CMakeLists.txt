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

add_custom_target(
  BuildExecutablesUsedInTests
  COMMAND
    echo "Dummy target"
  DEPENDS
    VariationalRegistration
    VariationalRegistration2D
)

#####################################
# 2D tests
#####################################

function(Test2D TestName OutputExt)
  if(OutputExt)
    set(OutParam "${TEMP}/${TestName}.${OutputExt}")
  endif()

  itk_add_test(
    NAME ${TestName}
    COMMAND
      itkTestDriver
      --compare
      DATA{Baseline/${TestName}.tif}
      ${TEMP}/${TestName}.tif
      $<TARGET_FILE:VariationalRegistration2D>
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
      ${ARGN}
      -W
      ${TEMP}/${TestName}.tif
      -O
      ${OutParam}
  )

  set_tests_properties(
    ${TestName}
    PROPERTIES
      DEPENDS
        BuildExecutablesUsedInTests
  )
endfunction()

itk_add_test(
  NAME VariationalRegistration2DPrintHelpTest
  COMMAND
    $<TARGET_FILE:VariationalRegistration2D>
    -?
)

# Active Thirion forces and gaussian smoothing, test 3D output
test2d(VariationalRegistrationGaussian2DTest mha -r 0 -v 1.5 -u 0 -3)

# Active Thirion forces and diffusive regularization
test2d(VariationalRegistrationDiffusive2DTest "" -r 1 -a 1.5)

if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  # Active Thirion forces and elastic regularization
  test2d(VariationalRegistrationElastic2DTest "" -r 2 -m 0.5 -b 1.0)

  # Active Thirion forces and curvature regularization
  test2d(VariationalRegistrationCurvature2DTest "" -r 3 -a 1)
endif()

# Passive Thirion forces and gaussian smoothing
test2d(VariationalRegistrationPassiveDemons2D "" -d 1 -a 1.5)

# Symmetric Thirion forces and gaussian smoothing
test2d(VariationalRegistrationSymmetricDemons2DTest "" -d 2 -a 1.5)

# SSD forces and gaussian smoothing
test2d(VariationalRegistrationSSD2DTest "" -f 1 -t 0.00075 -a 0.5)

# NCC forces and gaussian smoothing
test2d(VariationalRegistrationNCC2DTest "" -f 2 -t 40 -a 1.5 -q 3 -p 2)

# Active Thirion forces, diffusive regularization, diffeomorphic transform
test2d(VariationalRegistrationDiffeomorph2DTest "" -r 1 -a 1.5 -s 1 -e 2)

# Active Thirion forces, diffusive regularization, symmetric diffeomorphic transform
test2d(VariationalRegistrationSymDiff2DTest "" -r 1 -a 1.5 -s 2 -e 2)

#####################################
# 3D tests
#####################################

function(Test3D TestName)
  itk_add_test(
    NAME ${TestName}
    COMMAND
      itkTestDriver
      --compare
      DATA{Baseline/${TestName}.nii.gz}
      ${TEMP}/${TestName}.nii.gz
      $<TARGET_FILE:VariationalRegistration>
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
      ${ARGN}
      -W
      ${TEMP}/${TestName}.nii.gz
      -O
      ${TEMP}/${TestName}_displ.mhd
  )

  set_tests_properties(
    ${TestName}
    PROPERTIES
      DEPENDS
        BuildExecutablesUsedInTests
  )
endfunction()

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

itk_add_test(
  NAME VariationalRegistration3DPrintHelpTest
  COMMAND
    $<TARGET_FILE:VariationalRegistration>
    -?
)

# Active Thirion forces and gaussian smoothing
test3d(VariationalRegistrationGaussian3DTest -r 0 -v 1.5 -u 0)

# Active Thirion forces and diffusive regularization
test3d(VariationalRegistrationDiffusive3DTest -r 1 -a 1)

if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  # Active Thirion forces and elastic regularization
  test3d(VariationalRegistrationElastic3DTest -r 2 -m 0.25 -b 0.25)

  # Active Thirion forces and curvature regularization
  # Disabled: baseline VariationalRegistrationCurvature3DTest.nii.gz is
  # orphaned (404 from gh-pages, Girder, and itk.org).
  # Test3D(VariationalRegistrationCurvature3DTest -r 3 -a 1)
endif()

# SSD forces and gaussian smoothing
test3d(VariationalRegistrationSSD3DTest -f 1 -t 0.00001 -a 1)

# NCC forces and gaussian smoothing
test3d(VariationalRegistrationNCC3DTest -f 2 -t 40 -r 0 -v 1 -u 0 -q 2 -p 1)

# Active Thirion forces and diffusive regularization, diffeomorphic transform
test3d(VariationalRegistrationDiffeomorph3DTest -r 1 -a 1 -s 1 -e 2)

# Active Thirion forces and diffusive regularization, symmetric diffeomorphic transform
test3d(VariationalRegistrationSymDiff3DTest -r 1 -a 1 -s 2 -e 2)

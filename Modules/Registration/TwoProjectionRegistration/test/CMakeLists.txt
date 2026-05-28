itk_module_test()

set(
  TwoProjectionRegistrationTests
  TwoProjection2D3DRegistration.cxx
  GetDRRSiddonJacobsRayTracing.cxx
)

createtestdriver(TwoProjectionRegistration "${TwoProjectionRegistration-Test_LIBRARIES}" "${TwoProjectionRegistrationTests}")

itk_add_test(
  NAME TwoProjection2D3DRegistrationDownSizedCTTest
  COMMAND
    TwoProjectionRegistrationTestDriver
    TwoProjection2D3DRegistration
    -res
    1
    1
    1
    1
    -iso
    99.62
    101.18
    65
    -o
    ${ITK_TEST_OUTPUT_DIR}/boxheadDRRDev1_G0_Reg.tif
    ${ITK_TEST_OUTPUT_DIR}/boxheadDRRDev1_G90_Reg.tif
    DATA{Input/boxheadDRRDev1_G0.tif}
    0
    DATA{Input/boxheadDRRDev1_G90.tif}
    90
    DATA{Input/BoxheadCT.img,BoxheadCT.hdr}
)

# Disabled: BoxheadCTFull.img is orphaned (404 from gh-pages, Girder, itk.org).
# itk_add_test(NAME TwoProjection2D3DRegistrationFullSizeCTTest
#   COMMAND TwoProjectionRegistrationTestDriver TwoProjection2D3DRegistration
#      -res 0.5 0.5 0.5 0.5
#      -iso 255 259 130
#      -o ${ITK_TEST_OUTPUT_DIR}/BoxheadDRRFullDev1_G0_Reg.tif ${ITK_TEST_OUTPUT_DIR}/BoxheadDRRFullDev1_G90_Reg.tif
#      DATA{Input/BoxheadDRRFullDev1_G0.tif} 0
#      DATA{Input/BoxheadDRRFullDev1_G90.tif} 90
#      DATA{Input/BoxheadCTFull.img,BoxheadCTFull.hdr}
#   )
# set_property(TEST TwoProjection2D3DRegistrationFullSizeCTTest APPEND PROPERTY LABELS RUNS_LONG)

itk_add_test(
  NAME GetDRRSiddonJacobsRayTracingDownSizedCTTest1
  COMMAND
    TwoProjectionRegistrationTestDriver
    GetDRRSiddonJacobsRayTracing
    -rp
    0
    -rx
    -3
    -ry
    4
    -rz
    2
    -t
    5
    5
    5
    -iso
    99.62
    101.18
    65
    -res
    1
    1
    -size
    256
    256
    -o
    ${ITK_TEST_OUTPUT_DIR}/boxheadDRRDev1_G0.tif
    DATA{Input/BoxheadCT.img,BoxheadCT.hdr}
)

itk_add_test(
  NAME GetDRRSiddonJacobsRayTracingDownSizedCTTest2
  COMMAND
    TwoProjectionRegistrationTestDriver
    GetDRRSiddonJacobsRayTracing
    -rp
    90
    -rx
    -3
    -ry
    4
    -rz
    2
    -t
    5
    5
    5
    -iso
    99.62
    101.18
    65
    -res
    1
    1
    -size
    256
    256
    -o
    ${ITK_TEST_OUTPUT_DIR}/boxheadDRRDev1_G90.tif
    DATA{Input/BoxheadCT.img,BoxheadCT.hdr}
)

# Disabled: BoxheadCTFull.img is orphaned (404 from gh-pages, Girder, itk.org).
# itk_add_test(NAME GetDRRSiddonJacobsRayTracingFullSizedCTTest1
#   COMMAND TwoProjectionRegistrationTestDriver GetDRRSiddonJacobsRayTracing
#     -rp 0 -rx -3 -ry 4 -rz 2 -t 5 5 5
#     -iso 255 259 130 -res 0.5 0.5
#     -size 512 512
#     -o ${ITK_TEST_OUTPUT_DIR}/BoxheadDRRFullDev1_G0.tif
#     DATA{Input/BoxheadCTFull.img,BoxheadCTFull.hdr}
#   )
#
# itk_add_test(NAME GetDRRSiddonJacobsRayTracingFullSizedCTTest2
#   COMMAND TwoProjectionRegistrationTestDriver GetDRRSiddonJacobsRayTracing
#     -rp 90 -rx -3 -ry 4 -rz 2 -t 5 5 5
#     -iso 255 259 130 -res 0.5 0.5
#     -size 512 512
#     -o ${ITK_TEST_OUTPUT_DIR}/BoxheadDRRFullDev1_G90.tif
#     DATA{Input/BoxheadCTFull.img,BoxheadCTFull.hdr}
#   )

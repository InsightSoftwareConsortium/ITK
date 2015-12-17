itk_module_test()

set(TwoProjectionRegistrationTests TwoProjection2D3DRegistration.cxx)

createtestdriver(TwoProjectionRegistration "${TwoProjectionRegistration_LIBRARIES}" "${TwoProjectionRegistrationTests}")

itk_add_test(
  NAME TwoProjection2D3DRegistrationTest1
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

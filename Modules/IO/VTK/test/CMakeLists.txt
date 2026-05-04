itk_module_test()
set(
  ITKIOVTKTests
  itkVTKImageIO2Test.cxx
  itkVTKImageIO2Test2.cxx
  itkVTKImageIOFileReadTest.cxx
  itkVTKImageIOStreamTest.cxx
  itkVTKImageIOTest.cxx
  itkVTKImageIOTest2.cxx
  itkVTKImageIOTest3.cxx
  itkVTIImageIOTest.cxx
  itkVTIImageIOReadWriteTest.cxx
)

createtestdriver(ITKIOVTK "${ITKIOVTK-Test_LIBRARIES}" "${ITKIOVTKTests}")

set(
  ITKIOVTKGTests
  itkVTIImageIODirectionGTest.cxx
  itkVTIImageIOFutureFeaturesGTest.cxx
  itkVTIImageIOGeneratedFixturesGTest.cxx
  itkVTIImageIOSwapBufferGTest.cxx
)
creategoogletestdriver(ITKIOVTK "${ITKIOVTK-Test_LIBRARIES}" "${ITKIOVTKGTests}")

# The GTest driver reads its fixtures from a single VTI_TEST_INPUT_DIR
# directory.  Now that the fixtures are .cid-backed (fetched via
# ExternalData rather than committed in-tree), resolve each fixture's
# build-tree path through ExternalData_Expand_Arguments and stage all
# of them into a single directory the driver can point at.
set(
  _vti_gtest_fixtures
  VTI_oblique_direction.vti
  VTI_oblique_direction.mhd
  VTI_oblique_direction.raw
  VTI_scalar_u8_appended_raw.vti
  VTI_scalar_u8_appended_raw.mhd
  VTI_scalar_u8_appended_raw.raw
  VTI_scalar_f32_zlib_appended.vti
  VTI_scalar_f32_zlib_appended.mhd
  VTI_scalar_f32_zlib_appended.raw
  VTI_rgba_u8_appended_raw.vti
  VTI_rgba_u8_appended_raw.mhd
  VTI_rgba_u8_appended_raw.raw
  VTI_vector3_f32_zlib_appended.vti
  VTI_vector3_f32_zlib_appended.mhd
  VTI_vector3_f32_zlib_appended.raw
  VTI_tensor_f32_ascii.vti
  VTI_tensor_f32_ascii.mhd
  VTI_tensor_f32_ascii.raw
  VTI_guard_lz4.vti
  VTI_guard_lzma.vti
  VTI_guard_multipiece.vti
  VTI_guard_unknown_compressor.vti
)
set(_vti_gtest_input_dir "${CMAKE_CURRENT_BINARY_DIR}/InputData")
file(MAKE_DIRECTORY "${_vti_gtest_input_dir}")
set(_vti_gtest_staged "")
foreach(_f IN LISTS _vti_gtest_fixtures)
  ExternalData_Expand_Arguments(
    ITKData
    _vti_resolved
    DATA{Input/${_f}}
  )
  set(_dst "${_vti_gtest_input_dir}/${_f}")
  add_custom_command(
    OUTPUT
      "${_dst}"
    COMMAND
      ${CMAKE_COMMAND} -E copy_if_different "${_vti_resolved}" "${_dst}"
    DEPENDS
      "${_vti_resolved}"
    COMMENT "Staging VTI GTest fixture ${_f}"
    VERBATIM
  )
  list(APPEND _vti_gtest_staged "${_dst}")
endforeach()
add_custom_target(ITKIOVTKGTestFixtures DEPENDS ${_vti_gtest_staged})
add_dependencies(ITKIOVTKGTestDriver ITKIOVTKGTestFixtures)
target_compile_definitions(
  ITKIOVTKGTestDriver
  PRIVATE
    VTI_TEST_INPUT_DIR="${_vti_gtest_input_dir}"
)

itk_add_test(
  NAME itkVTIImageIOTest
  COMMAND
    ITKIOVTKTestDriver
    itkVTIImageIOTest
    ${ITK_TEST_OUTPUT_DIR}
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTestCTHead1
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_CTHead1.vti
    itkVTIImageIOReadWriteTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_CTHead1.vti
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTestVisibleWomanEyeSlice
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VisibleWomanEyeSlice.vti
    itkVTIImageIOReadWriteTest
    DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VisibleWomanEyeSlice.vti
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTestHeadMRVolume
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_HeadMRVolume.vti
    itkVTIImageIOReadWriteTest
    DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_HeadMRVolume.vti
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTestVHFColor
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VHFColor.vti
    itkVTIImageIOReadWriteTest
    DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VHFColor.vti
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTest_reco2D_16line
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{Input/reco2D_16line.vti}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_reco2D_16line.vti
    itkVTIImageIOReadWriteTest
    DATA{Input/reco2D_16line.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_reco2D_16line.vti
)

itk_add_test(
  NAME itkVTIImageIOReadWriteTest_reco2D_16line_compressed
  COMMAND
    ITKIOVTKTestDriver
    --compare
    DATA{Input/reco2D_16line.vti}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_reco2D_16line_compressed.vti
    itkVTIImageIOReadWriteTest
    DATA{Input/reco2D_16line.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_reco2D_16line_compressed.vti
    1
)

# itkVTIImageIOReadWriteTestVHFColorZLib is temporarily disabled until
# Input/VHFColorZLib.vti can be published through ExternalData.  The
# ParaView-produced fixture exceeds the 100 KB direct-commit cap and
# content-link-upload.itk.org has been broken since the storacha->Pinata
# migration began; tracked at
# https://github.com/InsightSoftwareConsortium/ITK/issues/4340 and
# https://github.com/InsightSoftwareConsortium/cmake-w3-externaldata-upload/issues/3 .
#
# Equivalent code-path coverage (multi-component Float32 ZLib-compressed
# appended-raw with UInt64 header, via a non-ITK independent writer) is
# provided by itkVTIImageIOGeneratedFixturesTest's
# `vector3_f32_zlib_appended` case, which reads a Python-stdlib-generated
# 4x4x2 Vector<float,3> fixture and pixel-compares to its MetaIO oracle.
#
# TODO: restore this test once the ExternalData upload tool is operational;
# un-comment the block below and commit the .cid pointer to
# Modules/IO/VTK/test/Input/VHFColorZLib.vti.cid .
#
# itk_add_test(
#   NAME itkVTIImageIOReadWriteTestVHFColorZLib
#   COMMAND
#     ITKIOVTKTestDriver
#     --compare
#     DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw}
#     ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VHFColorZLib.vti
#     itkVTIImageIOReadWriteTest
#     DATA{Input/VHFColorZLib.vti}
#     ${ITK_TEST_OUTPUT_DIR}/itkVTIImageIOReadWriteTest_VHFColorZLib.vti
# )

itk_add_test(
  NAME itkVTKImageIO2Test
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIO2Test
    ${ITK_TEST_OUTPUT_DIR}
)
itk_add_test(
  NAME itkVTKImageIOTest
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIOTest
    ${ITK_TEST_OUTPUT_DIR}/test1.vtk
    ${ITK_TEST_OUTPUT_DIR}/test2.vtk
)
itk_add_test(
  NAME itkVTKImageIO2Test2
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIO2Test2
    ${ITK_TEST_OUTPUT_DIR}/itkVTKImageIO2Test2.vtk
)
itk_add_test(
  NAME itkVTKImageIOTest2
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIOTest2
    DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw}
    ${ITK_TEST_OUTPUT_DIR}/testVector.vtk
)
itk_add_test(
  NAME itkVTKImageIOTest3
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIOTest3
    DATA{Input/fibers.vtk}
)
itk_add_test(
  NAME itkVTKImageIOStreamTest
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIOStreamTest
    ${ITK_TEST_OUTPUT_DIR}/itkVTKImageIOStreamTest.vtk
)
itk_add_test(
  NAME itkVTKImageIOFileReadTest
  COMMAND
    ITKIOVTKTestDriver
    itkVTKImageIOFileReadTest
    DATA{Input/matrix.vtk}
    DATA{Input/ironProt.vtk}
)

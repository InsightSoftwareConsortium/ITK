itk_module_test()

set(
  ITKIOMeshFreeSurferTests
  itkFreeSurferMeshIOTest.cxx
  itkMeshFileReadWriteTest.cxx
)

createtestdriver(ITKIOMeshFreeSurfer "${ITKIOMeshFreeSurfer-Test_LIBRARIES}" "${ITKIOMeshFreeSurferTests}")

itk_add_test(
  NAME itkFreeSurferMeshIOTestAscii
  COMMAND
    ITKIOMeshFreeSurferTestDriver
    itkFreeSurferMeshIOTest
    DATA{Baseline/sphere.fsa}
    ${ITK_TEST_OUTPUT_DIR}/fsmeshiosphere.fsa
    DATA{Baseline/sphere.fsb}
    ${ITK_TEST_OUTPUT_DIR}/fsa2fsbsphere.fsb
    0
    0
    0
    0
    0
    162
    0
    320
    0
    0
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/fsmeshiosphere.fsa
    ${ITK_TEST_OUTPUT_DIR}/fsa2fsbsphere.fsb
)
itk_add_test(
  NAME itkFreeSurferMeshIOTestBinary
  COMMAND
    ITKIOMeshFreeSurferTestDriver
    itkFreeSurferMeshIOTest
    DATA{Baseline/sphere.fsb}
    ${ITK_TEST_OUTPUT_DIR}/fsmeshiosphere.fsb
    DATA{Baseline/sphere.fsa}
    ${ITK_TEST_OUTPUT_DIR}/fsb2fsasphere.fsa
    0
    0
    0
    0
    0
    162
    0
    320
    0
    1
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/fsmeshiosphere.fsb
    ${ITK_TEST_OUTPUT_DIR}/fsb2fsasphere.fsa
)
itk_add_test(
  NAME itkMeshFileReadWriteTestFreeSurfer1
  COMMAND
    ITKIOMeshFreeSurferTestDriver
    itkMeshFileReadWriteTest
    DATA{Baseline/sphere.fsb}
    ${ITK_TEST_OUTPUT_DIR}/sphere.fsb
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/sphere.fsb
)
itk_add_test(
  NAME itkMeshFileReadWriteTestFreeSurfer2
  COMMAND
    ITKIOMeshFreeSurferTestDriver
    itkMeshFileReadWriteTest
    DATA{Baseline/sphere.fsa}
    ${ITK_TEST_OUTPUT_DIR}/sphere.fsa
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/sphere.fsa
)

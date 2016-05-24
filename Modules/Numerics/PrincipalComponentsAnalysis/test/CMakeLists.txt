itk_module_test()

set(kit PrincipalComponentsAnalysis)
set(${kit}Tests itkVectorKernelPCATest.cxx)

set(TEST_DATA_ROOT ${${kit}_SOURCE_DIR}/Data)

createtestdriver(${kit} "${${kit}-Test_LIBRARIES}" "${${kit}Tests}")

itk_add_test(
  NAME itkVectorKernelPCATest
  COMMAND
    ${kit}TestDriver
    itkVectorKernelPCATest
    DATA{${TEST_DATA_ROOT}/PCATestSurface.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_01.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_02.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_03.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_04.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_05.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_06.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_07.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_08.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_09.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_10.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_11.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_12.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_13.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_14.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_15.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_16.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_17.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_18.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_19.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_20.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_21.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_22.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_23.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_24.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_25.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_26.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_27.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_28.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_29.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_30.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_31.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_32.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_33.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_34.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_35.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_36.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_37.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_38.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_39.vtk}
    DATA{${TEST_DATA_ROOT}/PCATestSurface_alpha0_40.vtk}
)

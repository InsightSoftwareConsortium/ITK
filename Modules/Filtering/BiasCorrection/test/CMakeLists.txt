itk_module_test()
set(ITKBiasCorrectionTests
itkCompositeValleyFunctionTest.cxx
itkMRIBiasFieldCorrectionFilterTest.cxx
itkN4BiasFieldCorrectionImageFilterTest.cxx
)

CreateTestDriver(ITKBiasCorrection  "${ITKBiasCorrection-Test_LIBRARIES}" "${ITKBiasCorrectionTests}")

itk_add_test(NAME itkCompositeValleyFunctionTest
      COMMAND ITKBiasCorrectionTestDriver itkCompositeValleyFunctionTest)
itk_add_test(NAME itkMRIBiasFieldCorrectionFilterTest
      COMMAND ITKBiasCorrectionTestDriver itkMRIBiasFieldCorrectionFilterTest)
itk_add_test(NAME itkN4BiasFieldCorrectionImageFilterTest1
      COMMAND ITKBiasCorrectionTestDriver
    --compare DATA{Baseline/N4ControlPoints_2D.nii.gz}
              ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_2D.nii.gz
    itkN4BiasFieldCorrectionImageFilterTest 2
    ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20.png           # input
    ${ITK_TEST_OUTPUT_DIR}//N4ControlPoints_2D.nii.gz           # control point lattice
    4                                                           # shrink factor
    20x20                                                       # number of iterations
    none                                                        # mask
    200                                                         # spline distance
    )
itk_add_test(NAME itkN4BiasFieldCorrectionImageFilterTest2
      COMMAND ITKBiasCorrectionTestDriver
      --compare DATA{Baseline/N4ControlPoints_3D.nii.gz}
               ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_3D_Test2.nii.gz
    itkN4BiasFieldCorrectionImageFilterTest
    3
    DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha}            # input
    ${ITK_TEST_OUTPUT_DIR}//N4ControlPoints_3D_Test2.nii.gz                  # control point lattice
    3                                                                  # shrink factor
    10x10x10                                                           # number of iterations
    none                                                               # mask
    150                                                                # spline distance
    )

itk_add_test(NAME itkN4BiasFieldCorrectionImageFilterTest3
      COMMAND ITKBiasCorrectionTestDriver
      --compare DATA{Baseline/N4ControlPoints_3D.nii.gz}
               ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_3D_Test3.nii.gz
    itkN4BiasFieldCorrectionImageFilterTest
    3
    DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha}            # input
    ${ITK_TEST_OUTPUT_DIR}//N4ControlPoints_3D_Test3.nii.gz                  # control point lattice
    3                                                                  # shrink factor
    10x10x10                                                           # number of iterations
    none                                                               # mask
    150                                                                # spline distance
    1                                                                  # mask label
    )

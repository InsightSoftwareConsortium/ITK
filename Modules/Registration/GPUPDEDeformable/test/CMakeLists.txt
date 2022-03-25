if (ITK_USE_GPU)

itk_module_test()

set(ITKGPUPDEDeformableRegistration-tests
 itkGPUDemonsRegistrationFilterTest.cxx
 itkGPUDemonsRegistrationFilterTest2.cxx
 )

set(ITKGPUPDEDeformableRegistrationTest_LIBRARIES ITKCommon;ITKGPUCommon;ITKGPUFiniteDifference;ITKGPUPDEDeformableRegistration;ITKStatistics;ITKSpatialObjects;ITKPath;ITKOptimizers;ITKIOBMP;ITKIOBioRad;ITKIOGDCM;ITKIOGIPL;ITKIOJPEG;ITKIOLSM;ITKIOMeta;ITKIONIFTI;ITKIONRRD;ITKIOPNG;ITKIOStimulate;ITKIOTIFF;ITKIOVTK;itksys)

CreateTestDriver(ITKGPUPDEDeformableRegistration
 "${ITKGPUPDEDeformableRegistrationTest_LIBRARIES}"
 "${ITKGPUPDEDeformableRegistration-tests}")

itk_add_test(NAME itkGPUDemonsRegistrationFilterTestDim2
        COMMAND ITKGPUPDEDeformableRegistrationTestDriver itkGPUDemonsRegistrationFilterTest
        2 # number of dimensions
        50 # number of iterations
        DATA{Input/LenaFix.png}
        DATA{Input/LenaMov.png}
        ${ITK_TEST_OUTPUT_DIR}/gpuDemonsRegistrationTest2D.mha)

itk_add_test(NAME itkGPUDemonsRegistrationFilterTestDim3
        COMMAND ITKGPUPDEDeformableRegistrationTestDriver itkGPUDemonsRegistrationFilterTest
        3 # number of dimensions
        50 # number of iterations
        DATA{Input/LenaFix.png}
        DATA{Input/LenaMov.png}
        ${ITK_TEST_OUTPUT_DIR}/gpuDemonsRegistrationTest2D.mha)

itk_add_test(NAME itkGPUDemonsRegistrationFilterTest2
             COMMAND ITKGPUPDEDeformableRegistrationTestDriver
             itkGPUDemonsRegistrationFilterTest2
             ${ITK_TEST_OUTPUT_DIR}/gpuDemonsRegistrationTest2Fixed.mha
             ${ITK_TEST_OUTPUT_DIR}/gpuDemonsRegistrationTest2Warped.mha
             )


endif()

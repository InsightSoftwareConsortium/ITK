if (ITK_USE_GPU)

itk_module_test()

set(ITKGPUAnisotropicSmoothing-tests
 itkGPUGradientAnisotropicDiffusionImageFilterTest.cxx
)

CreateTestDriver(ITKGPUAnisotropicSmoothing "${ITKGPUAnisotropicSmoothing-Test_LIBRARIES}" "${ITKGPUAnisotropicSmoothing-tests}")

itk_add_test(NAME itkGPUGradientAnisotropicDiffusionImageFilterTest2D
        COMMAND ITKGPUAnisotropicSmoothingTestDriver itkGPUGradientAnisotropicDiffusionImageFilterTest
        DATA{Input/BrainProtonDensitySlice.png}
        ${ITK_TEST_OUTPUT_DIR}/gpuGradientAnisotropicDiffusionImageFilterTest2D.mha 2)

itk_add_test(NAME itkGPUGradientAnisotropicDiffusionImageFilterTest3D
        COMMAND ITKGPUAnisotropicSmoothingTestDriver itkGPUGradientAnisotropicDiffusionImageFilterTest
        DATA{Input/HeadMRVolume.mha}
        ${ITK_TEST_OUTPUT_DIR}/gpuGradientAnisotropicDiffusionImageFilterTest3D.mha 3)

endif()

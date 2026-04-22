itk_module_test()

set(AnisotropicDiffusionLBRTests
  CoherenceEnhancingDiffusionTest.cxx
  )

CreateTestDriver(AnisotropicDiffusionLBR  "${AnisotropicDiffusionLBR-Test_LIBRARIES}" "${AnisotropicDiffusionLBRTests}")

set(TestOutput ${ITK_TEST_OUTPUT_DIR})

################ fig:PacMan #################

itk_add_test(NAME PacMan_cEED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/PacMan_cEED.png} ${TestOutput}/PacMan_cEED.png
  CoherenceEnhancingDiffusionTest DATA{Input/PacMan.png} ${TestOutput}/PacMan_cEED.png 20 0.05 cEED 3)

itk_add_test(NAME PacMan_cCED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/PacMan_cCED.png} ${TestOutput}/PacMan_cCED.png
  CoherenceEnhancingDiffusionTest DATA{Input/PacMan.png} ${TestOutput}/PacMan_cCED.png 20 0.05 cCED 3)

itk_add_test(NAME PacMan_I COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/PacMan_I.png} ${TestOutput}/PacMan_I.png
  CoherenceEnhancingDiffusionTest DATA{Input/PacMan.png} ${TestOutput}/PacMan_I.png 20 0.05 Isotropic 3)

############# fig : Fingerprint #############

itk_add_test(NAME FingerPrint_cEED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/FingerPrint_cEED_20.png} ${TestOutput}/FingerPrint_cEED_20.png
  CoherenceEnhancingDiffusionTest DATA{Input/FingerPrint.png} ${TestOutput}/FingerPrint_cEED_20.png 20 0.02 cEED )

itk_add_test(NAME FingerPrint_cCED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/FingerPrint_cCED_20.png} ${TestOutput}/FingerPrint_cCED_20.png
  CoherenceEnhancingDiffusionTest DATA{Input/FingerPrint.png} ${TestOutput}/FingerPrint_cCED_20.png 20 0.02 cCED )

itk_add_test(NAME FingerPrint_I COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/FingerPrint_I_20.png} ${TestOutput}/FingerPrint_I_20.png
  CoherenceEnhancingDiffusionTest DATA{Input/FingerPrint.png} ${TestOutput}/FingerPrint_I_20.png 20 0.02 Isotropic )


################ fig : Cos3D ################

itk_add_test(NAME Cos3D_cCED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/Cos3D_cCED.vtk}  ${TestOutput}/Cos3D_cCED.vtk
  CoherenceEnhancingDiffusionTest DATA{Input/Cos3D_Noisy.vtk} ${TestOutput}/Cos3D_cCED.vtk 10 0.02 cCED 4 10 )


################ fig : Brain ################

itk_add_test(NAME mrbrain_cEED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/mrbrain_cEED.vtk}  ${TestOutput}/mrbrain_cEED.vtk
  CoherenceEnhancingDiffusionTest DATA{Input/mrbrain_noisy.vtk} ${TestOutput}/mrbrain_cEED.vtk 5 0.003 cEED )

################# fig : Lena ################

itk_add_test(NAME Lena_Detail_cCED_2 COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/Lena_Detail_cCED_2.png}  ${TestOutput}/Lena_Detail_cCED_2.png
  CoherenceEnhancingDiffusionTest DATA{Input/Lena_Detail.png} ${TestOutput}/Lena_Detail_cCED_2.png 2 0.003 cCED 0.5 2 4 )

itk_add_test(NAME Lena_Detail_cEED_2 COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/Lena_Detail_cEED_2.png}  ${TestOutput}/Lena_Detail_cEED_2.png
  CoherenceEnhancingDiffusionTest DATA{Input/Lena_Detail.png} ${TestOutput}/Lena_Detail_cEED_2.png 2 0.003 cEED 0.5 2 4 )

itk_add_test(NAME Lena_Detail_I_2 COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/Lena_Detail_I_2.png}  ${TestOutput}/Lena_Detail_I_2.png
  CoherenceEnhancingDiffusionTest DATA{Input/Lena_Detail.png} ${TestOutput}/Lena_Detail_I_2.png 2 0.003 Isotropic 0.5 2 4 )


############## fig : Vector field ###########

itk_add_test(NAME VectorField_Circle_cEED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/VectorField_Circle_cEED.vtk}  ${TestOutput}/VectorField_Circle_cEED.vtk
  CoherenceEnhancingDiffusionTest DATA{Input/VectorField_CircleOpposites.vtk} ${TestOutput}/VectorField_Circle_cEED.vtk 10 0.05 cEED )


############### fig : Triangle ##############


itk_add_test(NAME Triangle_cEED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/Triangle_cEED.png}  ${TestOutput}/Triangle_cEED.png
  CoherenceEnhancingDiffusionTest DATA{Input/Triangle.png} ${TestOutput}/Triangle_cEED.png 5 0.03 cEED )

itk_add_test(NAME Triangle_EED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/Triangle_EED.png}  ${TestOutput}/Triangle_EED.png
  CoherenceEnhancingDiffusionTest DATA{Input/Triangle.png} ${TestOutput}/Triangle_EED.png 5 0.03 EED )

itk_add_test(NAME Oscillations1_cCED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/Oscillations1_cCED.png}  ${TestOutput}/Oscillations1_cCED.png
  CoherenceEnhancingDiffusionTest DATA{Input/Oscillations_Noisy1.png} ${TestOutput}/Oscillations1_cCED.png 20 0.05 cCED )

itk_add_test(NAME Oscillations1_CED COMMAND AnisotropicDiffusionLBRTestDriver
  --compare DATA{Baseline/Oscillations1_CED.png}  ${TestOutput}/Oscillations1_CED.png
  CoherenceEnhancingDiffusionTest DATA{Input/Oscillations_Noisy1.png} ${TestOutput}/Oscillations1_CED.png 20 0.05 CED )

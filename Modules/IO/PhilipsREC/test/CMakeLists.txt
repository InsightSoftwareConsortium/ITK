itk_module_test()
set(ITKIOPhilipsRECTests
itkPhilipsRECImageIOTest.cxx
itkPhilipsRECImageIOOrientationTest.cxx
itkPhilipsRECImageIOPrintTest.cxx
)

CreateTestDriver(ITKIOPhilipsREC  "${ITKIOPhilipsREC-Test_LIBRARIES}" "${ITKIOPhilipsRECTests}")

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Philips)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkPhilipsRECImageIOTest1
      COMMAND ITKIOPhilipsRECTestDriver itkPhilipsRECImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/T1_MEASUREMENT_4_1.PAR,T1_MEASUREMENT_4_1.REC} ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOTest1.mha)
itk_add_test(NAME itkPhilipsRECImageIOOrientationTest1
      COMMAND ITKIOPhilipsRECTestDriver
    --compare DATA{Baseline/itkPhilipsRECImageIOOrientationTest1.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOOrientationTest1.mha
    itkPhilipsRECImageIOOrientationTest DATA{Input/Test_Orientation_2_3.PAR} DATA{Input/Test_Orientation_2_1.PAR} ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOOrientationTest1.mha)
itk_add_test(NAME itkPhilipsRECImageIOOrientationTest2
      COMMAND ITKIOPhilipsRECTestDriver
    --compare DATA{Baseline/itkPhilipsRECImageIOOrientationTest2.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOOrientationTest2.mha
    itkPhilipsRECImageIOOrientationTest DATA{Input/Test_Orientation_2_5.PAR} DATA{Input/Test_Orientation_2_1.REC.gz} ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOOrientationTest2.mha)
itk_add_test(NAME itkPhilipsRECImageIOOrientationTest3
      COMMAND ITKIOPhilipsRECTestDriver
    --compare DATA{Baseline/itkPhilipsRECImageIOOrientationTest3.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOOrientationTest3.mha
    itkPhilipsRECImageIOOrientationTest DATA{Input/Test_Orientation_2_5.REC.gz} DATA{Input/Test_Orientation_2_3.REC.gz} ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOOrientationTest3.mha)
itk_add_test(NAME itkPhilipsRECImageIOTest2
      COMMAND ITKIOPhilipsRECTestDriver itkPhilipsRECImageIOTest
              DATA{Input/Test_Cardiac_Phase_7_1.REC.gz} ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOTest2.mha)
itk_add_test(NAME itkPhilipsRECImageIOPrintTest1
      COMMAND ITKIOPhilipsRECTestDriver itkPhilipsRECImageIOPrintTest
              DATA{Input/Test_Cardiac_Phase_7_1.PAR})

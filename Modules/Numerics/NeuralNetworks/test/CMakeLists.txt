itk_module_test()
set(ITKNeuralNetworksTests
NNetClassifierTest1.cxx
NNetClassifierTest2.cxx
NNetClassifierTest3.cxx
NNetClassifierTest4.cxx
RBFTest1.cxx
QPropXORTest1.cxx
XORTest1.cxx
XORTest2.cxx
itkLogSigmoidTransferFunctionTest.cxx
itkSigmoidTransferFunctionTest.cxx
)

CreateTestDriver(ITKNeuralNetworks  "${ITKNeuralNetworks-Test_LIBRARIES}" "${ITKNeuralNetworksTests}")

itk_add_test(NAME NNetClassifierTest1
      COMMAND ITKNeuralNetworksTestDriver --redirectOutput ${ITK_TEST_OUTPUT_DIR}/NNetClassifierTest1.txt
    NNetClassifierTest1
        DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt} DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt})
set_tests_properties(NNetClassifierTest1 PROPERTIES ATTACHED_FILES_ON_FAIL ${ITK_TEST_OUTPUT_DIR}/NNetClassifierTest1.txt)


itk_add_test(NAME NNetClassifierTest2
      COMMAND ITKNeuralNetworksTestDriver NNetClassifierTest2
              DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt} DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt})

itk_add_test(NAME NNetClassifierTest3
      COMMAND ITKNeuralNetworksTestDriver --redirectOutput ${ITK_TEST_OUTPUT_DIR}/NNetClassifierTest3.txt
    NNetClassifierTest3
        DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt} DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt})
set_tests_properties(NNetClassifierTest3 PROPERTIES ATTACHED_FILES_ON_FAIL ${ITK_TEST_OUTPUT_DIR}/NNetClassifierTest3)

itk_add_test(NAME NNetClassifierTest4
      COMMAND ITKNeuralNetworksTestDriver NNetClassifierTest4
              DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt} DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt})
itk_add_test(NAME RBFTest1
      COMMAND ITKNeuralNetworksTestDriver --redirectOutput RBFTest1.txt
          RBFTest1
              DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_train.txt} DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_test.txt})
set_tests_properties(RBFTest1 PROPERTIES ATTACHED_FILES_ON_FAIL ${ITK_TEST_OUTPUT_DIR}/RBFTest1.txt)

itk_add_test(NAME QPropXORTest1
      COMMAND ITKNeuralNetworksTestDriver QPropXORTest1
              DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/qpropxortest.txt})
itk_add_test(NAME XORTest1
      COMMAND ITKNeuralNetworksTestDriver XORTest1
              DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt})
itk_add_test(NAME XORTest2
      COMMAND ITKNeuralNetworksTestDriver XORTest2
              DATA{${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt})

itk_add_test(NAME itkLogSigmoidTransferFunctionTest
      COMMAND ITKNeuralNetworksTestDriver itkLogSigmoidTransferFunctionTest)
itk_add_test(NAME itkSigmoidTransferFunctionTest
      COMMAND ITKNeuralNetworksTestDriver itkSigmoidTransferFunctionTest)

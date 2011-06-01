itk_module_test()
set(ITK-NeuralNetworksTests
itkNeuralNetworksHeaderTest.cxx
NNetClassifierTest1.cxx
NNetClassifierTest2.cxx
NNetClassifierTest3.cxx
NNetClassifierTest4.cxx
)

CreateTestDriver(ITK-NeuralNetworks  "${ITK-NeuralNetworks-Test_LIBRARIES}" "${ITK-NeuralNetworksTests}")

itk_add_test(NAME itkNeuralNetworksHeaderTest
      COMMAND ITK-NeuralNetworksTestDriver itkNeuralNetworksHeaderTest)
itk_add_test(NAME NNetClassifierTest1
      COMMAND ITK-NeuralNetworksTestDriver NNetClassifierTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
itk_add_test(NAME NNetClassifierTest2
      COMMAND ITK-NeuralNetworksTestDriver NNetClassifierTest2
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
itk_add_test(NAME NNetClassifierTest3
      COMMAND ITK-NeuralNetworksTestDriver NNetClassifierTest3
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
itk_add_test(NAME NNetClassifierTest4
      COMMAND ITK-NeuralNetworksTestDriver NNetClassifierTest4
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)

itk_module_test()
set(ITKNeuralNetworksTests
itkNeuralNetworksHeaderTest.cxx
NNetClassifierTest1.cxx
NNetClassifierTest2.cxx
NNetClassifierTest3.cxx
NNetClassifierTest4.cxx
RBFTest1.cxx
QPropXORTest1.cxx
XORTest1.cxx
XORTest2.cxx
)

CreateTestDriver(ITKNeuralNetworks  "${ITKNeuralNetworks-Test_LIBRARIES}" "${ITKNeuralNetworksTests}")

itk_add_test(NAME itkNeuralNetworksHeaderTest
      COMMAND ITKNeuralNetworksTestDriver itkNeuralNetworksHeaderTest)
itk_add_test(NAME NNetClassifierTest1
      COMMAND ITKNeuralNetworksTestDriver NNetClassifierTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
itk_add_test(NAME NNetClassifierTest2
      COMMAND ITKNeuralNetworksTestDriver NNetClassifierTest2
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
itk_add_test(NAME NNetClassifierTest3
      COMMAND ITKNeuralNetworksTestDriver NNetClassifierTest3
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
itk_add_test(NAME NNetClassifierTest4
      COMMAND ITKNeuralNetworksTestDriver NNetClassifierTest4
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
itk_add_test(NAME RBFTest1
      COMMAND ITKNeuralNetworksTestDriver RBFTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_test.txt)
itk_add_test(NAME QPropXORTest1
      COMMAND ITKNeuralNetworksTestDriver QPropXORTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/qpropxortest.txt)
itk_add_test(NAME XORTest1
      COMMAND ITKNeuralNetworksTestDriver XORTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
itk_add_test(NAME XORTest2
      COMMAND ITKNeuralNetworksTestDriver XORTest2
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)

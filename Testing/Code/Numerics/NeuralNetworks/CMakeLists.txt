include_regular_expression("^.*$")

if( NOT ITK_DISABLE_CXX_TESTING )
 if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-NeuralNets ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Numerics/NeuralNetworks)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)

set(KIT NeuralNetworks)
set(KIT_LIBS
  ITKIO ITKNumerics ITKStatistics
)

set(KitTests
  NNetClassifierTest1
  NNetClassifierTest2
  NNetClassifierTest3
  NNetClassifierTest4
  QPropXORTest1
  RBFTest1
  XORTest1
  XORTest2
  itkNeuralNetworksPrintTest
)

# Given KIT, KIT_LIBS and KitTests, build TestDriver
CreateTestDriver(${KIT} "${KIT_LIBS}" "${KitTests}" TestDriver)

set(NEURAL_NETWORKS_HEADER_TEST  ${CXX_TEST_PATH}/itkNeuralNetworksHeaderTest)
add_executable(itkNeuralNetworksHeaderTest itkNeuralNetworksHeaderTest.cxx)
target_link_libraries(itkNeuralNetworksHeaderTest ${KIT_LIBS})
add_test(itkNeuralNetworksHeaderTest ${NEURAL_NETWORKS_HEADER_TEST})

add_test(QPropXORTest1 ${TestDriver}
         QPropXORTest1 ${ITK_DATA_ROOT}/Input/NeuralNetworks/qpropxortest.txt)
add_test(XORTest1 ${TestDriver}
         XORTest1 ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
add_test(XORTest2 ${TestDriver}
         XORTest2 ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
add_test(NNetClassifierTest1 ${TestDriver}
         NNetClassifierTest1 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt
                             ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
add_test(NNetClassifierTest2 ${TestDriver}
         NNetClassifierTest2 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt
                             ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
add_test(NNetClassifierTest3 ${TestDriver}
         NNetClassifierTest3 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt
                             ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
add_test(NNetClassifierTest4 ${TestDriver}
         NNetClassifierTest4 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt
                             ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
add_test(RBFTest1 ${TestDriver}
         RBFTest1 ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_train.txt
                  ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_test.txt)

add_test( itkNeuralNetworksPrintTest ${TestDriver}
          itkNeuralNetworksPrintTest)

if(ITK_OBJCXX_COMPILER_WORKS)
  set(NEURALNETWORKS_OBJCXX_TEST ${CXX_TEST_PATH}/itkNeuralNetworksHeaderObjCxxTest)
  add_test(itkNeuralNetworksHeaderObjCxxTest ${NEURALNETWORKS_OBJCXX_TEST})
  add_executable(itkNeuralNetworksHeaderObjCxxTest itkNeuralNetworksHeaderObjCxxTest.mm)
  target_link_libraries(itkNeuralNetworksHeaderObjCxxTest ITKIO ITKNumerics ITKStatistics)
endif(ITK_OBJCXX_COMPILER_WORKS)

endif( NOT ITK_DISABLE_CXX_TESTING )

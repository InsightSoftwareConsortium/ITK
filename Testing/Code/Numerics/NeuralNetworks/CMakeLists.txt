IF( NOT ITK_DISABLE_CXX_TESTING )
 IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-NeuralNets ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Numerics/NeuralNetworks)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

  SET(NEURAL_NETWORKS_HEADER_TEST  ${CXX_TEST_PATH}/itkNeuralNetworksHeaderTest)
    
  INCLUDE_DIRECTORIES(
   ${ITK_SOURCE_DIR}/Code/Numerics/NeuralNetworks
   ${ITK_INCLUDE_DIRS}
    )
    
  LINK_DIRECTORIES(
      ${ITK_BINARY_DIR}/Code/Numerics/NeuralNetworks 
    )
    
   LINK_LIBRARIES(
      ${ITK_LIBRARIES}
    )
    
 
  SET(NNET_OBJECT_TESTS  ${CXX_TEST_PATH}/itkNeuralNetworkTests)

  ADD_TEST( itkNeuralNetworksHeaderTest ${NEURAL_NETWORKS_HEADER_TEST})

  ADD_TEST(XORTest1 ${NNET_OBJECT_TESTS} 
            XORTest1  ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
  ADD_TEST(XORTest2 ${NNET_OBJECT_TESTS} 
            XORTest2   ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
  ADD_TEST(QPropXORTest1 ${NNET_OBJECT_TESTS} 
            QPropXORTest1   ${ITK_DATA_ROOT}/Input/NeuralNetworks/qpropxortest.txt)
  ADD_TEST(NNetClassifierTest1 ${NNET_OBJECT_TESTS} 
           NNetClassifierTest1 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt 
                               ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
  ADD_TEST(NNetClassifierTest2 ${NNET_OBJECT_TESTS} 
           NNetClassifierTest2 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt 
                                 ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
  ADD_TEST(NNetClassifierTest3 ${NNET_OBJECT_TESTS} 
           NNetClassifierTest3 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt 
                               ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
  ADD_TEST(NNetClassifierTest4 ${NNET_OBJECT_TESTS} 
           NNetClassifierTest4 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt 
                               ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
  ADD_TEST(RBFTest1 ${NNET_OBJECT_TESTS} 
           RBFTest1 ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_train.txt
                    ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_test.txt)
  
  ADD_TEST( itkNeuralNetworksPrintTest ${NNET_OBJECT_TESTS} itkNeuralNetworksPrintTest)



   SET(NNET_SRCS
      XORTest1.cxx
      XORTest2.cxx
      QPropXORTest1.cxx
      NNetClassifierTest1.cxx
      NNetClassifierTest2.cxx
      NNetClassifierTest3.cxx
      NNetClassifierTest4.cxx
      RBFTest1.cxx
      itkNeuralNetworksPrintTest.cxx
    )

  ADD_EXECUTABLE(itkNeuralNetworkTests itkNeuralNetworkTests.cxx ${NNET_SRCS})

  ADD_EXECUTABLE(itkNeuralNetworksHeaderTest itkNeuralNetworksHeaderTest.cxx)


ENDIF( NOT ITK_DISABLE_CXX_TESTING )

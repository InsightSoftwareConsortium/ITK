include_regular_expression("^.*$")

if( NOT ITK_DISABLE_CXX_TESTING )
 if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-NeuralNets ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Numerics/NeuralNetworks)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)

  set(NEURAL_NETWORKS_HEADER_TEST  ${CXX_TEST_PATH}/itkNeuralNetworksHeaderTest)


  link_directories(
      ${ITK_BINARY_DIR}/Code/Numerics/NeuralNetworks
    )

   link_libraries(
      ${ITK_LIBRARIES}
    )


  set(NNET_OBJECT_TESTS  ${CXX_TEST_PATH}/itkNeuralNetworkTests)
  set(NNET_OBJECT_TESTS2 ${CXX_TEST_PATH}/itkNeuralNetworkTests2)
  set(NNET_OBJECT_TESTS3 ${CXX_TEST_PATH}/itkNeuralNetworkTests3)

  add_test( itkNeuralNetworksHeaderTest ${NEURAL_NETWORKS_HEADER_TEST})

  add_test(QPropXORTest1 ${NNET_OBJECT_TESTS}
            QPropXORTest1   ${ITK_DATA_ROOT}/Input/NeuralNetworks/qpropxortest.txt)
  add_test(XORTest1 ${NNET_OBJECT_TESTS}
            XORTest1  ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
  add_test(XORTest2 ${NNET_OBJECT_TESTS}
            XORTest2   ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
  add_test(NNetClassifierTest1 ${NNET_OBJECT_TESTS3}
           NNetClassifierTest1 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt
                               ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
  add_test(NNetClassifierTest2 ${NNET_OBJECT_TESTS3}
           NNetClassifierTest2 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt
                                 ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
  add_test(NNetClassifierTest3 ${NNET_OBJECT_TESTS3}
           NNetClassifierTest3 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt
                               ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
  add_test(NNetClassifierTest4 ${NNET_OBJECT_TESTS3}
           NNetClassifierTest4 ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt
                               ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
  add_test(RBFTest1 ${NNET_OBJECT_TESTS2}
           RBFTest1 ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_train.txt
                    ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_test.txt)

  add_test( itkNeuralNetworksPrintTest ${NNET_OBJECT_TESTS2} itkNeuralNetworksPrintTest)

   set(NNET_SRCS
      XORTest1.cxx
      XORTest2.cxx
      QPropXORTest1.cxx
    )

  set(NNET_SRCS2
      RBFTest1.cxx
      itkNeuralNetworksPrintTest.cxx
    )

   set(NNET_SRCS3
      NNetClassifierTest1.cxx
      NNetClassifierTest2.cxx
      NNetClassifierTest3.cxx
      NNetClassifierTest4.cxx
    )



  add_executable(itkNeuralNetworkTests itkNeuralNetworkTests.cxx ${NNET_SRCS})
  target_link_libraries(itkNeuralNetworkTests ITKIO ITKNumerics ITKStatistics)

  add_executable(itkNeuralNetworkTests2 itkNeuralNetworkTests2.cxx ${NNET_SRCS2})
  target_link_libraries(itkNeuralNetworkTests2 ITKIO ITKNumerics ITKStatistics)

  add_executable(itkNeuralNetworkTests3 itkNeuralNetworkTests3.cxx ${NNET_SRCS3})
  target_link_libraries(itkNeuralNetworkTests3 ITKIO ITKNumerics ITKStatistics)

  add_executable(itkNeuralNetworksHeaderTest itkNeuralNetworksHeaderTest.cxx)
  target_link_libraries(itkNeuralNetworksHeaderTest ITKIO ITKNumerics ITKStatistics)

  if(ITK_OBJCXX_COMPILER_WORKS)
    set(NEURALNETWORKS_OBJCXX_TEST ${CXX_TEST_PATH}/itkNeuralNetworksHeaderObjCxxTest)
    add_test(itkNeuralNetworksHeaderObjCxxTest ${NEURALNETWORKS_OBJCXX_TEST})
    add_executable(itkNeuralNetworksHeaderObjCxxTest itkNeuralNetworksHeaderObjCxxTest.mm)
    target_link_libraries(itkNeuralNetworksHeaderObjCxxTest ITKIO ITKNumerics ITKStatistics)
  endif(ITK_OBJCXX_COMPILER_WORKS)

endif( NOT ITK_DISABLE_CXX_TESTING )

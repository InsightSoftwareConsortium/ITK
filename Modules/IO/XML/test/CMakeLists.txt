itk_module_test()

set(ITKIOXMLTests
  itkDOMTest1.cxx
  itkDOMTest2.cxx
  itkDOMTest3.cxx
  itkDOMTest4.cxx
  itkDOMTest5.cxx
  itkDOMTest6.cxx
  itkDOMTest7.cxx
  itkDOMTest8.cxx
)

CreateTestDriver(ITKIOXML  "${ITKIOXML-Test_LIBRARIES}" "${ITKIOXMLTests}")

itk_add_test(NAME itkDOMTest1
  COMMAND ITKIOXMLTestDriver itkDOMTest1
)

itk_add_test(NAME itkDOMTest21
  COMMAND ITKIOXMLTestDriver
  itkDOMTest2 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} ${ITK_TEST_OUTPUT_DIR}/xmldom-test21-output.xml
)

itk_add_test(NAME itkDOMTest22
  COMMAND ITKIOXMLTestDriver
  itkDOMTest2 DATA{${ITK_DATA_ROOT}/Input/XML/test.pso.xml} ${ITK_TEST_OUTPUT_DIR}/xmldom-test22-output.xml
)

itk_add_test(NAME itkDOMTest3
  COMMAND ITKIOXMLTestDriver
  itkDOMTest3 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml}
)

itk_add_test(NAME itkDOMTest4_01
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/1" "/1"
)

itk_add_test(NAME itkDOMTest4_02
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user" "/1"
)

itk_add_test(NAME itkDOMTest4_03
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user:0" "/1"
)

itk_add_test(NAME itkDOMTest4_04
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/1/who:2" "/1/10"
)

itk_add_test(NAME itkDOMTest4_05
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/!" "/1/1"
)

itk_add_test(NAME itkDOMTest4_06
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/!:0" "/1/1"
)

itk_add_test(NAME itkDOMTest4_07
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/!:5" "/1/8"
)

itk_add_test(NAME itkDOMTest4_08
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/-" "/0"
)

itk_add_test(NAME itkDOMTest4_09
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/-0" "/1"
)

itk_add_test(NAME itkDOMTest4_10
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/-1" "/0"
)

itk_add_test(NAME itkDOMTest4_11
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/body/-2" "/0"
)

itk_add_test(NAME itkDOMTest4_12
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/to/+" "/1"
)

itk_add_test(NAME itkDOMTest4_13
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/to/+1" "/1"
)

itk_add_test(NAME itkDOMTest4_14
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/to/+0" "/0"
)

itk_add_test(NAME itkDOMTest4_15
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/to/+1" "/1"
)

itk_add_test(NAME itkDOMTest4_16
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/to/+2" "/2"
)

itk_add_test(NAME itkDOMTest4_17
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/:hi" "/1/9"
)

itk_add_test(NAME itkDOMTest4_18
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/." "/1"
)

itk_add_test(NAME itkDOMTest4_19
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/who:2/.." "/1"
)

itk_add_test(NAME itkDOMTest4_20
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user//" "[]"
)

itk_add_test(NAME itkDOMTest4_21
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/" "[]"
)

itk_add_test(NAME itkDOMTest4_22
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/" "/1"
)

itk_add_test(NAME itkDOMTest4_23
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "/user/!/+5/-2/.././who:2/../:hi" "/1/9"
)

itk_add_test(NAME itkDOMTest4_24
  COMMAND ITKIOXMLTestDriver
  itkDOMTest4 DATA{${ITK_DATA_ROOT}/Input/XML/test.xml} "[]" "[]"
)

itk_add_test(NAME itkDOMTest5
  COMMAND ITKIOXMLTestDriver
  itkDOMTest5 ${ITK_TEST_OUTPUT_DIR}/xmldom-test5/output.xml
)

itk_add_test(NAME itkDOMTest6
  COMMAND ITKIOXMLTestDriver itkDOMTest6
)

itk_add_test(NAME itkDOMTest7
  COMMAND ITKIOXMLTestDriver itkDOMTest7
)

itk_add_test(NAME itkDOMTest8
  COMMAND ITKIOXMLTestDriver
  itkDOMTest8 ${ITK_TEST_OUTPUT_DIR}/xmldom-test8/ output/test_file
)

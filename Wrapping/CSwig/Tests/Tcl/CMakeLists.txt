IF(ITK_CSWIG_TCL)
    SET(ITK_WISH ${CXX_TEST_PATH}/itkwish)
    ADD_TEST(PrintAllTcl ${ITK_WISH} ${ITK_SOURCE_DIR}/Wrapping/CSwig/Tests/Tcl/PrintAll.tcl)
ENDIF(ITK_CSWIG_TCL)

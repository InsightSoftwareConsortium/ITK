
if( NOT ITK_DISABLE_TCL_TESTING )
if(ITK_CSWIG_TCL)
    set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)
    set(ITK_WISH ${CXX_TEST_PATH}/itkwish)
    add_test(PrintAllTcl ${ITK_WISH} ${ITK_SOURCE_DIR}/Wrapping/CSwig/Tests/Tcl/PrintAll.tcl ${TEMP}/PrintAll.txt)
endif(ITK_CSWIG_TCL)
endif( NOT ITK_DISABLE_TCL_TESTING )

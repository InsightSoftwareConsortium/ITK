set(ITK_TCL_EXE_DIR_BUILD "${ITK_EXECUTABLE_PATH}")
set(ITK_TCL_EXE_NAME_ROOT "itkwish")

configure_file(${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl/itkTclConfigure.h.in
               ${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/itkTclConfigure.h)

include_directories(${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl)
add_executable(itkwish itkTclAppInit.cxx)
target_link_libraries(itkwish
  ITKAlgorithmsTcl
  ITKBasicFiltersATcl
  ITKBasicFiltersBTcl
  ITKIOTcl
  ITKNumericsTcl
  ITKCommonATcl
  ITKCommonBTcl
  VXLNumericsTcl
  SwigRuntimeTcl
  ${TCL_LIBRARY}
  ${TK_LIBRARY}
)

if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(itkwish PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)
install(TARGETS itkwish
  RUNTIME DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeExecutables)

install(FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/itkinteraction.tcl
  ${CMAKE_CURRENT_SOURCE_DIR}/itktesting.tcl
  ${CMAKE_CURRENT_SOURCE_DIR}/itkdata.tcl
  DESTINATION ${ITK_INSTALL_LIB_DIR_CM24}/tcl
  COMPONENT RuntimeLibraries)

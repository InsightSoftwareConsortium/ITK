ADD_LIBRARY( itkNetlibSlatec
  # from SLATEC/FNLIB
  d1mach.c
  d9lgmc.c
  d9gmit.c
  d9lgic.c
  d9lgit.c
  dbetai.c
  dcsevl.c
  dgami.c
  dgamit.c
  dgamlm.c
  dgamma.c
  dgamr.c
  dlbeta.c
  dlgams.c
  dlngam.c
  dlnrel.c
  initds.c
  xermsg.c
  d_int.c
  d_mod.c  
 )

IF(ITK_USE_SYSTEM_VXL)
  TARGET_LINK_LIBRARIES( itkNetlibSlatec v3p_netlib netlib)
ELSE(ITK_USE_SYSTEM_VXL)
  TARGET_LINK_LIBRARIES( itkNetlibSlatec itkv3p_netlib )
ENDIF(ITK_USE_SYSTEM_VXL)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(itkNetlibSlatec PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS itkNetlibSlatec
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

IF (BORLAND)
  SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w-8004 -w-8057")
ENDIF (BORLAND)

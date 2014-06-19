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

INSTALL(TARGETS itkNetlibSlatec
  EXPORT ${ITK3P_INSTALL_EXPORT_NAME}
  RUNTIME DESTINATION ${ITK3P_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
  LIBRARY DESTINATION ${ITK3P_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
  ARCHIVE DESTINATION ${ITK3P_INSTALL_ARCHIVE_DIR} COMPONENT Development)

IF (BORLAND)
  SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w-8004 -w-8057")
ENDIF (BORLAND)

IF(GLUT_FOUND)

  IF (WIN32)
     # - Avoid the glutCreateMenu_ATEXIT_HACK pb.
     # - Sort-of a hack to avoid the bug in glut.h to avoid
     #   ambiguity between 'std::exit(int)' and 'std::exit(int)' in function
     ADD_DEFINITIONS (-DGLUT_DISABLE_ATEXIT_HACK)
     ADD_DEFINITIONS (-DGLUT_BUILDING_LIB)
  ENDIF (WIN32)


  INCLUDE_DIRECTORIES(
    ${GLUT_INCLUDE_DIR}
    )

  ADD_LIBRARY(ITKGlut
    itkGlutMaster.cxx
    itkGlutWindow.cxx
    )

  INSTALL_TARGETS(/lib/InsightToolkit ITKGlut)
  INSTALL_FILES(/include/InsightToolkit/Utilities/GlutMaster "(\\.h)$")

ENDIF(GLUT_FOUND)

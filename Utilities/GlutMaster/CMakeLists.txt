IF(GLUT_FOUND)
  ADD_LIBRARY(ITKGlut
    itkGlutMaster.cxx
    itkGlutWindow.cxx
    )

  INSTALL_TARGETS(/lib/InsightToolkit ITKGlut)
  INSTALL_FILES(/include/InsightToolkit/Utilities/GlutMaster "(\\.h)$")
ENDIF(GLUT_FOUND)

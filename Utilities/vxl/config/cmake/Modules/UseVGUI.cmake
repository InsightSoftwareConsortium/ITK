SET( HAS_VGUI "NO" )
IF(BUILD_VGUI)

  INCLUDE(${CMAKE_ROOT}/Modules/FindOpenGL.cmake)
  IF (OPENGL_FOUND)
  IF (OPENGL_GLU_FOUND)

    INCLUDE_DIRECTORIES( ${OPENGL_INCLUDE_PATH} )
    SET( HAS_VGUI "YES" )
    ADD_DEFINITIONS( -DHAS_OPENGL)

    # There is a bug in the CMake 1.4.x dependency analysis that
    # affects the vgui dependencies: the libraries get re-ordered
    # incorrectly. This is a work-around until that gets fixed.
    INCLUDE(${CMAKE_ROOT}/Modules/FindGTK.cmake)
    IF(VGUI_USE_GTK)
      LINK_LIBRARIES( ${GTK_LIBRARIES} ${OPENGL_LIBRARIES} )
    ENDIF(VGUI_USE_GTK)

  ENDIF (OPENGL_GLU_FOUND)
  ENDIF (OPENGL_FOUND)

ENDIF(BUILD_VGUI)

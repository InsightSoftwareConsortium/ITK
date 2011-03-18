# find SimVoleon library for Coin3D based Volume Rendering 
# Once done this will define
#
# SIMVOLEON_FOUND        - system has SIMVOLEON - volume rendering library
# SIMVOLEON_INCLUDE_DIR  - where the SimVoleon include directory can be found
# SIMVOLEON_LIBRARY      - Linking library
#
IF(COIN3D_FOUND)
  IF (WIN32)
    IF (CYGWIN)

      FIND_PATH(SIMVOLEON_INCLUDE_DIR VolumeViz/nodes/SoVolumeRender.h
        /usr/include/
        /usr/local/include/
      )

      FIND_LIBRARY(SIMVOLEON_LIBRARY SIMVoleon
        /usr/lib
        /usr/local/lib
      )

    ELSE (CYGWIN)

       FIND_PATH(SIMVOLEON_INCLUDE_DIR VolumeViz/nodes/SoVolumeRender.h
        "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Coin3D;InstallPath]/include"

       )

       FIND_LIBRARY(SIMVOLEON_LIBRARY simvoleon2
        "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Coin3D;InstallPath]/lib"
       )

       IF (SIMVOLEON_LIBRARY)
         ADD_DEFINITIONS ( -DSIMVOLEON_NOT_DLL )
       ELSE (SIMVOLEON_LIBRARY)
         SET (SIMVOLEON_LIBRARY simvoleon2 CACHE STRING "SIMVoleon Library - Coin3D based Volume Rendering API")
       ENDIF (SIMVOLEON_LIBRARY)

    ENDIF (CYGWIN)

  ELSE (WIN32)
    IF(APPLE)
      FIND_PATH(SIMVOLEON_INCLUDE_DIR VolumeViz/nodes/SoVolumeRender.h
        /usr/include
        /usr/local/include
      )

      FIND_LIBRARY(SIMVOLEON_LIBRARY SimVoleon
        /usr/lib
        /usr/local/lib
      )   

    ELSE(APPLE)

      FIND_PATH(SIMVOLEON_INCLUDE_DIR VolumeViz/nodes/SoVolumeRender.h
        /usr/include
        /usr/local/include
      )

      FIND_LIBRARY(SIMVOLEON_LIBRARY SimVoleon
        /usr/lib
        /usr/local/lib
      )   

    ENDIF(APPLE)

  ENDIF (WIN32)

ENDIF(COIN3D_FOUND)

  SET( SIMVOLEON_FOUND "NO" )
  IF(SIMVOLEON_LIBRARY)
    SET( SIMVOLEON_FOUND "YES" )
  ENDIF(SIMVOLEON_LIBRARY)


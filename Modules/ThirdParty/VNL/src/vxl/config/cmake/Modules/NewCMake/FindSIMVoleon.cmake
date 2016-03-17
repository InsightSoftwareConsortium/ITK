# find SimVoleon library for Coin3D based Volume Rendering
# Once done this will define
#
# SIMVOLEON_FOUND        - system has SIMVOLEON - volume rendering library
# SIMVOLEON_INCLUDE_DIR  - where the SimVoleon include directory can be found
# SIMVOLEON_LIBRARY      - Linking library
#
if(COIN3D_FOUND)
  if(WIN32)
    if(CYGWIN)

      find_path(SIMVOLEON_INCLUDE_DIR VolumeViz/nodes/SoVolumeRender.h
        /usr/include/
        /usr/local/include/
      )

      find_library(SIMVOLEON_LIBRARY SIMVoleon
        /usr/lib
        /usr/local/lib
      )

    else()

       find_path(SIMVOLEON_INCLUDE_DIR VolumeViz/nodes/SoVolumeRender.h
        "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Coin3D;InstallPath]/include"

       )

       find_library(SIMVOLEON_LIBRARY simvoleon2
        "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Coin3D;InstallPath]/lib"
       )

       if(SIMVOLEON_LIBRARY)
         add_definitions( -DSIMVOLEON_NOT_DLL )
       else()
         set(SIMVOLEON_LIBRARY simvoleon2 CACHE STRING "SIMVoleon Library - Coin3D based Volume Rendering API")
       endif()

    endif()

  else()
    if(APPLE)
      find_path(SIMVOLEON_INCLUDE_DIR VolumeViz/nodes/SoVolumeRender.h
        /usr/include
        /usr/local/include
      )

      find_library(SIMVOLEON_LIBRARY SimVoleon
        /usr/lib
        /usr/local/lib
      )

    else()

      find_path(SIMVOLEON_INCLUDE_DIR VolumeViz/nodes/SoVolumeRender.h
        /usr/include
        /usr/local/include
      )

      find_library(SIMVOLEON_LIBRARY SimVoleon
        /usr/lib
        /usr/local/lib
      )

    endif()

  endif()

endif()

  set( SIMVOLEON_FOUND "NO" )
  if(SIMVOLEON_LIBRARY)
    set( SIMVOLEON_FOUND "YES" )
  endif()


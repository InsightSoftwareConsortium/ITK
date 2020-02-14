# modified by Kongbin to add support for volume rendering

set(HAS_BGUI3D "NO")
if(BGUI3D_FOUND)

  # Find the Coin3D library
  find_package(Coin3D)

  if(COIN3D_FOUND)
    include_directories( ${COIN3D_INCLUDE_DIR} )
    set( HAS_BGUI3D "YES" )
    add_definitions( -DHAS_BGUI3D )
    link_libraries( ${COIN3D_LIBRARY} )
  endif()

  # Find the SimVoleon library
  include( ${MODULE_PATH}/NewCMake/FindSIMVoleon.cmake )

  if(SIMVOLEON_FOUND)
    include_directories( ${SIMVOLEON_INCLUDE_DIR} )
    link_libraries( ${SIMVOLEON_LIBRARY} )
  endif()

  include_directories( ${brl_INCLUDE_DIR} )

endif()

# This case is for using BGUI3D in an external project
if(VXL_BGUI3D_FOUND)

  include_directories( ${VXL_BRL_INCLUDE_DIR} )
  include_directories( ${VXL_COIN3D_INCLUDE_DIR} )
  add_definitions( -DHAS_BGUI3D )
  add_definitions( -DCOIN_NOT_DLL )
  set( HAS_BGUI3D "YES" )

endif()

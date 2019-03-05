# Find the ECW library - Enhanced Compression Wavelets for JPEG2000.
#
# Sets
#   ECW_FOUND.  If false, don't try to use ecw
#   ECW_INCLUDE_DIR
#   ECW_LIBRARIES
#   VXL_USING_NATIVE_ECW native installation discovered

# The original sponsorring website of this library appears to have vanished,
# but there are still traces at http://www.gdal.org/frmt_ecw.html and a
# distribution at https://svn.zib.de/lenne3d/lib/libecw/current - IMS  7-Dec-2009.


# wrap entire file in VXL_USE_ECW option
# see core/vil/CMakeLists.txt

if( VXL_USE_ECW )

  # search for native installation
  if( NOT VXL_FORCE_V3P_J2K )
    set( ECW_FOUND "NO" )

    find_path( ECW_ncsecw_DIR NCSEcw.h
      /usr/include
      /usr/local/include
    )

    if( ECW_ncsecw_DIR )

      find_library( ECW_ncsecw_LIBRARY NCSEcw
        /usr/lib
        /usr/local/lib
        /usr/lib64
        /usr/local/lib64
      )

      find_library( ECW_ncsutil_LIBRARY NCSUtil
        /usr/lib
        /usr/local/lib
        /usr/lib64
        /usr/local/lib64
      )

      if( ECW_ncsutil_LIBRARY )
        if( ECW_ncsecw_LIBRARY )
          set( ECW_FOUND "YES" )
          set( ECW_INCLUDE_DIR ${ECW_ncsecw_DIR} )
          set( ECW_LIBRARIES ${ECW_ncsutil_LIBRARY} ${ECW_ncsecw_LIBRARY} )
          set( VXL_USING_NATIVE_ECW "YES" )
        endif()
      endif()

    endif()
  endif()

  # check for V3P installation
  if( NOT ECW_FOUND )
    set( VXL_V3P_ECW_INCLUDE ${VXL_ROOT_SOURCE_DIR}/v3p/j2k/Source/include )

    if( EXISTS ${VXL_V3P_ECW_INCLUDE}/NCSEcw.h )

      if( WIN32 )
        include(${MODULE_PATH}/NewCMake/FindWin32SDK.cmake)
        find_package(MFC)
        if( WIN32SDK_FOUND AND MFC_FOUND)
          set( ECW_FOUND "YES" )
        endif()
      else()
        set( ECW_FOUND "YES" )
        add_definitions( -DLINUX -DPOSIX )
      endif()

      if ( ECW_FOUND )
        set( ECW_INCLUDE_DIR ${VXL_V3P_ECW_INCLUDE} )
        set( ECW_LIBRARIES NCSUtil NCSEcw )
      endif()

    endif()

  endif()

endif()

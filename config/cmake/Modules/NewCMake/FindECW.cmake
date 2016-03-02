# Find the ECW library - Enhanced Compression Wavelets for JPEG2000.
#
# Sets
#   ECW_FOUND.  If false, don't try to use ecw
#   ECW_INCLUDE_DIR
#   ECW_LIBRARIES

# The original sponsorring website of this library appears to have vanished,
# but there are still traces at http://www.gdal.org/frmt_ecw.html and a
# distribution at https://svn.zib.de/lenne3d/lib/libecw/current - IMS  7-Dec-2009.
if( VXL_FORCE_V3P_J2K )
else()
set( ECW_FOUND "NO" )

find_path( ECW_INCLUDE_DIR NCSEcw.h
  /usr/include
  /usr/local/include
)

if( ECW_INCLUDE_DIR )

  find_library( ECW_ncsutil_LIBRARY NCSUtild
    /usr/lib
    /usr/local/lib
    /usr/lib64
    /usr/local/lib64
  )

  find_library( ECW_ncsecw_LIBRARY NCSEcwd
    /usr/lib
    /usr/local/lib
    /usr/lib64
    /usr/local/lib64
  )

  if( ECW_ncsutil_LIBRARY )
  if( ECW_ncsecw_LIBRARY )

    set( ECW_FOUND "YES" )
    set( ECW_LIBRARIES ${ECW_ncsutil_LIBRARY} ${ECW_ncsecw_LIBRARY} )

  endif()
  endif()


endif()
endif()

if( ECW_FOUND )
    set(VXL_USING_NATIVE_J2K "YES")
else()
include(${MODULE_PATH}/NewCMake/FindWin32SDK.cmake)
find_package(MFC)

set(J2K_SOURCES_FOUND "NO")
if(EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/j2k/Source/include/NCSEcw.h )
if(EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/j2k/Source/include/NCSUtil.h)
if(EXISTS ${VXL_ROOT_SOURCE_DIR}/v3p/j2k/Source/include/NCScnet.h)
set(J2K_SOURCES_FOUND "YES")
endif()
endif()
endif()



if( WIN32 AND J2K_SOURCES_FOUND AND WIN32SDK_FOUND AND MFC_FOUND)
    set( ECW_FOUND "YES" )
    set( ECW_INCLUDE_DIR ${VXL_ROOT_SOURCE_DIR}/v3p/j2k/Source/include)
    set( ECW_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_DIR}/include/vxl/v3p/j2k)
    set( ECW_LIBRARIES NCSEcw NCSUtil )
  endif()
 endif()

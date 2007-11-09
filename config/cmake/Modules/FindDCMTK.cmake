#
# Find the DCMTK libraries
#
# This file is used to find either a system built DCMTK library or the
# one in v3p, if provided.
#
# Provides:
# DCMTK_INCLUDE_DIR   - Directories to include to use DCMTK
# DCMTK_LIBRARIES     - Files to link against to use DCMTK
# DCMTK_FOUND         - If false, don't try to use DCMTK
# DCMTK_DIR           - (optional) Source directory for DCMTK
#
#
# Additionally,
# VXL_USING_NATIVE_DCMTK - True if we are using system DCMTK libraries.

# If this FORCE variable is unset or is false, try to find a native library
IF( VXL_FORCE_V3P_DCMTK )
ELSE( VXL_FORCE_V3P_DCMTK )
  INCLUDE( ${VXL_CMAKE_DIR}/NewCMake/FindDCMTK.cmake )
ENDIF( VXL_FORCE_V3P_DCMTK )

IF( DCMTK_FOUND )

  SET( VXL_USING_NATIVE_DCMTK "YES" )

ELSE( DCMTK_FOUND )

  # If the v3p version exists and is being build, use it
  IF( VXL_BUILD_DCMTK )

    SET( DCMTK_FOUND "YES" )
    SET( DCMTK_INCLUDE_DIR
      ${dcmtk_BINARY_DIR} # for generated osconfig
      ${dcmtk_SOURCE_DIR}/ofstd/include
      ${dcmtk_SOURCE_DIR}/dcmdata/include
      ${dcmtk_SOURCE_DIR}/dcmimgle/include
    )
    SET( DCMTK_LIBRARIES dcmtk )
    
  ENDIF( VXL_BUILD_DCMTK )

ENDIF( DCMTK_FOUND )

#-----------------------------------------------------------------------------
# Include directories for other projects installed on the system.
SET(ITK_INCLUDE_DIRS_SYSTEM "")

#-----------------------------------------------------------------------------
# Include directories from the build tree.
SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_BINARY_DIR})

# These directories are always needed.
SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${ITK_SOURCE_DIR}/Code/Algorithms
  ${ITK_SOURCE_DIR}/Code/BasicFilters
  ${ITK_SOURCE_DIR}/Code/Common 
  ${ITK_SOURCE_DIR}/Code/Numerics
  ${ITK_SOURCE_DIR}/Code/IO 
  ${ITK_SOURCE_DIR}/Code/Numerics/FEM
  ${ITK_SOURCE_DIR}/Code/Numerics/Statistics
  ${ITK_SOURCE_DIR}/Code/SpatialObject
  ${ITK_SOURCE_DIR}/Utilities/MetaIO
  ${ITK_SOURCE_DIR}/Utilities/MetaIO/SpatialObject
  ${ITK_SOURCE_DIR}/Utilities/DICOMParser
  ${ITK_BINARY_DIR}/Utilities
)

# VXL include directories.
SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Utilities/vxl/vcl
    ${ITK_SOURCE_DIR}/Utilities/vxl/v3p/netlib
    ${ITK_SOURCE_DIR}/Utilities/vxl 
    ${ITK_BINARY_DIR}/Utilities/vxl/vcl
    ${ITK_BINARY_DIR}/Utilities/vxl
  )

IF(WIN32)
  IF(NOT CYGWIN)
    SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
      ${ITK_SOURCE_DIR}/Utilities/vxl/vcl/config.win32
    )
  ENDIF(NOT CYGWIN)
ENDIF(WIN32)

# Include directory for stdlib.
IF(NOT ITK_USE_SYSTEM_STDLIB)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
      ${ITK_SOURCE_DIR}/Utilities/stdlib)
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

#-----------------------------------------------------------------------------
# Include directories needed for .cxx files in ITK.  These include
# directories will NOT be available to user projects.
SET(ITK_INCLUDE_DIRS_BUILD_TREE_CXX)

#-----------------------------------------------------------------------------
# Include directories from the install tree.
SET(ITK_INSTALL_INCLUDE_DIR "${CMAKE_INSTALL_PREFIX}/include/InsightToolkit")
SET(ITK_INCLUDE_DIRS_INSTALL_TREE ${ITK_INCLUDE_DIRS_INSTALL_TREE}
  ${ITK_INSTALL_INCLUDE_DIR}
  ${ITK_INSTALL_INCLUDE_DIR}/Algorithms
  ${ITK_INSTALL_INCLUDE_DIR}/BasicFilters
  ${ITK_INSTALL_INCLUDE_DIR}/Common 
  ${ITK_INSTALL_INCLUDE_DIR}/Numerics
  ${ITK_INSTALL_INCLUDE_DIR}/IO 
  ${ITK_INSTALL_INCLUDE_DIR}/Numerics/FEM
  ${ITK_INSTALL_INCLUDE_DIR}/Numerics/Statistics
  ${ITK_INSTALL_INCLUDE_DIR}/SpatialObject
  ${ITK_INSTALL_INCLUDE_DIR}/Utilities/MetaIO
  ${ITK_INSTALL_INCLUDE_DIR}/Utilities/MetaIO/SpatialObject
  ${ITK_INSTALL_INCLUDE_DIR}/Utilities/DICOMParser
  ${ITK_INSTALL_INCLUDE_DIR}/Utilities
)

SET(ITK_INCLUDE_DIRS_INSTALL_TREE ${ITK_INCLUDE_DIRS_INSTALL_TREE}
    ${ITK_INSTALL_INCLUDE_DIR}/Utilities/vxl/vcl
    ${ITK_INSTALL_INCLUDE_DIR}/Utilities/vxl
)

# Include directory for stdlib.
IF(NOT ITK_USE_SYSTEM_STDLIB)
  SET(ITK_INCLUDE_DIRS_INSTALL_TREE ${ITK_INCLUDE_DIRS_INSTALL_TREE}
      ${ITK_INSTALL_INCLUDE_DIR}/Utilities/stdlib)
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

#-----------------------------------------------------------------------------
# Include directories for 3rd-party utilities provided by ITK.
ITK_THIRD_PARTY_INCLUDE(ZLIB zlib)
ITK_THIRD_PARTY_INCLUDE(PNG  png)
ITK_THIRD_PARTY_INCLUDE(JPEG  jpeg)

IF(VXL_FROM_UTILITIES)
 INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/vxl/vcl
                      ${ITK_BINARY_DIR}/Utilities/vxl/vcl
                      ${ITK_SOURCE_DIR}/Utilities/vxl/v3p/netlib
                      ${ITK_BINARY_DIR}/Utilities/vxl
                      ${ITK_SOURCE_DIR}/Utilities/vxl)
  IF(WIN32)
    IF(NOT CYGWIN)
      INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/vxl/vcl/config.win32)
    ENDIF(NOT CYGWIN)
  ENDIF(WIN32)
ELSE(VXL_FROM_UTILITIES)
  INCLUDE_DIRECTORIES(
  ${ITK_BINARY_DIR}/Code/Numerics/vxl 
  ${ITK_SOURCE_DIR}/Code/Numerics/vxl 
  ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl 
  ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl 
  )
ENDIF(VXL_FROM_UTILITIES)



#-----------------------------------------------------------------------------
# Include directories for other projects installed on the system.
SET(ITK_INCLUDE_DIRS_SYSTEM "")

#-----------------------------------------------------------------------------
# Include directories from the source tree.
SET(ITK_INCLUDE_DIRS_SOURCE_TREE "")

# These directories are always needed.
SET(ITK_INCLUDE_DIRS_SOURCE_TREE ${ITK_INCLUDE_DIRS_SOURCE_TREE}
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
)

# VXL include directories.
IF(VXL_FROM_UTILITIES)
  SET(ITK_INCLUDE_DIRS_SOURCE_TREE ${ITK_INCLUDE_DIRS_SOURCE_TREE}
    ${ITK_SOURCE_DIR}/Utilities/vxl/vcl
    ${ITK_SOURCE_DIR}/Utilities/vxl/v3p/netlib
    ${ITK_SOURCE_DIR}/Utilities/vxl
  )

  IF(WIN32)
    IF(NOT CYGWIN)
      SET(ITK_INCLUDE_DIRS_SOURCE_TREE ${ITK_INCLUDE_DIRS_SOURCE_TREE}
        ${ITK_SOURCE_DIR}/Utilities/vxl/vcl/config.win32
      )
    ENDIF(NOT CYGWIN)
  ENDIF(WIN32)
ELSE(VXL_FROM_UTILITIES)
  SET(ITK_INCLUDE_DIRS_SOURCE_TREE ${ITK_INCLUDE_DIRS_SOURCE_TREE}
    ${ITK_SOURCE_DIR}/Code/Numerics/vxl 
    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl 
  )
ENDIF(VXL_FROM_UTILITIES)

# Include directory for stdlib.
IF(NOT ITK_USE_SYSTEM_STDLIB)
  SET(ITK_INCLUDE_DIRS_SOURCE_TREE ${ITK_INCLUDE_DIRS_SOURCE_TREE}
      ${ITK_SOURCE_DIR}/Utilities/stdlib)
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

#-----------------------------------------------------------------------------
# Include directories from the build tree.
SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_BINARY_DIR})

IF(VXL_FROM_UTILITIES)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_BINARY_DIR}/Utilities/vxl/vcl
    ${ITK_BINARY_DIR}/Utilities/vxl
  )
ELSE(VXL_FROM_UTILITIES)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_BINARY_DIR}/Code/Numerics/vxl 
    ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl
  )
ENDIF(VXL_FROM_UTILITIES)

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
)

IF(VXL_FROM_UTILITIES)
  SET(ITK_INCLUDE_DIRS_INSTALL_TREE ${ITK_INCLUDE_DIRS_INSTALL_TREE}
    ${ITK_INSTALL_INCLUDE_DIR}/Utilities/vxl/vcl
    ${ITK_INSTALL_INCLUDE_DIR}/Utilities/vxl
  )
ELSE(VXL_FROM_UTILITIES)
  SET(ITK_INCLUDE_DIRS_INSTALL_TREE ${ITK_INCLUDE_DIRS_INSTALL_TREE}
    ${ITK_INSTALL_INCLUDE_DIR}/Numerics/vxl 
    ${ITK_INSTALL_INCLUDE_DIR}/Numerics/vxl/vcl
  )
ENDIF(VXL_FROM_UTILITIES)

# Include directory for stdlib.
IF(NOT ITK_USE_SYSTEM_STDLIB)
  SET(ITK_INCLUDE_DIRS_INSTALL_TREE ${ITK_INCLUDE_DIRS_INSTALL_TREE}
      ${ITK_INSTALL_INCLUDE_DIR}/Utilities/stdlib)
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

#-----------------------------------------------------------------------------
# Include directories for 3rd-party utilities provided by ITK.
ITK_THIRD_PARTY_INCLUDE(ZLIB zlib)
ITK_THIRD_PARTY_INCLUDE(PNG  png)

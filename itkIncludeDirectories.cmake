#-----------------------------------------------------------------------------
# Include directories for other projects installed on the system.
SET(ITK_INCLUDE_DIRS_SYSTEM "")

IF(ITK_CSWIG_TCL)
  SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM} ${TCL_INCLUDE_PATH})
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  # Python include directory.
  SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM}
    ${PYTHON_INCLUDE_PATH})
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA)
  # Java include directories.
  SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM}
      ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_INCLUDE_PATH})
ENDIF(ITK_CSWIG_JAVA)

IF(ITK_USE_SYSTEM_VXL)
  # System VXL include directories.
  SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM}
    ${VXL_VCL_INCLUDE_DIR} ${VXL_CORE_INCLUDE_DIR}
    )
ENDIF(ITK_USE_SYSTEM_VXL)

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
  ${ITK_SOURCE_DIR}/Utilities/DICOMParser
  ${ITK_BINARY_DIR}/Utilities/DICOMParser
  ${ITK_BINARY_DIR}/Utilities/expat
  ${ITK_SOURCE_DIR}/Utilities/expat
  ${ITK_BINARY_DIR}/Utilities
)

# VXL include directories.
IF(NOT ITK_USE_SYSTEM_VXL)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Utilities/vxl/vcl
    ${ITK_SOURCE_DIR}/Utilities/vxl/core
    ${ITK_BINARY_DIR}/Utilities/vxl/vcl
    ${ITK_BINARY_DIR}/Utilities/vxl/core
    )
ENDIF(NOT ITK_USE_SYSTEM_VXL)

#-----------------------------------------------------------------------------
# Include directories needed for .cxx files in ITK.  These include
# directories will NOT be available to user projects.
SET(ITK_INCLUDE_DIRS_BUILD_TREE_CXX)
IF(NOT ITK_USE_SYSTEM_VXL)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE_CXX ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX}
    ${ITK_SOURCE_DIR}/Utilities/vxl/v3p/netlib)
ENDIF(NOT ITK_USE_SYSTEM_VXL)

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
  ${ITK_INSTALL_INCLUDE_DIR}/Utilities/DICOMParser
  ${ITK_INSTALL_INCLUDE_DIR}/Utilities
)

IF(NOT ITK_USE_SYSTEM_VXL)
  SET(ITK_INCLUDE_DIRS_INSTALL_TREE ${ITK_INCLUDE_DIRS_INSTALL_TREE}
    ${ITK_INSTALL_INCLUDE_DIR}/Utilities/vxl/vcl
    ${ITK_INSTALL_INCLUDE_DIR}/Utilities/vxl/core
    )
ENDIF(NOT ITK_USE_SYSTEM_VXL)

#-----------------------------------------------------------------------------
# Include directories for 3rd-party utilities provided by ITK.
ITK_THIRD_PARTY_INCLUDE(ZLIB zlib)
ITK_THIRD_PARTY_INCLUDE(PNG  png)
ITK_THIRD_PARTY_INCLUDE(JPEG jpeg)
ITK_THIRD_PARTY_INCLUDE(TIFF tiff)

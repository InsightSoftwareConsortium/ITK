###############################################################################
# Create the PyUtils library

SUBDIRS(PyUtils)
SUBDIRS(Tests)
SUBDIRS(itkExtras)

###############################################################################
# Add doxygen support : Experimental
SET(DOXYGEN_MAN_PATH CACHE PATH "Experimental/Python only: Directory in which doxygen-generated man pages for itk classes are or should be placed.")
IF(DOXYGEN_MAN_PATH)
  # Configure the make_doxygen_conf.py script.
  SET(CONFIG_ITK_PATH)
  FOREACH(dir ${ITK_INCLUDE_DIRS})
    SET(CONFIG_ITK_PATH "${CONFIG_ITK_PATH}'${dir}',\n")
  ENDFOREACH(dir)
  SET(CONFIG_WRAP_ITK_DOXYGEN_ROOT "${DOXYGEN_MAN_PATH}")
  SET(CONFIG_ITK_SOURCE_DIR "${ITK_SOURCE_DIR}")
  CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/make_doxygen_config.py.in" 
    "${CMAKE_CURRENT_BINARY_DIR}/make_doxygen_config.py"
    @ONLY IMMEDIATE)
ENDIF(DOXYGEN_MAN_PATH)


###############################################################################
# Configure the path-dependent itkConfig.py

# we specify these directories with relative paths  so that the file can be 
# bundled up into an install conventiently. Python will take care of turning
# the / path separator into \ on windows if needed.

IF(CMAKE_CONFIGURATION_TYPES)
  SET(CONFIG_WRAP_ITK_DATA_ROOT "../../Data")
  SET(CONFIG_WRAP_ITK_TEST_ROOT "../../Testing")
  SET(CONFIG_PYTHON_CONFIGPY_DIR "../Configuration")

  FOREACH(config ${CMAKE_CONFIGURATION_TYPES})
    # SWIG-generated libs and *.py files are sent to ${config} subdir
    # This assumes that LIBRARY_OUTPUT_PATH is WrapITK_BINARY_DIR/bin (bad!)
    # TODO: We need a better way to do this.
    SET(CONFIG_PYTHON_SWIGPY_DIR "../../lib/${config}")
    SET(CONFIG_PYTHON_SWIGLIB_DIR "../../lib/${config}")
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
                   "${WrapITK_BINARY_DIR}/Python/${config}/itkConfig.py"
                   @ONLY IMMEDIATE)
  ENDFOREACH(config)
ELSE(CMAKE_CONFIGURATION_TYPES)
  SET(CONFIG_WRAP_ITK_DATA_ROOT "../Data")
  SET(CONFIG_WRAP_ITK_TEST_ROOT "../Testing")
  SET(CONFIG_PYTHON_CONFIGPY_DIR "Configuration")

  SET(CONFIG_PYTHON_SWIGPY_DIR "../lib")
  SET(CONFIG_PYTHON_SWIGLIB_DIR "../lib")
  CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
                 "${WrapITK_BINARY_DIR}/Python/itkConfig.py"
                 @ONLY IMMEDIATE)
ENDIF(CMAKE_CONFIGURATION_TYPES)

SET(CONFIG_WRAP_ITK_TEST_ROOT "None")
SET(CONFIG_PYTHON_SWIGLIB_DIR "../lib")
SET(CONFIG_PYTHON_SWIGPY_DIR "../lib")
SET(CONFIG_PYTHON_CONFIGPY_DIR "Configuration")
CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
               "${WrapITK_BINARY_DIR}/Python/InstallOnly/itkConfig.py"
              @ONLY IMMEDIATE)

WRAP_ITK_INSTALL(/Python "${WrapITK_BINARY_DIR}/Python/InstallOnly/itkConfig.py")


###############################################################################
# Copy python files for out-of-source builds, and set up install of same.

# Create a list of Python files.
# WrapITK/Python/*.py
SET(ITK_PYTHON_FILES
  itk
  itkBase
  itkTemplate
  itkTypes
#  itkExtras/__init__
  itkLazy
  )
# Done listing files.

# Now copy these files if necessary.

IF ("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
  # In source build -- no need to copy Python file. Do need to set up the install.
  FOREACH(file ${ITK_PYTHON_FILES})
    SET(install_tgt "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
    SET(WRAP_ITK_PYTHON_INSTALL_FILES ${WRAP_ITK_PYTHON_INSTALL_FILES} "${install_tgt}")
  ENDFOREACH(file)
ELSE ("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
  SET(WRAP_ITK_PYTHON_FILES)
  ADD_CUSTOM_TARGET(copy_python_files ALL)
  FOREACH(file ${ITK_PYTHON_FILES})
    SET(src "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
    # recall that WRAP_ITK_BUILD_INTDIR expands to nothing if no config types are set,
    # or to "CMAKE_CONFIG_INTDIR/" if there are such. Likewise, WRAP_ITK_INSTALL_INTDIR
    # expands to ${BUILD_TYPE}/ or nothing.
    SET(copy_tgt "${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_BUILD_INTDIR}${file}.py")
    SET(WRAP_ITK_PYTHON_FILES ${WRAP_ITK_PYTHON_FILES} "${copy_tgt}")
    SET(install_tgt "${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_INSTALL_INTDIR}${file}.py")
    SET(WRAP_ITK_PYTHON_INSTALL_FILES ${WRAP_ITK_PYTHON_INSTALL_FILES} "${install_tgt}")
    
    # create the directory to avoid loosing case on windows
    FILE(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_INSTALL_INTDIR}${path})

    ADD_CUSTOM_COMMAND(SOURCE ${src}
      COMMAND ${CMAKE_COMMAND}
      ARGS -E copy ${src} ${copy_tgt}
      OUTPUTS ${copy_tgt}
      TARGET copy_python_files
      COMMENT "Copying ${file}.py to build dir.")
  ENDFOREACH(file)
  
  # driver command to make the copy happen.
  ADD_CUSTOM_COMMAND(
    SOURCE copy_python_files
    DEPENDS ${WRAP_ITK_PYTHON_FILES}
    TARGET copy_python_files)
ENDIF ("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")

# Install the package python files.
WRAP_ITK_INSTALL(/Python ${WRAP_ITK_PYTHON_INSTALL_FILES})

###############################################################################
# Configure and install the custom python .pth files

IF(CMAKE_CONFIGURATION_TYPES)

  FOREACH(config ${CMAKE_CONFIGURATION_TYPES})
    SET(CONFIG_WRAP_ITK_PYTHON_DIR "${CMAKE_CURRENT_BINARY_DIR}/${config}")

    # SWIG-generated libs and *.py files are sent to ${config} subdir
    # This assumes that LIBRARY_OUTPUT_PATH is WrapITK_BINARY_DIR/bin (bad!)
    # TODO: We need a better way to do this.
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/${config}/WrapITK.pth"
                   @ONLY IMMEDIATE)
  ENDFOREACH(config)
ELSE(CMAKE_CONFIGURATION_TYPES)
  SET(CONFIG_WRAP_ITK_PYTHON_DIR "${CMAKE_CURRENT_BINARY_DIR}")

  CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/WrapITK.pth"
                 @ONLY IMMEDIATE)

ENDIF(CMAKE_CONFIGURATION_TYPES)

SET(CONFIG_WRAP_ITK_PYTHON_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/Python")
CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
            "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/WrapITK.pth"
            @ONLY IMMEDIATE)

IF(PYTHON_EXECUTABLE)
  FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/det_spp.py "try:\n  import distutils.sysconfig; print distutils.sysconfig.get_python_lib(1)\nexcept: pass")
  EXEC_PROGRAM("${PYTHON_EXECUTABLE}"
    ARGS "\"${CMAKE_CURRENT_BINARY_DIR}/det_spp.py\""
    OUTPUT_VARIABLE py_spp
  )
ENDIF(PYTHON_EXECUTABLE)

STRING(REGEX REPLACE "\\\\" "/" py_spp_nobackslashes "${py_spp}")
SET(PY_SITE_PACKAGES_PATH "${py_spp_nobackslashes}" CACHE PATH "Python site-packages directory to install a .pth file pointing at WrapITK Python modules.")
MARK_AS_ADVANCED(PY_SITE_PACKAGES_PATH)
IF(PY_SITE_PACKAGES_PATH)
  INSTALL_AT_ABSOLUTE_PATH(install_wrapitk_compatibility "${PY_SITE_PACKAGES_PATH}" "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/WrapITK.pth")
ENDIF(PY_SITE_PACKAGES_PATH)



# # compile python files
# SET(PYTHON_MODULES "")
# FOREACH(m ${WRAP_ITK_MODULES})
#   SET(PYTHON_MODULES ${PYTHON_MODULES} _${m}Python)
# ENDFOREACH(m)
# 
# ADD_CUSTOM_TARGET(PythonCompile ALL
#   ${PYTHON_EXECUTABLE}
#   ${PY_SITE_PACKAGES_PATH}/../compileall.py -l
#   -d "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/lib"
#   ${WrapITK_BINARY_DIR}/lib
#   
#   COMMAND ${PYTHON_EXECUTABLE}
#   ${PY_SITE_PACKAGES_PATH}/../compileall.py -l
#   -d "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/Python"
#   ${WrapITK_BINARY_DIR}/Python
#   
#   DEPENDS SwigRuntimePython ${PYTHON_MODULES}
# )



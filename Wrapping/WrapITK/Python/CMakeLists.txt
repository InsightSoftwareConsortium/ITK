###############################################################################
# Create the PyUtils library

add_subdirectory(PyUtils)
add_subdirectory(Tests)
add_subdirectory(itkExtras)

###############################################################################
# Add doxygen support : Experimental
set(DOXYGEN_MAN_PATH CACHE PATH "Experimental/Python only: Directory in which doxygen-generated man pages for itk classes are or should be placed.")
if(DOXYGEN_MAN_PATH)
  # Configure the make_doxygen_conf.py script.
  set(CONFIG_ITK_PATH)
  foreach(dir ${ITK_INCLUDE_DIRS})
    set(CONFIG_ITK_PATH "${CONFIG_ITK_PATH}'${dir}',\n")
  endforeach(dir)
  set(CONFIG_WRAP_ITK_DOXYGEN_ROOT "${DOXYGEN_MAN_PATH}")
  set(CONFIG_ITK_SOURCE_DIR "${ITK_SOURCE_DIR}")
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/make_doxygen_config.py.in"
    "${CMAKE_CURRENT_BINARY_DIR}/make_doxygen_config.py"
    @ONLY IMMEDIATE)
endif(DOXYGEN_MAN_PATH)


###############################################################################
# Configure the path-dependent itkConfig.py

# we specify these directories with relative paths  so that the file can be
# bundled up into an install conventiently. Python will take care of turning
# the / path separator into \ on windows if needed.

if(CMAKE_CONFIGURATION_TYPES)
  set(CONFIG_WRAP_ITK_DATA_ROOT "../../Data")
  set(CONFIG_WRAP_ITK_TEST_ROOT "../../Testing")
  set(CONFIG_PYTHON_CONFIGPY_DIR "../Configuration")

  foreach(config ${CMAKE_CONFIGURATION_TYPES})
    # SWIG-generated libs and *.py files are sent to ${config} subdir
    # This assumes that LIBRARY_OUTPUT_PATH is WrapITK_BINARY_DIR/bin (bad!)
    # TODO: We need a better way to do this.
    set(CONFIG_PYTHON_SWIGPY_DIR "../../lib/${config}")
    set(CONFIG_PYTHON_SWIGLIB_DIR "../../lib/${config}")
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
                   "${WrapITK_BINARY_DIR}/Python/${config}/itkConfig.py"
                   @ONLY IMMEDIATE)
  endforeach(config)
else(CMAKE_CONFIGURATION_TYPES)
  set(CONFIG_WRAP_ITK_DATA_ROOT "../Data")
  set(CONFIG_WRAP_ITK_TEST_ROOT "../Testing")
  set(CONFIG_PYTHON_CONFIGPY_DIR "Configuration")

  set(CONFIG_PYTHON_SWIGPY_DIR "../lib")
  set(CONFIG_PYTHON_SWIGLIB_DIR "../lib")
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
                 "${WrapITK_BINARY_DIR}/Python/itkConfig.py"
                 @ONLY IMMEDIATE)
endif(CMAKE_CONFIGURATION_TYPES)

set(CONFIG_WRAP_ITK_TEST_ROOT "None")
set(CONFIG_PYTHON_SWIGLIB_DIR "../lib")
set(CONFIG_PYTHON_SWIGPY_DIR "../lib")
set(CONFIG_PYTHON_CONFIGPY_DIR "Configuration")
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
               "${WrapITK_BINARY_DIR}/Python/InstallOnly/itkConfig.py"
              @ONLY IMMEDIATE)

WRAP_ITK_INSTALL(/Python "${WrapITK_BINARY_DIR}/Python/InstallOnly/itkConfig.py")


###############################################################################
# Copy python files for out-of-source builds, and set up install of same.

# Create a list of Python files.
# WrapITK/Python/*.py
set(ITK_PYTHON_FILES
  itk
  itkBase
  itkTemplate
  itkTypes
#  itkExtras/__init__
  itkLazy
  )
# Done listing files.

# Now copy these files if necessary.

if("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
  # In source build -- no need to copy Python file. Do need to set up the install.
  foreach(file ${ITK_PYTHON_FILES})
    set(install_tgt "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
    set(WRAP_ITK_PYTHON_INSTALL_FILES ${WRAP_ITK_PYTHON_INSTALL_FILES} "${install_tgt}")
  endforeach(file)
else("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
  set(WRAP_ITK_PYTHON_FILES)
  add_custom_target(copy_python_files ALL)
  foreach(file ${ITK_PYTHON_FILES})
    set(src "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
    # recall that WRAP_ITK_BUILD_INTDIR expands to nothing if no config types are set,
    # or to "CMAKE_CONFIG_INTDIR/" if there are such. Likewise, WRAP_ITK_INSTALL_INTDIR
    # expands to ${BUILD_TYPE}/ or nothing.
    set(copy_tgt "${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_BUILD_INTDIR}${file}.py")
    set(WRAP_ITK_PYTHON_FILES ${WRAP_ITK_PYTHON_FILES} "${copy_tgt}")
    set(install_tgt "${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_INSTALL_INTDIR}${file}.py")
    set(WRAP_ITK_PYTHON_INSTALL_FILES ${WRAP_ITK_PYTHON_INSTALL_FILES} "${install_tgt}")

    # create the directory to avoid loosing case on windows
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_INSTALL_INTDIR}${path})

    add_custom_command(SOURCE ${src}
      COMMAND ${CMAKE_COMMAND}
      ARGS -E copy ${src} ${copy_tgt}
      OUTPUTS ${copy_tgt}
      TARGET copy_python_files
      COMMENT "Copying ${file}.py to build dir.")
  endforeach(file)

  # driver command to make the copy happen.
  add_custom_command(
    SOURCE copy_python_files
    DEPENDS ${WRAP_ITK_PYTHON_FILES}
    TARGET copy_python_files)
endif("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")

# Install the package python files.
WRAP_ITK_INSTALL(/Python ${WRAP_ITK_PYTHON_INSTALL_FILES})

###############################################################################
# Configure and install the custom python .pth files

if(CMAKE_CONFIGURATION_TYPES)

  foreach(config ${CMAKE_CONFIGURATION_TYPES})
    set(CONFIG_WRAP_ITK_PYTHON_DIR "${CMAKE_CURRENT_BINARY_DIR}/${config}")

    # SWIG-generated libs and *.py files are sent to ${config} subdir
    # This assumes that LIBRARY_OUTPUT_PATH is WrapITK_BINARY_DIR/bin (bad!)
    # TODO: We need a better way to do this.
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/${config}/WrapITK.pth"
                   @ONLY IMMEDIATE)
  endforeach(config)
else(CMAKE_CONFIGURATION_TYPES)
  set(CONFIG_WRAP_ITK_PYTHON_DIR "${CMAKE_CURRENT_BINARY_DIR}")

  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/WrapITK.pth"
                 @ONLY IMMEDIATE)

endif(CMAKE_CONFIGURATION_TYPES)

set(CONFIG_WRAP_ITK_PYTHON_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/Python")
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
            "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/WrapITK.pth"
            @ONLY IMMEDIATE)

if(PYTHON_EXECUTABLE)
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/det_spp.py "try:\n  import distutils.sysconfig; print distutils.sysconfig.get_python_lib(1)\nexcept: pass")
  exec_program("${PYTHON_EXECUTABLE}"
    ARGS "\"${CMAKE_CURRENT_BINARY_DIR}/det_spp.py\""
    OUTPUT_VARIABLE py_spp
  )
endif(PYTHON_EXECUTABLE)

string(REGEX REPLACE "\\\\" "/" py_spp_nobackslashes "${py_spp}")
set(PY_SITE_PACKAGES_PATH "${py_spp_nobackslashes}" CACHE PATH "Python site-packages directory to install a .pth file pointing at WrapITK Python modules.")
mark_as_advanced(PY_SITE_PACKAGES_PATH)
if(PY_SITE_PACKAGES_PATH)
  INSTALL_AT_ABSOLUTE_PATH(install_wrapitk_compatibility "${PY_SITE_PACKAGES_PATH}" "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/WrapITK.pth")
endif(PY_SITE_PACKAGES_PATH)



# # compile python files
# set(PYTHON_MODULES "")
# foreach(m ${WRAP_ITK_MODULES})
#   set(PYTHON_MODULES ${PYTHON_MODULES} _${m}Python)
# endforeach(m)
#
# add_custom_target(PythonCompile ALL
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



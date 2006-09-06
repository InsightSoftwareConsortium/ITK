# Copy python files for out-of-source builds, and set up install of same.

# Create a list of Python files.
# WrapITK/Python/itkExtras/*.py
SET(ITK_PYTHON_EXTRAS_FILES
  __init__
  )
# Done listing files.

# Now copy these files if necessary.
SET(WRAP_ITK_PYTHON_EXTRAS_INSTALL_FILES "")
IF ("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
  # In source build -- no need to copy Python file. Do need to set up the install.
  FOREACH(file ${ITK_PYTHON_EXTRAS_FILES})
    SET(install_tgt "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
    SET(WRAP_ITK_PYTHON_EXTRAS_INSTALL_FILES ${WRAP_ITK_PYTHON_EXTRAS_FILES} "${install_tgt}")
  ENDFOREACH(file)
ELSE ("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
  SET(WRAP_ITK_PYTHON_EXTRAS_FILES)
  ADD_CUSTOM_TARGET(copy_python_extras_files ALL)
  FOREACH(file ${ITK_PYTHON_EXTRAS_FILES})
    SET(src "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
    # recall that WRAP_ITK_BUILD_INTDIR expands to nothing if no config types are set,
    # or to "CMAKE_CONFIG_INTDIR/" if there are such. Likewise, WRAP_ITK_INSTALL_INTDIR
    # expands to ${BUILD_TYPE}/ or nothing.
    SET(copy_tgt "${CMAKE_CURRENT_BINARY_DIR}/${file}.py")
    SET(WRAP_ITK_PYTHON_EXTRAS_FILES ${WRAP_ITK_PYTHON_EXTRAS_FILES} "${copy_tgt}")
    SET(install_tgt "${CMAKE_CURRENT_BINARY_DIR}/${file}.py")
    SET(WRAP_ITK_PYTHON_EXTRAS_INSTALL_FILES ${WRAP_ITK_PYTHON_EXTRAS_INSTALL_FILES} "${install_tgt}")

    # create the directory to avoid loosing case on windows
    FILE(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_INSTALL_INTDIR}${path})

    ADD_CUSTOM_COMMAND(SOURCE ${src}
      COMMAND ${CMAKE_COMMAND}
      ARGS -E copy ${src} ${copy_tgt}
      OUTPUTS ${copy_tgt}
      TARGET copy_python_extras_files
      COMMENT "source copy")
  ENDFOREACH(file)

  # driver command to make the copy happen.
  ADD_CUSTOM_COMMAND(
    SOURCE copy_python_extras_files
    DEPENDS ${WRAP_ITK_PYTHON_EXTRAS_FILES}
    TARGET copy_python_extras_files)
ENDIF ("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")

# Install the package python files.
WRAP_ITK_INSTALL(/Python/itkExtras ${WRAP_ITK_PYTHON_EXTRAS_INSTALL_FILES})


# Copy python files for out-of-source builds, and set up install of same.

# Create a list of Python files.
# WrapITK/Python/itkExtras/*.py
set(ITK_PYTHON_EXTRAS_FILES
  __init__
  )
# Done listing files.

# Now copy these files if necessary.
set(WRAP_ITK_PYTHON_EXTRAS_INSTALL_FILES "")
if("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
  # In source build -- no need to copy Python file. Do need to set up the install.
  foreach(file ${ITK_PYTHON_EXTRAS_FILES})
    set(install_tgt "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
    set(WRAP_ITK_PYTHON_EXTRAS_INSTALL_FILES ${WRAP_ITK_PYTHON_EXTRAS_FILES} "${install_tgt}")
  endforeach(file)
else("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
  set(WRAP_ITK_PYTHON_EXTRAS_FILES)
  add_custom_target(copy_python_extras_files ALL)
  foreach(file ${ITK_PYTHON_EXTRAS_FILES})
    set(src "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
    # recall that WRAP_ITK_BUILD_INTDIR expands to nothing if no config types are set,
    # or to "CMAKE_CONFIG_INTDIR/" if there are such. Likewise, WRAP_ITK_INSTALL_INTDIR
    # expands to ${BUILD_TYPE}/ or nothing.
    set(copy_tgt "${CMAKE_CURRENT_BINARY_DIR}/${file}.py")
    set(WRAP_ITK_PYTHON_EXTRAS_FILES ${WRAP_ITK_PYTHON_EXTRAS_FILES} "${copy_tgt}")
    set(install_tgt "${CMAKE_CURRENT_BINARY_DIR}/${file}.py")
    set(WRAP_ITK_PYTHON_EXTRAS_INSTALL_FILES ${WRAP_ITK_PYTHON_EXTRAS_INSTALL_FILES} "${install_tgt}")

    # create the directory to avoid loosing case on windows
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_INSTALL_INTDIR}${path})

    add_custom_command(SOURCE ${src}
      COMMAND ${CMAKE_COMMAND}
      ARGS -E copy ${src} ${copy_tgt}
      OUTPUTS ${copy_tgt}
      TARGET copy_python_extras_files
      COMMENT "source copy")
  endforeach(file)

  # driver command to make the copy happen.
  add_custom_command(
    SOURCE copy_python_extras_files
    DEPENDS ${WRAP_ITK_PYTHON_EXTRAS_FILES}
    TARGET copy_python_extras_files)
endif("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")

# Install the package python files.
WRAP_ITK_BINDINGS_INSTALL(/Python/itkExtras ${WRAP_ITK_PYTHON_EXTRAS_INSTALL_FILES})

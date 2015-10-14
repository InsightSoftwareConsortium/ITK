if(ITK_USE_KWSTYLE)
  # Define and configure configuration files
  set(kwstyle_itk_configuration_file
    ${ITK_SOURCE_DIR}/Utilities/KWStyle/ITK.kws.xml
    )
  set(kwstyle_itk_examples_files_list_file
    ${ITK_BINARY_DIR}/Utilities/KWStyle/ITKExamplesFiles.txt
    )
  configure_file( # KWStyle requires that the files list be absolute paths
    ${ITK_SOURCE_DIR}/Utilities/KWStyle/ITKExamplesFiles.txt.in
    ${kwstyle_itk_examples_files_list_file}
    )
  set(kwstyle_itk_overwrite_file
    ${ITK_SOURCE_DIR}/Utilities/KWStyle/ITKOverwrite.txt
    )

  # Define formatting for error messages for output of build target
  option(KWSTYLE_USE_VIM_FORMAT
    "Set KWStyle to generate errors with a VIM-compatible format."
    OFF
    )
  option(KWSTYLE_USE_GCC_FORMAT
    "Set KWStyle to generate errors with a GCC-compatible format."
    OFF
    )
  option(KWSTYLE_USE_MSVC_FORMAT
    "Set KWStyle to generate errors with a VisualStudio-compatible format."
    ${MSVC_IDE} # default to TRUE only with a Visual Studio IDE
    )

  mark_as_advanced(KWSTYLE_USE_VIM_FORMAT)
  mark_as_advanced(KWSTYLE_USE_GCC_FORMAT)
  mark_as_advanced(KWSTYLE_USE_MSVC_FORMAT)

  set(kwstyle_editor_format "")
  if(KWSTYLE_USE_VIM_FORMAT)
    list(APPEND kwstyle_editor_format -vim)
  endif()
  if(KWSTYLE_USE_GCC_FORMAT)
    list(APPEND kwstyle_editor_format -gcc)
  endif()
  if(KWSTYLE_USE_MSVC_FORMAT)
    list(APPEND kwstyle_editor_format -msvc)
  endif()

  list(LENGTH
    kwstyle_editor_format
    kwstyle_editor_format_length
    )
  if(kwstyle_editor_format_length GREATER 1)
    message(FATAL_ERROR "At most, only one of KWSTYLE_USE_*_FORMAT can be set to TRUE.")
  endif()

  # Add build target and CTest test
  set(kwstyle_common_arguments
    -xml ${kwstyle_itk_configuration_file}
    -v
    -o ${kwstyle_itk_overwrite_file}
    )
  add_custom_target(StyleCheckExamples
    COMMAND ${KWSTYLE_EXECUTABLE}
      ${kwstyle_common_arguments}
      -D ${kwstyle_itk_examples_files_list_file}
      ${kwstyle_editor_format}
    COMMENT "Examples Style Checker"
    WORKING_DIRECTORY ${ITK_SOURCE_DIR} # the paths in kwstyle_itk_configuration_file are relative
    )
  if(BUILD_TESTING)
    set(itk-module KWStyle)
    itk_add_test(NAME KWStyleExamplesTest
      COMMAND ${KWSTYLE_EXECUTABLE}
        ${kwstyle_common_arguments}
        -D ${kwstyle_itk_examples_files_list_file}
        -gcc
      WORKING_DIRECTORY ${ITK_SOURCE_DIR}
      )
  endif(BUILD_TESTING)

endif(ITK_USE_KWSTYLE)

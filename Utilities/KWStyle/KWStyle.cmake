
find_package(KWStyle 1.0.1
  QUIET
  )
option(ITK_USE_KWSTYLE
  "Enable the use of KWStyle for checking coding style."
  ${KWSTYLE_FOUND} # default
  )
mark_as_advanced(ITK_USE_KWSTYLE)

if(ITK_USE_KWSTYLE)
  find_package(KWStyle 1.0.1
    QUIET
    REQUIRED # throw a FATAL_ERROR if KWStyle isn't found
    )

  # Define and configure configuration files
  set(KWSTYLE_ITK_CONFIGURATION_FILE
    ${ITK_SOURCE_DIR}/Utilities/KWStyle/ITK.kws.xml
    )
  set(KWSTYLE_ITK_CODE_FILES_LIST_FILE
    ${ITK_BINARY_DIR}/Utilities/KWStyle/ITKCodeFiles.txt
    )
  configure_file( # KWStyle requires that the files list be absolute paths
    ${ITK_SOURCE_DIR}/Utilities/KWStyle/ITKCodeFiles.txt.in
    ${KWSTYLE_ITK_CODE_FILES_LIST_FILE}
    )
  set(KWSTYLE_ITK_EXAMPLES_FILES_LIST_FILE
    ${ITK_BINARY_DIR}/Utilities/KWStyle/ITKExamplesFiles.txt
    )
  configure_file( # KWStyle requires that the files list be absolute paths
    ${ITK_SOURCE_DIR}/Utilities/KWStyle/ITKExamplesFiles.txt.in
    ${KWSTYLE_ITK_EXAMPLES_FILES_LIST_FILE}
    )
  set(KWSTYLE_ITK_OVERWRITE_FILE
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

  set(KWSTYLE_EDITOR_FORMAT "")
  if(KWSTYLE_USE_VIM_FORMAT)
    list(APPEND KWSTYLE_EDITOR_FORMAT -vim)
  endif()
  if(KWSTYLE_USE_GCC_FORMAT)
    list(APPEND KWSTYLE_EDITOR_FORMAT -gcc)
  endif()
  if(KWSTYLE_USE_MSVC_FORMAT)
    list(APPEND KWSTYLE_EDITOR_FORMAT -msvc)
  endif()

  list(LENGTH
    KWSTYLE_EDITOR_FORMAT
    KWSTYLE_EDITOR_FORMAT_LENGTH
    )
  if(KWSTYLE_EDITOR_FORMAT_LENGTH GREATER 1)
    message(FATAL_ERROR "At most, only one of KWSTYLE_USE_*_FORMAT can be set to TRUE.")
  endif()

  # Add build target and CTest test
  set(KWSTYLE_COMMON_ARGUMENTS
    -xml ${KWSTYLE_ITK_CONFIGURATION_FILE}
    -v
    -o ${KWSTYLE_ITK_OVERWRITE_FILE}
    )
  add_custom_target(StyleCheckCode
    COMMAND ${KWSTYLE_EXECUTABLE}
      ${KWSTYLE_COMMON_ARGUMENTS}
      -D ${KWSTYLE_ITK_CODE_FILES_LIST_FILE}
      ${KWSTYLE_EDITOR_FORMAT}
    COMMENT "Coding Style Checker"
    WORKING_DIRECTORY ${ITK_SOURCE_DIR} # the paths in KWSTYLE_CONFIGURATION_FILE are relative
    )
  add_custom_target(StyleCheckExamples
    COMMAND ${KWSTYLE_EXECUTABLE}
      ${KWSTYLE_COMMON_ARGUMENTS}
      -D ${KWSTYLE_ITK_EXAMPLES_FILES_LIST_FILE}
      ${KWSTYLE_EDITOR_FORMAT}
    COMMENT "Examples Style Checker"
    WORKING_DIRECTORY ${ITK_SOURCE_DIR} # the paths in KWSTYLE_CONFIGURATION_FILE are relative
    )
  if(BUILD_TESTING)
    set(itk-module KWStyle)
    # for uniformity and brevity, test will always output GCC-style
    itk_add_test(NAME KWStyleCodeTest
      COMMAND ${KWSTYLE_EXECUTABLE}
        ${KWSTYLE_COMMON_ARGUMENTS}
        -D ${KWSTYLE_ITK_CODE_FILES_LIST_FILE}
        -gcc
      WORKING_DIRECTORY ${ITK_SOURCE_DIR}
      )
    itk_add_test(NAME KWStyleExamplesTest
      COMMAND ${KWSTYLE_EXECUTABLE}
        ${KWSTYLE_COMMON_ARGUMENTS}
        -D ${KWSTYLE_ITK_EXAMPLES_FILES_LIST_FILE}
        -gcc
      WORKING_DIRECTORY ${ITK_SOURCE_DIR}
      )
  endif(BUILD_TESTING)

endif(ITK_USE_KWSTYLE)

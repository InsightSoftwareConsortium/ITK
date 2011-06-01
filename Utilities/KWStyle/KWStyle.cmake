option(ITK_USE_KWSTYLE "Enable the use of KWStyle for checking coding style." OFF)
if(ITK_USE_KWSTYLE)

set(itk-module KWStyle)

# Set the required KWStyle version
set(KWSTYLE_REQ_MAJOR 1)
set(KWSTYLE_REQ_MINOR 0)
set(KWSTYLE_REQ_PATCH 1)

option(KWSTYLE_USE_VIM_FORMAT "Set KWStyle to generate errors with a VIM-compatible format." OFF)
option(KWSTYLE_USE_MSVC_FORMAT "Set KWStyle to generate errors with a VisualStudio-compatible format." OFF)

find_program(KWSTYLE_EXECUTABLE
NAMES KWStyle
PATHS
/usr/local/bin
)

if(KWSTYLE_EXECUTABLE)

  execute_process(
  COMMAND ${KWSTYLE_EXECUTABLE} -version
  OUTPUT_VARIABLE KWSTYLE_VERSION_TEXT
  )

string(STRIP ${KWSTYLE_VERSION_TEXT} KWSTYLE_VERSION_TEXT)

if(KWSTYLE_VERSION_TEXT STREQUAL "Version: Not defined")
  message("This project requires a newer version of KWStyle. Please upgrade the KWStyle executable.")
else(${KWSTYLE_VERSION_TEXT} STREQUAL "Version: Not defined")

  string(LENGTH ${KWSTYLE_VERSION_TEXT} KWSTYLE_VERSION_LENGTH)
  math(EXPR KWSTYLE_VERSION_FINAL_LENGTH "${KWSTYLE_VERSION_LENGTH}-9")
  string(SUBSTRING ${KWSTYLE_VERSION_TEXT} 9 ${KWSTYLE_VERSION_FINAL_LENGTH} KWSTYLE_VERSION)

  # now parse the parts of the user given version string into variables
  string(REGEX REPLACE "^([0-9]+)\\.[0-9]+\\.[0-9]+" "\\1" KWSTYLE_MAJOR_VERSION "${KWSTYLE_VERSION}")
  string(REGEX REPLACE "^[0-9]+\\.([0-9])+\\.[0-9]+" "\\1" KWSTYLE_MINOR_VERSION "${KWSTYLE_VERSION}")
  string(REGEX REPLACE "^[0-9]+\\.[0-9]+\\.([0-9]+)" "\\1" KWSTYLE_PATCH_VERSION "${KWSTYLE_VERSION}")

  math(EXPR KWSTYLE_REQ_VERSION "${KWSTYLE_REQ_MAJOR}*10000 + ${KWSTYLE_REQ_MINOR}*100 + ${KWSTYLE_REQ_PATCH}")
  math(EXPR KWSTYLE_LONG_VERSION "${KWSTYLE_MAJOR_VERSION}*10000 + ${KWSTYLE_MINOR_VERSION}*100 + ${KWSTYLE_PATCH_VERSION}")

  # Set the minimum require version for batchmake
  if(KWSTYLE_LONG_VERSION LESS KWSTYLE_REQ_VERSION)
    message(FATAL_ERROR "This project requires a newer version of KWStyle. Please upgrade the KWStyle executable.")
  else(KWSTYLE_LONG_VERSION LESS KWSTYLE_REQ_VERSION)
    set(KWSTYLE_FOUND 1)
  endif(KWSTYLE_LONG_VERSION LESS KWSTYLE_REQ_VERSION)
endif(KWSTYLE_VERSION_TEXT STREQUAL "Version: Not defined")

if(KWSTYLE_FOUND)
#
#  Define file names
#
set(KWSTYLE_CONFIGURATION_FILE
  ${PROJECT_BINARY_DIR}/Utilities/KWStyle/ITK.kws.xml)

set(KWSTYLE_ITK_FILES_LIST
  ${PROJECT_BINARY_DIR}/Utilities/KWStyle/ITKFiles.txt)

set(KWSTYLE_ITK_REVIEW_FILES_LIST
  ${PROJECT_BINARY_DIR}/Utilities/KWStyle/ITKReviewFiles.txt)

set(KWSTYLE_ITK_OVERWRITE_FILE
  ${PROJECT_SOURCE_DIR}/Utilities/KWStyle/ITKOverwrite.txt )

#
# Configure the files
#
configure_file(
  ${PROJECT_SOURCE_DIR}/Utilities/KWStyle/ITKFiles.txt.in
  ${KWSTYLE_ITK_FILES_LIST})

configure_file(
  ${PROJECT_SOURCE_DIR}/Utilities/KWStyle/ITKReviewFiles.txt.in
  ${KWSTYLE_ITK_REVIEW_FILES_LIST})

configure_file(
  ${PROJECT_SOURCE_DIR}/Utilities/KWStyle/ITK.kws.xml.in
  ${KWSTYLE_CONFIGURATION_FILE})


#
#  Define formatting for error messages
#
set(KWSTYLE_EDITOR_FORMAT " ")
set(KWSTYLE_EDITOR_FORMAT "")

if(${CMAKE_CXX_COMPILER} MATCHES "cl.exe$")
  set(KWSTYLE_USE_MSVC_FORMAT 1)
endif(${CMAKE_CXX_COMPILER} MATCHES "cl.exe$")

if(${CMAKE_C_COMPILER} MATCHES "g[cx][cx]$")
  set(KWSTYLE_USE_VIM_FORMAT 1)
endif(${CMAKE_C_COMPILER} MATCHES "g[cx][cx]$")

if(KWSTYLE_USE_VIM_FORMAT)
  set(KWSTYLE_EDITOR_FORMAT -vim)
endif(KWSTYLE_USE_VIM_FORMAT)

if(KWSTYLE_USE_MSVC_FORMAT)
  set(KWSTYLE_EDITOR_FORMAT -msvc)
endif(KWSTYLE_USE_MSVC_FORMAT)

set(KWSTYLE_ARGUMENTS_REVIEW
  -xml ${KWSTYLE_CONFIGURATION_FILE} -v -D ${KWSTYLE_ITK_REVIEW_FILES_LIST}
  -o ${KWSTYLE_ITK_OVERWRITE_FILE} ${KWSTYLE_EDITOR_FORMAT}
  )

set(KWSTYLE_ARGUMENTS_CODE
  -xml ${KWSTYLE_CONFIGURATION_FILE} -v -D ${KWSTYLE_ITK_FILES_LIST}
  -o ${KWSTYLE_ITK_OVERWRITE_FILE} ${KWSTYLE_EDITOR_FORMAT}
  )

add_custom_command(
  OUTPUT ${ITK_BINARY_DIR}/KWStyleReviewReport.txt
  COMMAND ${KWSTYLE_EXECUTABLE}
  ARGS    ${KWSTYLE_ARGUMENTS_REVIEW}
  COMMENT "Coding Style Checker"
  )

add_custom_command(
  OUTPUT  ${ITK_BINARY_DIR}/KWStyleCodeReport.txt
  COMMAND ${KWSTYLE_EXECUTABLE}
  ARGS    ${KWSTYLE_ARGUMENTS_CODE}
  COMMENT "Coding Style Checker"
  )

add_custom_target(StyleCheckReview DEPENDS ${ITK_BINARY_DIR}/KWStyleReviewReport.txt)
add_custom_target(StyleCheckCode DEPENDS ${ITK_BINARY_DIR}/KWStyleCodeReport.txt)

itk_add_test(NAME KWStyleReviewTest
  COMMAND ${KWSTYLE_EXECUTABLE}
  ${KWSTYLE_ARGUMENTS_REVIEW})

itk_add_test(NAME KWStyleCodeTest
  COMMAND ${KWSTYLE_EXECUTABLE}
  ${KWSTYLE_ARGUMENTS_CODE})

endif(KWSTYLE_FOUND)
endif(KWSTYLE_EXECUTABLE)
endif(ITK_USE_KWSTYLE)

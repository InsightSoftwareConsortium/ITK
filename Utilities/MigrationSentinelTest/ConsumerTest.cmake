# Configures the minimal consumer against the ITK build tree.
cmake_minimum_required(VERSION 3.16)

execute_process(
  COMMAND
    "${CMAKE_COMMAND}" -S "${CONSUMER_SOURCE_DIR}" -B "${CONSUMER_BINARY_DIR}"
    -DITK_DIR:PATH=${ITK_BINARY_DIR}
  RESULT_VARIABLE _result
  OUTPUT_VARIABLE _output
  ERROR_VARIABLE _output
)

if(NOT _result EQUAL 0)
  message(FATAL_ERROR "consumer configure failed:\n${_output}")
endif()
message(STATUS "ConsumerTest passed:\n${_output}")

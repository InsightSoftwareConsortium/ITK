#
# Tests whether the compiler is Intel icc and needs -i_dynamic
#
# VARIABLE - variable to store the result to
#

macro(TESTNO_ICC_IDYNAMIC_NEEDED VARIABLE LOCAL_TEST_DIR)
  if("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
    try_run(${VARIABLE} HAVE_${VARIABLE}
      ${CMAKE_BINARY_DIR}
      ${LOCAL_TEST_DIR}/TestNO_ICC_IDYNAMIC_NEEDED.cxx
      OUTPUT_VARIABLE OUTPUT)
    message(STATUS "Check if using the Intel icc compiler, and if -i_dynamic is needed... COMPILE_RESULT...${HAVE_${VARIABLE}} RUN_RESULT...${VARIABLE}\n")
    if(HAVE_${VARIABLE}) #Test compiled, either working intel w/o -i_dynamic, or another compiler
      if(${VARIABLE})   #Intel icc compiler, -i_dynamic not needed
        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                       "-i_dynamic not needed, (Not Intel icc, or this version of Intel icc does not conflict with OS glibc.")
        message(STATUS "-i_dynamic not needed, (Not Intel icc, or this version of Intel icc does not conflict with OS glibc.")
      else(${VARIABLE}) #The compiler is not Intel icc
        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                       "The compiler ERROR--This should never happen")
        message(STATUS "The compiler ERROR--This should never happen")
      endif(${VARIABLE})
    else(HAVE_${VARIABLE})  #Test did not compile, either badly broken compiler, or intel -i_dynamic needed
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "\tThe -i_dynamic compiler flag is needed for the Intel icc compiler on this platform.\n")
      message("The -i_dynamic compiler flag is needed for the Intel icc compiler on this platform.")
    endif(HAVE_${VARIABLE})
    file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log "TestNO_ICC_IDYNAMIC_NEEDED produced following output:\n${OUTPUT}\n\n")
  endif("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
endmacro(TESTNO_ICC_IDYNAMIC_NEEDED)

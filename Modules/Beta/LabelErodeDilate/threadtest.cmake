macro(EXEC_CHECK CMD ARGS )
    execute_process(COMMAND ${CMD} ${ARGS} RESULT_VARIABLE CMD_RESULT)
    if(CMD_RESULT)
        message(FATAL_ERROR "Error running ${CMD}")
    endif()
endmacro()

set (CMD1 ${CMAKE_CURRENT_BINARY_DIR}/labelSetsDilatePerf)

#message(STATUS "${TESTIMAGE}")

execute_process(COMMAND ${CMD1} -i ${TESTIMAGE} -o dil_4.mha --repetitions 1 --threads 4 -r 50 RESULT_VARIABLE CMD_RESULT )

if(CMD_RESULT)
     message(FATAL_ERROR "Error running ${CMD}")
endif()

execute_process(COMMAND ${CMD1} -i ${TESTIMAGE} -o dil_1.mha --repetitions 1 --threads 1 -r 50 RESULT_VARIABLE CMD_RESULT )

if(CMD_RESULT)
   message(FATAL_ERROR "Error running ${CMD}")
endif()

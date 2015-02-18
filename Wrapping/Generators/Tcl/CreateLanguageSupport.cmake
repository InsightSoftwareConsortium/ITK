macro(ADD_TCL_TYPEMAP simple_name cpp_name swig_name template_params)
  # write me
endmacro()

macro(TCL_SUPPORT_CONFIGURE_FILES)
  # write me
endmacro()

macro(END_WRAPPER_LIBRARY_TCL)

  set(cpp_files )

  set(modules )

  foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})

    get_filename_component(base_name ${source} NAME_WE)
    string(REGEX REPLACE "^wrap_" "" group_name "${base_name}")

    # create the swig interface for all the groups in the module
    #
    set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${base_name}.i")
    set(lib ${group_name}Tcl)
    set(cpp_file "${CMAKE_CURRENT_BINARY_DIR}/${base_name}Tcl.cpp")

    add_custom_command(
      OUTPUT ${cpp_file}
      COMMAND swig -c++ -tcl -fcompact -O -Werror -w508 -w312 -w314 -w509 -w302 -w362
      -w365 -w366 -w367 -w368 -w378 -w503 # operator???, to be suppressed later...
      -l${WrapITK_SOURCE_DIR}/Tcl/tcl.i
      -o ${cpp_file}
#      -I${WRAPPER_MASTER_INDEX_OUTPUT_DIR}
      -outdir ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
      ${interface_file}
      WORKING_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/tcl
      DEPENDS ${interface_file} ${WrapITK_SOURCE_DIR}/Tcl/tcl.i
    )
    add_custom_target(${base_name}SwigTcl DEPENDS ${cpp_file})

    list(APPEND cpp_files ${cpp_file})

    list(APPEND modules ${group_name})

    add_library(${lib} SHARED ${cpp_file})
    set_target_properties(${lib} PROPERTIES PREFIX "")
    target_link_libraries(${lib} ${WRAPPER_LIBRARY_LINK_LIBRARIES} ${TCL_LIBRARY})

  endforeach()

#  add_library(${WRAPPER_LIBRARY_NAME}Tcl MODULE ${cpp_files})
#  set_target_properties(${WRAPPER_LIBRARY_NAME}Tcl PROPERTIES PREFIX "_")
#  target_link_libraries( ${WRAPPER_LIBRARY_NAME}Tcl
#    ${WRAPPER_LIBRARY_LINK_LIBRARIES}
#    ${TCL_LIBRARY}
#  )

  add_custom_target(${WRAPPER_LIBRARY_NAME}SwigTcl DEPENDS ${cpp_files})

  add_custom_target(${WRAPPER_LIBRARY_NAME}Tcl DEPENDS ${modules})


endmacro()

###############################################################################
option(WRAP_ITK_USE_SYSTEM_GCCXML "Use system gccxml. If OFF, gccxml is built as an external project." OFF)

if(WRAP_ITK_USE_SYSTEM_GCCXML)
  find_package(GCCXML REQUIRED)
  set(GCCXML ${GCCXML} CACHE INTERNAL "gccxml executable.")
else()
  include(ExternalProject)
  # build gccxml as an external project
  ExternalProject_add(GCC_XML
  CVS_REPOSITORY ":pserver:anoncvs@www.gccxml.org:/cvsroot/GCC_XML"
  CVS_MODULE "gccxml"
#  CVS_TAG -r ITK-3-20
  UPDATE_COMMAND ""
  PREFIX gccxml
  CMAKE_ARGS
    -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
    -DCMAKE_CXX_COMPILER_ARG1:STRING=${CMAKE_CXX_COMPILER_ARG1}
    -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
    -DCMAKE_C_COMPILER_ARG1:STRING=${CMAKE_C_COMPILER_ARG1}
    -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
    -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_CURRENT_BINARY_DIR}/gccxml/
    -DBUILD_EXAMPLES:BOOL=OFF
    -DBUILD_TESTING:BOOL=OFF
  )
  set(GCCXML ${CMAKE_CURRENT_BINARY_DIR}/gccxml/bin/gccxml CACHE INTERNAL "gccxml executable.")
endif()

###############################################################################
# install the files requires for gccxml
if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  WRAP_ITK_INSTALL(/Configuration/Languages/GccXML CMakeLists.txt)
  WRAP_ITK_INSTALL(/Configuration/Languages/GccXML gcc_xml.inc.in)
  WRAP_ITK_INSTALL(/Configuration/Languages/GccXML wrap_.cxx.in)
endif(NOT EXTERNAL_WRAP_ITK_PROJECT)


###############################################################################
# hide cable swig dir in external projects - it is safer to use the same as the
# one used to build wrapitk
if(EXTERNAL_WRAP_ITK_PROJECT)
  mark_as_advanced(GCCXML)
endif(EXTERNAL_WRAP_ITK_PROJECT)


###############################################################################
# store the current dir, so it can be reused later
set(WRAP_ITK_GCCXML_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "gccxml source dir")
set(WRAP_ITK_GCCXML_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "gccxml binary dir")


set(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${PROJECT_BINARY_DIR}/Typedefs")
set(WRAPPER_SWIG_LIBRARY_OUTPUT_DIR "${PROJECT_BINARY_DIR}/SWIG")



macro(WRAP_INCLUDE_GCCXML include_file)
  if("${include_file}" MATCHES "<.*>")
    set(GCC_XML_INCLUDES "${GCC_XML_INCLUDES}#include ${include_file}\n")
  else("${include_file}" MATCHES "<.*>")
    set(GCC_XML_INCLUDES "${GCC_XML_INCLUDES}#include \"${include_file}\"\n")
  endif("${include_file}" MATCHES "<.*>")
endmacro(WRAP_INCLUDE_GCCXML)


macro(ADD_SIMPLE_TYPEDEF_GCCXML wrap_class swig_name)
  set(GCC_XML_TYPEDEFS "${GCC_XML_TYPEDEFS}      typedef ${wrap_class} ${swig_name};\n")
endmacro(ADD_SIMPLE_TYPEDEF_GCCXML)


macro(WRAP_MODULE_GCCXML module)
  # clear the typedefs and the includes
  set(GCC_XML_TYPEDEFS )
  set(GCC_XML_INCLUDES )
  set(GCC_XML_FORCE_INSTANTIATE )
endmacro(WRAP_MODULE_GCCXML)

macro(END_WRAP_MODULE_GCCXML module)
  # write the wrap_*.cxx file
  #
  # Global vars used: WRAPPER_INCLUDE_FILES WRAPPER_MODULE_NAME and WRAPPER_TYPEDEFS
  # Global vars modified: none

#   message("${GCC_XML_INCLUDES}${GCC_XML_TYPEDEFS}")

  # Create the cxx file.
  set(file_name "${module}.cxx")
  set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${file_name}")
  set(CONFIG_WRAPPER_INCLUDES "${GCC_XML_INCLUDES}")
  set(CONFIG_WRAPPER_MODULE_NAME "${WRAPPER_MODULE_NAME}")
  set(CONFIG_WRAPPER_TYPEDEFS "${GCC_XML_TYPEDEFS}")
  set(CONFIG_WRAPPER_FORCE_INSTANTIATE "${GCC_XML_FORCE_INSTANTIATE}")
  configure_file("${WRAP_ITK_GCCXML_SOURCE_DIR}/wrap_.cxx.in"
    "${cxx_file}" @ONLY IMMEDIATE)

  # generate the xml file
  set(gccxml_inc_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/gcc_xml.inc")
  set(xml_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.xml")

  if(WRAP_ITK_USE_CCACHE)
    add_custom_command(
      OUTPUT ${xml_file}
      COMMAND "${CCACHE_EXECUTABLE}" "${GCCXML}"
            -c
            -fxml-start=_cable_
            -o ${xml_file}
            ${CONFIG_GCCXML_INC_CONTENTS}
            -DCSWIG
            -DCABLE_CONFIGURATION
            -DITK_MANUAL_INSTANTIATION
            ${cxx_file}
      DEPENDS ${GCCXML} ${cxx_file}
    )
  else(WRAP_ITK_USE_CCACHE)
    add_custom_command(
      OUTPUT ${xml_file}
      COMMAND ${GCCXML}
            -fxml-start=_cable_
            -fxml=${xml_file}
            --gccxml-gcc-options ${gccxml_inc_file}
            -DCSWIG
            -DCABLE_CONFIGURATION
            -DITK_MANUAL_INSTANTIATION
            ${cxx_file}
      DEPENDS ${GCCXML} ${cxx_file} ${gccxml_inc_file}
    )
  endif(WRAP_ITK_USE_CCACHE)

  set(GCC_XML_OUTPUT_FILES ${GCC_XML_OUTPUT_FILES} ${xml_file})

endmacro(END_WRAP_MODULE_GCCXML)


macro(ADD_ONE_TYPEDEF_GCCXML  wrap_method wrap_class swig_name template_params)
  # insert a blank line to separate the classes
  set(GCC_XML_TYPEDEFS "${GCC_XML_TYPEDEFS}\n")
  if("${wrap_method}" MATCHES "FORCE_INSTANTIATE")
    # add a peace of code for type instantiation
    set(GCC_XML_FORCE_INSTANTIATE "${GCC_XML_FORCE_INSTANTIATE}  sizeof(${swig_name});\n")
  endif("${wrap_method}" MATCHES "FORCE_INSTANTIATE")
endmacro(ADD_ONE_TYPEDEF_GCCXML)

macro(WRAP_LIBRARY_GCCXML library_name)
  # create the files used to pass the file to include to gccxml
  set(gccxml_inc_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/gcc_xml.inc")
  set(CONFIG_GCCXML_INC_CONTENTS)
  get_directory_property(include_dir_list INCLUDE_DIRECTORIES)

  if(WRAP_ITK_USE_CCACHE)
    # it is used on the command line
    foreach(dir ${include_dir_list})
      set(CONFIG_GCCXML_INC_CONTENTS ${CONFIG_GCCXML_INC_CONTENTS} -I${dir})
    endforeach(dir)
  else(WRAP_ITK_USE_CCACHE)
    # really create the content of the file
    foreach(dir ${include_dir_list})
      set(CONFIG_GCCXML_INC_CONTENTS "${CONFIG_GCCXML_INC_CONTENTS}-I${dir}\n")
    endforeach(dir)
    # and the file.
    configure_file("${WRAP_ITK_GCCXML_SOURCE_DIR}/gcc_xml.inc.in" "${gccxml_inc_file}"
      @ONLY IMMEDIATE)
  endif(WRAP_ITK_USE_CCACHE)
  set(GCC_XML_OUTPUT_FILES )
endmacro(WRAP_LIBRARY_GCCXML)

macro(END_WRAP_LIBRARY_GCCXML)
  add_custom_target(${WRAPPER_LIBRARY_NAME}GccXML DEPENDS ${GCC_XML_OUTPUT_FILES})
  if(NOT WRAP_ITK_USE_SYSTEM_GCCXML)
    add_dependencies(${WRAPPER_LIBRARY_NAME}GccXML GCC_XML)
  endif()
  set(${WRAPPER_LIBRARY_NAME}XmlFiles ${GCC_XML_OUTPUT_FILES} CACHE INTERNAL "Internal ${WRAPPER_LIBRARY_NAME}Xml file list.")
endmacro(END_WRAP_LIBRARY_GCCXML)

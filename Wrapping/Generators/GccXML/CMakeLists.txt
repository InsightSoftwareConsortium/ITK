###############################################################################
option(ITK_USE_SYSTEM_GCCXML "Use system gccxml. If OFF, gccxml is built as an external project." OFF)
mark_as_advanced(ITK_USE_SYSTEM_GCCXML)

set(gccxml_url http://itk.org/files/gccxml/gccxml-2014-06-10.tar.gz)
set(gccxml_md5 3ca85731d1d95a9d433eed865d2079c0)

set(gccxml_ep ${CMAKE_CURRENT_BINARY_DIR}/gccxml/bin/gccxml)
if(WIN32)
  set(gccxml_ep "${gccxml_ep}.exe")
endif()

if(ITK_USE_SYSTEM_GCCXML)
  # the path set for the EP build prevents find_package to do its job
  if("${GCCXML}" STREQUAL "${gccxml_ep}")
    unset(GCCXML CACHE)
  endif()
  find_package(GCCXML REQUIRED)
  set(GCCXML ${GCCXML} CACHE FILEPATH "gccxml executable." FORCE)
else()
  include(ExternalProject)
  # build gccxml as an external project
  if(WIN32)
    # windows build requires an extra step after the install. We use the test to run that step
    set(test TEST_COMMAND ${CMAKE_CURRENT_BINARY_DIR}/gccxml/bin/gccxml_vcconfig.bat)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /D_CRT_SECURE_NO_WARNINGS")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /D_CRT_SECURE_NO_WARNINGS")
  else()
    set(test )
  endif()
  itk_download_attempt_check(GCC_XML)
  ExternalProject_Add(GCC_XML
  URL ${gccxml_url}
  URL_MD5 ${gccxml_md5}
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
    -DBUILD_TESTING:BOOL=OFF
  ${test}
  STEP_TARGETS install
  )
  set_property(TARGET GCC_XML PROPERTY _EP_TEST_AFTER_INSTALL 1)
  set_property(TARGET GCC_XML PROPERTY _EP_TEST_BEFORE_INSTALL 0)
  set(GCCXML ${gccxml_ep} CACHE FILEPATH "gccxml executable." FORCE)
endif()
mark_as_advanced(GCCXML)

###############################################################################
# install the files requires for gccxml
if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  WRAP_ITK_INSTALL(/Configuration/Generators/GccXML CMakeLists.txt)
  WRAP_ITK_INSTALL(/Configuration/Generators/GccXML gcc_xml.inc.in)
  WRAP_ITK_INSTALL(/Configuration/Generators/GccXML wrap_.cxx.in)
endif()


###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_GCCXML_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "gccxml source dir")
set(ITK_WRAP_GCCXML_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "gccxml binary dir")


set(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${PROJECT_BINARY_DIR}/Typedefs")
set(WRAPPER_SWIG_LIBRARY_OUTPUT_DIR "${PROJECT_BINARY_DIR}/SWIG")



macro(itk_wrap_include_gccxml include_file)
  if("${include_file}" MATCHES "<.*>")
    set(GCC_XML_INCLUDES "${GCC_XML_INCLUDES}#include ${include_file}\n")
  else()
    set(GCC_XML_INCLUDES "${GCC_XML_INCLUDES}#include \"${include_file}\"\n")
  endif()
endmacro()


macro(itk_wrap_simple_type_gccxml wrap_class swig_name)
  set(GCC_XML_TYPEDEFS "${GCC_XML_TYPEDEFS}      typedef ${wrap_class} ${swig_name};\n")
  set(GCC_XML_FORCE_INSTANTIATE "${GCC_XML_FORCE_INSTANTIATE}  sizeof(${swig_name});\n")
endmacro()


macro(itk_wrap_submodule_gccxml module)
  # clear the typedefs and the includes
  set(GCC_XML_TYPEDEFS )
  set(GCC_XML_INCLUDES )
  set(GCC_XML_FORCE_INSTANTIATE )
endmacro()

macro(itk_end_wrap_submodule_gccxml module)
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
  configure_file("${ITK_WRAP_GCCXML_SOURCE_DIR}/wrap_.cxx.in"
    "${cxx_file}" @ONLY)
  set(ADDITIONAL_GCC_XML_FLAGS "")
  if(MSVC90)
    set( ADDITIONAL_GCC_XML_FLAGS "-D\"_HAS_TR1=0\"" )
  endif()

  # generate the xml file
  set(gccxml_inc_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/gcc_xml.inc")
  set(xml_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.xml")

  set(_gccxml_depends)
  if(NOT ITK_USE_SYSTEM_GCCXML)
    # ExternalProject target for GCC_XML.
    set(_gccxml_depends GCC_XML)
  endif()
  if(ITK_USE_CCACHE)
    add_custom_command(
      OUTPUT ${xml_file}
      COMMAND "${CCACHE_EXECUTABLE}" "${GCCXML}"
            -c
            -fxml-start=_cable_
            -o ${xml_file}
            ${CONFIG_GCCXML_INC_CONTENTS}
            -DCABLE_CONFIGURATION
            -DITK_MANUAL_INSTANTIATION
            ${cxx_file}
      DEPENDS ${_gccxml_depends} ${cxx_file}
    )
  else()
    add_custom_command(
      OUTPUT ${xml_file}
      COMMAND ${GCCXML}
            -fxml-start=_cable_
            -fxml=${xml_file}
            --gccxml-gcc-options ${gccxml_inc_file}
            -DCABLE_CONFIGURATION
            -DITK_MANUAL_INSTANTIATION
            ${ADDITIONAL_GCC_XML_FLAGS}
            ${cxx_file}
      DEPENDS ${_gccxml_depends} ${cxx_file} ${gccxml_inc_file}
    )
  endif()

  set(GCC_XML_OUTPUT_FILES ${GCC_XML_OUTPUT_FILES} ${xml_file})

endmacro()


macro(itk_wrap_one_type_gccxml  wrap_method wrap_class swig_name template_params)
  # insert a blank line to separate the classes
  set(GCC_XML_TYPEDEFS "${GCC_XML_TYPEDEFS}\n")
  # add a peace of code for type instantiation
  set(GCC_XML_FORCE_INSTANTIATE "${GCC_XML_FORCE_INSTANTIATE}\n")
endmacro()

macro(itk_wrap_module_gccxml library_name)
  # create the files used to pass the file to include to gccxml
  set(gccxml_inc_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/gcc_xml.inc")
  set(CONFIG_GCCXML_INC_CONTENTS)
  get_directory_property(include_dir_list INCLUDE_DIRECTORIES)
  list( REMOVE_DUPLICATES include_dir_list )

  if(ITK_USE_CCACHE)
    # it is used on the command line
    foreach(dir ${include_dir_list})
      set(CONFIG_GCCXML_INC_CONTENTS ${CONFIG_GCCXML_INC_CONTENTS} -I${dir})
    endforeach()
  else()
    # really create the content of the file
    foreach(dir ${include_dir_list})
      set(CONFIG_GCCXML_INC_CONTENTS "${CONFIG_GCCXML_INC_CONTENTS}-I${dir}\n")
    endforeach()
    # and the file.
    configure_file("${ITK_WRAP_GCCXML_SOURCE_DIR}/gcc_xml.inc.in" "${gccxml_inc_file}"
      @ONLY)
  endif()
  set(GCC_XML_OUTPUT_FILES )
endmacro()

macro(itk_end_wrap_module_gccxml)
  add_custom_target(${WRAPPER_LIBRARY_NAME}GccXML DEPENDS ${GCC_XML_OUTPUT_FILES})
  set(${WRAPPER_LIBRARY_NAME}XmlFiles ${GCC_XML_OUTPUT_FILES} CACHE INTERNAL "Internal ${WRAPPER_LIBRARY_NAME}Xml file list.")
endmacro()

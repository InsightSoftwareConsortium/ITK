find_package(Doxygen REQUIRED)

###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_DOC_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "doc source dir")
set(ITK_WRAP_DOC_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "doc binary dir")

###############################################################################
macro(itk_wrap_module_DOC library_name)
  set(ITK_WRAP_DOC_DOXYGEN_HEADERS )  # doxygen headers to process in this lib
  set(ITK_WRAP_DOC_DOXYGEN_XML_FILES )  # xml files produced by doxygen in this lib
  set(ITK_WRAP_DOC_DOCSTRING_FILES )  # swig docstring files produced by doxygen in this lib
endmacro()


###############################################################################
macro(itk_wrap_named_class_DOC class swig_name)
  if("${WRAPPER_WRAP_METHOD}" STREQUAL "ENUM")
    # doc is not generated in the same way for enum. Just ignore it
    set(ITK_WRAP_DOC_GENERATE_DOXY2SWIG_INPUT OFF)
  else()
    set(ITK_WRAP_DOC_GENERATE_DOXY2SWIG_INPUT OFF)
    get_directory_property(dirs INCLUDE_DIRECTORIES)
    set(paths )
    foreach(dir ${dirs})
      list(APPEND paths "${dir}/${swig_name}.h")
    endforeach()
    file(GLOB doc_path ${paths})
    if(doc_path AND "${class}" MATCHES "^itk::")
      # store the header
      list(APPEND ITK_WRAP_DOC_DOXYGEN_HEADERS "${doc_path}")
      # and the produced file
      string(REPLACE "::" "_" base_name "${class}")
      # some simple computations to find the xml file produced for this class
      string(REGEX REPLACE "([A-Z])" "\\1" xmlname ${class})
      string(REGEX REPLACE ":" "_1" xmlname ${xmlname})
      # Converts camel case names to snake case.
      string(REGEX REPLACE "([A-Z])" "_\\1" xmlname ${xmlname})
      string(TOLOWER  ${xmlname} xmlname)
      list(APPEND ITK_WRAP_DOC_DOXYGEN_XML_FILES "${CMAKE_CURRENT_BINARY_DIR}/Doc/xml/class${xmlname}.xml")
      # the doxy2swig input
      set(ITK_WRAP_DOC_DOXY2SWIG_INPUT "${ITK_WRAP_DOC_DOXY2SWIG_INPUT}\n${CMAKE_CURRENT_BINARY_DIR}/Doc/xml/class${xmlname}.xml\t${class}")
      set(ITK_WRAP_DOC_GENERATE_DOXY2SWIG_INPUT ON)
    endif()
  endif()

endmacro()

macro(itk_wrap_one_type_DOC wrap_method wrap_class swig_name template_params)
  if(ITK_WRAP_DOC_GENERATE_DOXY2SWIG_INPUT)
    set(ITK_WRAP_DOC_DOXY2SWIG_INPUT "${ITK_WRAP_DOC_DOXY2SWIG_INPUT}\t${swig_name}")
  endif()
endmacro()

###############################################################################
macro(itk_end_wrap_module_DOC)
    # create the target doc dir
    set(library_doc_build_dir "${CMAKE_CURRENT_BINARY_DIR}/Doc") # Library documentation interface files building directory
                                                                 # TODO: direct name of the library dir?
    file(MAKE_DIRECTORY ${library_doc_build_dir})

    # configure doxygen input file.
    # be sure to not include a header several times
    if(NOT "${ITK_WRAP_DOC_DOXYGEN_HEADERS}" STREQUAL "")
      UNIQUE(headers "${ITK_WRAP_DOC_DOXYGEN_HEADERS}")
    endif()
    set(library_doxygen_config_file ${library_doc_build_dir}/doxygen.config)
    set(ITK_WRAP_DOC_DOXYGEN_HEADERS_FORMATED)
    foreach(header ${headers})
      set(ITK_WRAP_DOC_DOXYGEN_HEADERS_FORMATED "${ITK_WRAP_DOC_DOXYGEN_HEADERS_FORMATED}           \"${header}\"\\\n")
    endforeach()
    set(ITK_WRAP_DOC_LIBRARY_DIR "${library_doc_build_dir}")
    configure_file("${ITK_WRAP_DOC_SOURCE_DIR}/doxygen.config.in"
      "${library_doxygen_config_file}"
      @ONLY)

    # which files are produced?
    set(outputs ${ITK_WRAP_DOC_DOXYGEN_XML_FILES})

    # run doxygen
    add_custom_command(
      OUTPUT ${outputs} "${library_doc_build_dir}/xml/combine.xslt"  # this file is always produced and avoid an error if ${outputs} is empty
      COMMAND "${DOXYGEN_EXECUTABLE}" "${library_doxygen_config_file}"
#      WORKING_DIRECTORY ${ITK_WRAP_DOC_BINARY_DIR}
      DEPENDS ${ITK_WRAP_DOC_DOXYGEN_HEADERS} "${library_doxygen_config_file}"
      COMMENT "-- Wrapping library ${WRAPPER_LIBRARY_NAME}: Constructing documentation xml structure."
    )

    add_custom_target(${WRAPPER_LIBRARY_NAME}Doxygen ALL DEPENDS ${outputs} ${ITK_WRAP_DOC_DOCSTRING_FILES})

endmacro()

macro(itk_wrap_submodule_DOC module)
  set(ITK_WRAP_DOC_DOXY2SWIG_INPUT )  # the c++ name - swig names definitions
endmacro()

###############################################################################
# This macro is called once per module
# Global variable WRAPPER_MODULE_NAME can be used
# in the macro to current module name
#
macro(itk_end_wrap_submodule_DOC)
    set(doxy2swig_config_file ${CMAKE_CURRENT_BINARY_DIR}/Doc/${WRAPPER_MODULE_NAME}.conf)
    configure_file("${ITK_WRAP_DOC_SOURCE_DIR}/itk_doxy2swig.conf.in"
      "${doxy2swig_config_file}"
      @ONLY)
    # run itk_doxy2swig
    set(itk_doxy2swig_py "${ITK_WRAP_DOC_SOURCE_DIR}/itk_doxy2swig.py")
    set(swig_doc_interface_file ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_MODULE_NAME}_doc.i)
    add_custom_command(
    OUTPUT ${swig_doc_interface_file}
    COMMAND ${PYTHON_EXECUTABLE} ${itk_doxy2swig_py} ${doxy2swig_config_file} ${swig_doc_interface_file}
    DEPENDS ${ITK_WRAP_DOC_DOXYGEN_XML_FILES} ${doxy2swig_config_file} ${itk_doxy2swig_py}
#    COMMENT "-- Wrapping library ${WRAPPER_MODULE_NAME}: Generating swig interface for inline documentation."
    )
    list(APPEND ITK_WRAP_DOC_DOCSTRING_FILES ${swig_doc_interface_file})
endmacro()

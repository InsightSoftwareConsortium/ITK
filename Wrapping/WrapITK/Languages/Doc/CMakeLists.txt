find_package(Doxygen REQUIRED)

option(WRAP_ITK_DOC_MAN "Generate unix manual pages." ON)

###############################################################################
# install the files requires for doxygen
if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  WRAP_ITK_INSTALL(/Configuration/Languages/Doc CMakeLists.txt)
  WRAP_ITK_INSTALL(/Configuration/Languages/Doc doxygen.config.in)
  WRAP_ITK_INSTALL(/Configuration/Languages/Doc itk_doxy2swig.conf.in)
  WRAP_ITK_INSTALL(/Configuration/Languages/Doc doxy2swig.py)
  WRAP_ITK_INSTALL(/Configuration/Languages/Doc itk_doxy2swig.py)
endif(NOT EXTERNAL_WRAP_ITK_PROJECT)

###############################################################################
# store the current dir, so it can be reused later
set(WRAP_ITK_DOC_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "doc source dir")
set(WRAP_ITK_DOC_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "doc binary dir")

set(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${PROJECT_BINARY_DIR}/Typedefs" CACHE INTERNAL "typedefs dir")


###############################################################################
# the var to store the file produced by doxygen
set(WRAP_ITK_DOC_DOXYGEN_INSTALLED_PAGES "" CACHE INTERNAL "man pages produced by doxygen and already installed")


###############################################################################
macro(WRAP_LIBRARY_DOC library_name)
  set(WRAP_ITK_DOC_DOXYGEN_HEADERS )  # doxygen headers to process in this lib
  set(WRAP_ITK_DOC_DOXYGEN_PAGES )  # pages produced by doxygen in this lib
  set(WRAP_ITK_DOC_DOXYGEN_XML_FILES )  # xml files produced by doxygen in this lib
  set(WRAP_ITK_DOC_DOCSTRING_FILES )  # swig docstring files produced by doxygen in this lib
endmacro(WRAP_LIBRARY_DOC)


###############################################################################
macro(WRAP_NAMED_CLASS_DOC class swig_name)
  if("${WRAPPER_WRAP_METHOD}" STREQUAL "ENUM")
    # doc is not generated in the same way for enum. Just ignore it
    set(WRAP_ITK_DOC_GENERATE_DOXY2SWIG_INPUT OFF)
  else("${WRAPPER_WRAP_METHOD}" STREQUAL "ENUM")
    set(WRAP_ITK_DOC_GENERATE_DOXY2SWIG_INPUT OFF)
    get_directory_property(dirs INCLUDE_DIRECTORIES)
    set(paths )
    foreach(dir ${dirs})
      set(paths ${paths} "${dir}/${swig_name}.h")
    endforeach(dir)
    file(GLOB doc_path ${paths})
    if(doc_path AND "${class}" MATCHES "^itk::")
      # store the header
      set(WRAP_ITK_DOC_DOXYGEN_HEADERS ${WRAP_ITK_DOC_DOXYGEN_HEADERS} "${doc_path}")
      # and the produced file
      string(REPLACE "::" "_" base_name "${class}")
      set(page "${CMAKE_CURRENT_BINARY_DIR}/Doc/man3/${base_name}.3")
      set(WRAP_ITK_DOC_DOXYGEN_PAGES "${WRAP_ITK_DOC_DOXYGEN_PAGES};${page}")
      # and in install the manpage, if requested, and if not yet installed from another dir
      if(WRAP_ITK_DOC_MAN AND NOT "${WRAP_ITK_DOC_DOXYGEN_INSTALLED_PAGES}" MATCHES "(^|;)${base_name}\\.3(;|$)")
        WRAP_ITK_INSTALL(/Doc/man3 "${page}")
        set(WRAP_ITK_DOC_DOXYGEN_INSTALLED_PAGES ${WRAP_ITK_DOC_DOXYGEN_INSTALLED_PAGES} "${base_name}.3" CACHE INTERNAL "man pages produced by doxygen and already installed")
      endif(WRAP_ITK_DOC_MAN AND NOT "${WRAP_ITK_DOC_DOXYGEN_INSTALLED_PAGES}" MATCHES "(^|;)${base_name}\\.3(;|$)")

      # some simple computations to find the xml file produced for this class
      string(REGEX REPLACE "([A-Z])" "_\\1" xmlname ${class})
      string(REGEX REPLACE ":" "_1" xmlname ${xmlname})
      string(TOLOWER  ${xmlname} xmlname)
      set(WRAP_ITK_DOC_DOXYGEN_XML_FILES ${WRAP_ITK_DOC_DOXYGEN_XML_FILES} "${CMAKE_CURRENT_BINARY_DIR}/Doc/xml/class${xmlname}.xml")

      # the doxy2swig input
      set(WRAP_ITK_DOC_DOXY2SWIG_INPUT "${WRAP_ITK_DOC_DOXY2SWIG_INPUT}\n${CMAKE_CURRENT_BINARY_DIR}/Doc/xml/class${xmlname}.xml\t${class}")
      set(WRAP_ITK_DOC_GENERATE_DOXY2SWIG_INPUT ON)
    endif(doc_path AND "${class}" MATCHES "^itk::")
  endif("${WRAPPER_WRAP_METHOD}" STREQUAL "ENUM")

endmacro(WRAP_NAMED_CLASS_DOC)

macro(ADD_ONE_TYPEDEF_DOC wrap_method wrap_class swig_name template_params)
  if(WRAP_ITK_DOC_GENERATE_DOXY2SWIG_INPUT)
    set(WRAP_ITK_DOC_DOXY2SWIG_INPUT "${WRAP_ITK_DOC_DOXY2SWIG_INPUT}\t${swig_name}")
  endif(WRAP_ITK_DOC_GENERATE_DOXY2SWIG_INPUT)
endmacro(ADD_ONE_TYPEDEF_DOC)

###############################################################################
macro(END_WRAP_LIBRARY_DOC)
    # create the target doc dir
    set(library_doc_build_dir "${CMAKE_CURRENT_BINARY_DIR}/Doc") # Library documentation interface files building directory
                                                                 # TODO: direct name of the library dir?
    file(MAKE_DIRECTORY ${library_doc_build_dir})

    # configure doxygen input file.
    # be sure to not include a header several times
    UNIQUE(headers "${WRAP_ITK_DOC_DOXYGEN_HEADERS}")
    set(library_doxygen_config_file ${library_doc_build_dir}/doxygen.config)
    set(WRAP_ITK_DOC_DOXYGEN_HEADERS_FORMATED)
    foreach(header ${headers})
      set(WRAP_ITK_DOC_DOXYGEN_HEADERS_FORMATED "${WRAP_ITK_DOC_DOXYGEN_HEADERS_FORMATED}           \"${header}\"\\\n")
    endforeach(header)
    set(WRAP_ITK_DOC_GENERATE_MAN "NO")
    if(WRAP_ITK_DOC_MAN)
      set(WRAP_ITK_DOC_GENERATE_MAN "YES")
    endif(WRAP_ITK_DOC_MAN)
    set(WRAP_ITK_DOC_LIBRARY_DIR "${library_doc_build_dir}")
    configure_file("${WRAP_ITK_DOC_SOURCE_DIR}/doxygen.config.in"
      "${library_doxygen_config_file}"
      @ONLY IMMEDIATE)

    # which files are produced?
    set(outputs ${WRAP_ITK_DOC_DOXYGEN_XML_FILES})
    if(WRAP_ITK_DOC_MAN)
      set(outputs ${outputs} ${WRAP_ITK_DOC_DOXYGEN_PAGES})
    endif(WRAP_ITK_DOC_MAN)

    # run doxygen
    add_custom_command(
      OUTPUT ${outputs} "${library_doc_build_dir}/xml/combine.xslt"  # this file is always produced and avoid an error if ${outputs} is empty
      COMMAND "${DOXYGEN_EXECUTABLE}" "${library_doxygen_config_file}"
#      WORKING_DIRECTORY ${WRAP_ITK_DOC_BINARY_DIR}
      DEPENDS ${WRAP_ITK_DOC_DOXYGEN_HEADERS} "${library_doxygen_config_file}"
      COMMENT "-- Wrapping library ${WRAPPER_LIBRARY_NAME}: Constructing documentation xml structure."
    )

    add_custom_target(${WRAPPER_LIBRARY_NAME}Doxygen ALL DEPENDS ${outputs} ${WRAP_ITK_DOC_DOCSTRING_FILES})

endmacro(END_WRAP_LIBRARY_DOC)

macro(WRAP_MODULE_DOC module)
  set(WRAP_ITK_DOC_DOXY2SWIG_INPUT )  # the c++ name - swig names definitions
endmacro(WRAP_MODULE_DOC)

###############################################################################
# This macro is called once per module
# Global variable WRAPPER_MODULE_NAME can be used
# in the macro to current module name
#
macro(END_WRAP_MODULE_DOC)
    set(doxy2swig_config_file ${CMAKE_CURRENT_BINARY_DIR}/Doc/${WRAPPER_MODULE_NAME}.conf)
    configure_file("${WRAP_ITK_DOC_SOURCE_DIR}/itk_doxy2swig.conf.in"
      "${doxy2swig_config_file}"
      @ONLY IMMEDIATE)

    # run itk_doxy2swig
    set(itk_doxy2swig_py "${WRAP_ITK_DOC_SOURCE_DIR}/itk_doxy2swig.py")
    set(swig_doc_interface_file ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_MODULE_NAME}_doc.i)
    add_custom_command(
    OUTPUT ${swig_doc_interface_file}
    COMMAND ${PYTHON_EXECUTABLE} ${itk_doxy2swig_py} ${doxy2swig_config_file} ${swig_doc_interface_file}
    DEPENDS ${WRAP_ITK_DOC_DOXYGEN_XML_FILES} ${doxy2swig_config_file} ${itk_doxy2swig_py}
#    COMMENT "-- Wrapping library ${WRAPPER_MODULE_NAME}: Generating swig interface for inline documentation."
    )
    set(WRAP_ITK_DOC_DOCSTRING_FILES ${WRAP_ITK_DOC_DOCSTRING_FILES} ${swig_doc_interface_file})

    # install the produced file
    WRAP_ITK_INSTALL(/Configuration/Typedefs "${swig_doc_interface_file}")
endmacro(END_WRAP_MODULE_DOC)

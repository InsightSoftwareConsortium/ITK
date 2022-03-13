find_package(Doxygen REQUIRED OPTIONAL_COMPONENTS dot)

###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_DOC_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "doc source dir")
set(ITK_WRAP_DOC_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "doc binary dir")

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


###############################################################################
macro(itk_end_wrap_module_DOC)
    # be sure to not include a header several times
    if(NOT "${ITK_WRAP_DOC_DOXYGEN_HEADERS}" STREQUAL "")
      list(REMOVE_DUPLICATES ITK_WRAP_DOC_DOXYGEN_HEADERS)
    endif()

    # which files are produced?
    set(outputs ${ITK_WRAP_DOC_DOXYGEN_XML_FILES})

    # run doxygen

    # create the target doc dir
    set(ITK_WRAP_DOC_LIBRARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/Doc") # Library documentation interface files building directory
    # TODO: direct name of the library dir?
    file(MAKE_DIRECTORY ${ITK_WRAP_DOC_LIBRARY_DIR})

    # The DoxygenConfig.cmake is a replacement for configuring a doxygen.config.in file
    # https://cmake.org/cmake/help/v3.16/module/FindDoxygen.html
    include(${WRAP_ITK_CMAKE_DIR}/DoxygenConfig.cmake)
    doxygen_add_docs(${WRAPPER_LIBRARY_NAME}Doxygen
            ${ITK_WRAP_DOC_DOXYGEN_HEADERS}
            ALL
            WORKING_DIRECTORY ${ITK_WRAP_DOC_LIBRARY_DIR}
            USE_STAMP_FILE
            COMMENT "-- Wrapping library ${WRAPPER_LIBRARY_NAME}: Constructing documentation xml structure.")
    add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Doxygen)
    unset(ITK_WRAP_DOC_LIBRARY_DIR)
endmacro()


###############################################################################
# This macro is called once per module
#
macro(itk_end_wrap_submodule_DOC module_name)
    set(doxy2swig_config_file ${CMAKE_CURRENT_BINARY_DIR}/Doc/${module_name}.conf)
    configure_file("${ITK_WRAP_DOC_SOURCE_DIR}/itk_doxy2swig.conf.in"
      "${doxy2swig_config_file}"
      @ONLY)
    # run itk_doxy2swig
    set(itk_doxy2swig_py "${ITK_WRAP_DOC_SOURCE_DIR}/itk_doxy2swig.py")
    set(swig_doc_interface_file ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${module_name}_doc.i)
    add_custom_command(
      OUTPUT ${swig_doc_interface_file}
      COMMAND ${Python3_EXECUTABLE} ${itk_doxy2swig_py} ${doxy2swig_config_file} ${swig_doc_interface_file}
      #DEPENDS ${ITK_WRAP_DOC_DOXYGEN_XML_FILES} ${doxy2swig_config_file} ${itk_doxy2swig_py}
      DEPENDS ${WRAPPER_LIBRARY_NAME}Doxygen ${doxy2swig_config_file} ${itk_doxy2swig_py}
  #    COMMENT "-- Wrapping library ${module_name}: Generating swig interface for inline documentation."
    )
    list(APPEND ITK_WRAP_DOC_DOCSTRING_FILES ${swig_doc_interface_file})

    unset(swig_doc_interface_file)
    unset(itk_doxy2swig_py)
    unset(doxy2swig_config_file)
endmacro()

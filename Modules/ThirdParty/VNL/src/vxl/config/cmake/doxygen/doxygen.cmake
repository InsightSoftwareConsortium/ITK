option(BUILD_DOCUMENTATION
  "Build doxygen-based code documentation." OFF)

# Dummy stubs to avoid BUILD_DOCUMENTATION ocnditionals around calls.
function(doxygen_add_book)
endfunction()
function(doxygen_add_package)
endfunction()
function(doxygen_add_library)
endfunction()

if(BUILD_DOCUMENTATION)
  #-------------------------------------------------------------------
  # helper functions
  #-------------------------------------------------------------------
  # doxygen_add_book(<package> <description>)
  #
  # - package     : package or site (e.g., core, contrib/mul, etc.)
  # - description : comment describing the package
  #
  # Example usage:
  #
  #   doxygen_add_package(contrib/mul
  #     "Manchester University Libraries overview documentation"
  #     )
  function(doxygen_add_book _book _description)
    if(TEXI2HTML_EXECUTABLE)
      string(REPLACE / _ bookname ${_book})
      file(APPEND "${CMAKE_BINARY_DIR}/doxygen_configuration.cmake"
        "set(DOXYGEN_BOOK_LIST \${DOXYGEN_BOOK_LIST} ${_book})\n"
        "set(DOXYGEN_${bookname}_BOOK_DESCRIPTION\n"
        "    \"${_description}\")\n"
        )
    endif()
  endfunction()

  # doxygen_add_package(<package> <description>)
  #
  # - package     : package or site (e.g., core, contrib/mul, etc.)
  # - description : comment describing the package
  #
  # Example usage:
  #
  #   doxygen_add_package(contrib/mul
  #     "Manchester University Libraries"
  #     )
  function(doxygen_add_package _package _description)
    string(REPLACE / _ packname ${_package})
    file(APPEND "${CMAKE_BINARY_DIR}/doxygen_configuration.cmake"
      "set(DOXYGEN_PACKAGE_LIST \${DOXYGEN_PACKAGE_LIST} ${_package})\n"
      "set(DOXYGEN_${packname}_DESCRIPTION \"${_description}\")\n"
      )
  endfunction()

  # doxygen_add_library(<lib>
  #                     [DEPENDS <dep1> <dep2> ...]
  #                     [PACKAGE <package>]
  #                     [DESCRIPTION <description>]
  #                     )
  #
  # - lib         : name of library
  # - package     : package to which it belongs
  # - depN        : dependencies of this lib
  # - description : comment describing the library
  #
  # Example usage:
  #
  #   doxygen_add_library(contrib/gel/mrc/vpgl
  #     DEPENDS core/vcsl core/vgl core/vnl core/vbl
  #     PACKAGE contrib/gel
  #     DESCRIPTION "Photogrammetry Library"
  #     )
  function(doxygen_add_library _library)
    # parse arguments
    set(DEPENDS)
    set(PACKAGE)
    set(DESCRIPTION)
    set(active_option)
    set(options_list PACKAGE DEPENDS DESCRIPTION)
    foreach(arg ${ARGN})
      list(FIND options_list ${arg} result)
      if(result EQUAL -1)
        set(${active_option} ${${active_option}} ${arg})
        if(NOT "${active_option}" STREQUAL "DEPENDS")
          set(active_option)
        endif()
      else()
        set(active_option ${arg})
      endif()
    endforeach()
    if(NOT PACKAGE)
      set(PACKAGE other)
    endif()

    # start work
    set(library ${_library})

    string(REPLACE / _ libname ${library})
    string(REPLACE / _ packname ${PACKAGE})

    get_filename_component(prefix ${library} NAME)
    set(prefix ${prefix}_)

    # NOTE: tagfiles will be resolved in doxygen_makeall.cmake
    set(tagfiles @tagfiles@)
    configure_file(
      "${DOXYGEN_SCRIPT_DIR}/doxyfile.in"
      "${CMAKE_BINARY_DIR}/doxy/output/doxyfile.${libname}"
      @ONLY
      )

    file(APPEND "${CMAKE_BINARY_DIR}/doxygen_configuration.cmake"
      "set(DOXYGEN_LIBRARY_LIST\n"
      "  \${DOXYGEN_LIBRARY_LIST} ${library})\n"
      "set(DOXYGEN_${libname}_DEPS \"${DEPENDS}\")\n"
      "set(DOXYGEN_${libname}_DESCRIPTION \"${DESCRIPTION}\")\n"
      "set(DOXYGEN_${packname}_LIBRARY_LIST\n"
      "  \${DOXYGEN_${packname}_LIBRARY_LIST} ${library})\n"
      )
  endfunction()

  #-------------------------------------------------------------------
  # find packages needed
  #-------------------------------------------------------------------
  find_package(Doxygen REQUIRED)
  find_package(Perl REQUIRED)

  find_program(TEXI2HTML_EXECUTABLE texi2html)
  if(NOT TEXI2HTML_EXECUTABLE)
    message(WARNING "Texi2html not found; no books will be built.")
  endif()

  find_package(Subversion QUIET)
  find_program(PSTOPNM_EXECUTABLE pstopnm)
  find_program(PNMTOPNG_EXECUTABLE pnmtopng)
  if(PSTOPNM_EXECUTABLE AND PNMTOPNG_EXECUTABLE)
    set(NetPBM_FOUND TRUE)
  else()
    find_package(ImageMagick QUIET COMPONENTS convert)
  endif()

  #-------------------------------------------------------------------
  #
  #-------------------------------------------------------------------
  # FIXME: Should others be cached?: DOXYGEN_STYLESHEET.
  set(DOXYGEN_OUTPUT_DIR "${CMAKE_BINARY_DIR}/doxy"
    CACHE PATH "Path to your doxygen output."
    )
  set(DOXYGEN_INDEX_FILE index.html
    CACHE STRING "Name of your multi-lib global index."
    )
  set(DOXYGEN_MERGE_DOCS_WITH ""
    CACHE STRING "Merge documentation to existing index."
    )

  get_filename_component(DOXYGEN_SCRIPT_DIR
    "${CMAKE_CURRENT_LIST_FILE}" PATH
    )
  set(DOXYGEN_SOURCE_DIR   "${VXL_ROOT_SOURCE_DIR}")
  set(DOXYGEN_INPUT_FILTER "${DOXYGEN_SCRIPT_DIR}/vxl_doxy.pl")
  set(DOXYGEN_STYLESHEET) # FIXME: This is not really used so far...
  if(DOXYGEN_DOT_FOUND)
    option(DOXYGEN_USE_GRAPHVIZ
      "Use graphviz to generate class diagrams" ON)
    if(DOXYGEN_USE_GRAPHVIZ)
      set(DOXYGEN_USE_DOT YES)
    else()
      set(DOXYGEN_USE_DOT NO)
    endif()
  endif()

  # make configuration loadable when running build_doxygen_doc target
  file(WRITE "${CMAKE_BINARY_DIR}/doxygen_configuration.cmake"
    "# Doxygen configuration variables for:\n"
    "#   ${DOXYGEN_SCRIPT_DIR}/doxygen_makeall.cmake\n"
    "# *** This is a auto-generated file. DO NOT edit! ***\n"
    "\n"
    "set(Subversion_FOUND ${Subversion_FOUND})\n"
    "set(Subversion_SVN_EXECUTABLE ${Subversion_SVN_EXECUTABLE})\n"
    "\n"
    "set(PERL_EXECUTABLE ${PERL_EXECUTABLE})\n"
    "set(TEXI2HTML_EXECUTABLE ${TEXI2HTML_EXECUTABLE})\n"
    "set(NetPBM_FOUND ${NetPBM_FOUND})\n"
    "set(PSTOPNM_EXECUTABLE ${PSTOPNM_EXECUTABLE})\n"
    "set(PNMTOPNG_EXECUTABLE ${PNMTOPNG_EXECUTABLE})\n"
    "set(ImageMagick_FOUND ${ImageMagick_FOUND})\n"
    "set(ImageMagick_convert_EXECUTABLE ${ImageMagick_convert_EXECUTABLE})\n"
    "\n"
    "set(DOXYGEN_EXECUTABLE ${DOXYGEN_EXECUTABLE})\n"
    "set(DOXYGEN_OUTPUT_DIR ${DOXYGEN_OUTPUT_DIR})\n"
    "set(DOXYGEN_INDEX_FILE ${DOXYGEN_INDEX_FILE})\n"
    "set(DOXYGEN_MERGE_DOCS_WITH ${DOXYGEN_MERGE_DOCS_WITH})\n"
    "set(DOXYGEN_SCRIPT_DIR ${DOXYGEN_SCRIPT_DIR})\n"
    "set(DOXYGEN_SOURCE_DIR ${DOXYGEN_SOURCE_DIR})\n"
    "set(DOXYGEN_LIBRARY_LIST)\n"
    "set(DOXYGEN_BOOK_LIST)\n"
    "\n"
    #"set( ${})\n"
    )

  # prepare header for use by doxygen
  set(title "\$title")
  configure_file(
    "${DOXYGEN_SCRIPT_DIR}/doxy_header.html"
    "${CMAKE_BINARY_DIR}/doxy/output/doxy_header.html"
    )

  #-------------------------------------------------------------------
  # Add doxygen target
  # Pivot on version 2.6.3 - newer CMake's support "SOURCES" option on
  # the "add_custom_target" command.
  #
  # Using CMAKE_VERSION in the following test is more to the point,
  # but it is not supported in V2.6.3 and earlier versions of CMAKE.
  #
  # The CMAKE_MINIMUM_REQUIRED_VERSION can differ from what is set in the top
  # level CMake file when VXL is imported into other projects that use CMake.
  # When building VXL as a stand-alone library, the CMAKE_MINIMUM_REQUIRED_VERSION
  # value is set from the top level CMakeList.txt file. When the VXL
  # export config is used by other projects,
  # the CMAKE_MINIMUM_REQUIRED_VERSION is set from the top level cmake file
  # for that project, where the CMAKE_MINIMUM_REQUIRED_VERSION may be greater than
  # V2.6.3.  If it is greater, then we can use the enhanced version of add_custom_target()
  # that supports the SOURCES ketword.
  #-------------------------------------------------------------------
  set( doxygen_sources )
  if(CMAKE_MINIMUM_REQUIRED_VERSION GREATER 2.6.3)
    # this file does not exist in clean build
    if( NOT EXISTS ${CMAKE_BINARY_DIR}/doxygen_last_build_rev.cmake )
       file(WRITE "${CMAKE_BINARY_DIR}/doxygen_last_build_rev.cmake"
         "# *** This is a auto-generated file. DO NOT edit! ***\n\n" )
    endif()

    set( doxygen_sources
      SOURCES
        "${DOXYGEN_SCRIPT_DIR}/doxygen.cmake"
        "${DOXYGEN_SCRIPT_DIR}/doxygen_makeall.cmake"
        "${DOXYGEN_SCRIPT_DIR}/doxyfile.in"
        "${DOXYGEN_SCRIPT_DIR}/doxy_header.html"
        "${DOXYGEN_SCRIPT_DIR}/vxl_doxy.pl"
        "${CMAKE_BINARY_DIR}/doxygen_configuration.cmake"
        "${CMAKE_BINARY_DIR}/doxygen_last_build_rev.cmake"
        "${CMAKE_BINARY_DIR}/doxygen_last_build_rev.cmake"
     )
  endif()

  add_custom_target(build_doxygen_doc
    ${CMAKE_COMMAND} -P "${DOXYGEN_SCRIPT_DIR}/doxygen_makeall.cmake"
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
    COMMENT "Build Doxygen Documentation"
    VERBATIM
    ${doxygen_sources}
    )

endif()

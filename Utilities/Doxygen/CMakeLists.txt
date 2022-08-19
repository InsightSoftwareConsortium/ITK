option(ITK_BUILD_DOCUMENTATION "Build the documentation (Doxygen)." OFF)
mark_as_advanced(ITK_BUILD_DOCUMENTATION)
if(ITK_BUILD_DOCUMENTATION)
  find_package(UnixCommands)
  find_package(Doxygen)
  find_package(Gnuplot)
  find_package(HTMLHelp)
  find_package(Perl)
  find_package(Wget)
endif()


# -------------------------------------------------------------
#
# Generate Doxygen configuration file in all configuration file
#
if(${DOXYGEN_DOT_FOUND})
  set(ITK_HAVE_DOT "YES")
else()
  set(ITK_HAVE_DOT "NO")
endif()

set(DOX_MODULE_LIST ${ITK_MODULES_ENABLED})

foreach(itk-module ${DOX_MODULE_LIST})
  if(${itk-module}_IS_TEST)
    list(APPEND DOXYGEN_TEST_DIRS "${${itk-module}_SOURCE_DIR}")
  else()
    if(EXISTS ${${itk-module}_SOURCE_DIR}/include)
      list(APPEND DOXYGEN_INCLUDE_DIRS "${${itk-module}_SOURCE_DIR}/include")
    endif()
  endif()
endforeach()

list(APPEND DOXYGEN_TEST_DIRS "${ITK_SOURCE_DIR}/Examples")
list(APPEND DOXYGEN_TEST_DIRS "${ITK_SOURCE_DIR}")
list(APPEND DOXYGEN_TEST_DIRS "${ITK_SOURCE_DIR}/Modules/Remote")


#
# Configure the script and the doxyfile, then add target
#
set(ITK_DOXYGEN_OUTPUT_DIR ${ITK_BINARY_DIR}/Utilities/Doxygen)

set(ITK_DOXYGEN_DIAGRAMS YES)

cmake_dependent_option(ITK_DOXYGEN_HTML "Doxygen will generate HTML Output" ON "ITK_BUILD_DOCUMENTATION" OFF)
cmake_dependent_option(ITK_DOXYGEN_DOCSET "Doxygen will generate additional index files for Xcode 3" OFF "ITK_BUILD_DOCUMENTATION" OFF)
cmake_dependent_option(ITK_DOXYGEN_CHM "Doxygen will generate Microsoft HTML help" OFF "ITK_BUILD_DOCUMENTATION" OFF)
cmake_dependent_option(ITK_DOXYGEN_QHP "Doxygen will generate Qt Compressed Help" OFF "ITK_BUILD_DOCUMENTATION" OFF)
cmake_dependent_option(ITK_DOXYGEN_ECLIPSEHELP "Doxygen will generate Eclipse help plugin" OFF "ITK_BUILD_DOCUMENTATION" OFF)
cmake_dependent_option(ITK_DOXYGEN_LATEX "Doxygen will generate LaTeX output" OFF "ITK_BUILD_DOCUMENTATION" OFF)
cmake_dependent_option(ITK_DOXYGEN_RTF "Doxygen will generate RTF output optimized for Word 97" OFF "ITK_BUILD_DOCUMENTATION" OFF)
cmake_dependent_option(ITK_DOXYGEN_XML "Doxygen will generate XML output" OFF "ITK_BUILD_DOCUMENTATION" OFF)

cmake_dependent_option(ITK_DOXYGEN_SERVER_BASED_SEARCH "Doxygen HTML will use PHP server based search." ON ITK_DOXYGEN_HTML OFF)

mark_as_advanced(ITK_DOXYGEN_HTML
  ITK_DOXYGEN_DOCSET
  ITK_DOXYGEN_CHM
  ITK_DOXYGEN_QHP
  ITK_DOXYGEN_ECLIPSEHELP
  ITK_DOXYGEN_LATEX
  ITK_DOXYGEN_RTF
  ITK_DOXYGEN_XML
  ITK_DOXYGEN_SERVER_BASED_SEARCH
  )

find_package(Perl)

if(PERL_FOUND)
  set(ITK_DOXYGEN_INPUT_FILTER
    "${PERL_EXECUTABLE} ${ITK_BINARY_DIR}/Utilities/Doxygen/itkdoxygen.pl")

  configure_file(${ITK_SOURCE_DIR}/Utilities/Doxygen/itkdoxygen.pl.in
               ${ITK_BINARY_DIR}/Utilities/Doxygen/itkdoxygen.pl)
else()
  set(ITK_DOXYGEN_INPUT_FILTER)
endif()

# -------------------------------------------------------------
if (ITK_BUILD_DOCUMENTATION)


  # When updating Doxygen versions, also re-run workbox-sw to regenerate the
  # service worker pre-cache. Do this by running:
  #
  #  npm install -g workbox-cli
  #  cd ITK-Doxygen-build
  #  make Documentation
  #  cd Utilities/Doxygen
  #  workbox inject:manifest
  #  cp ./html/serviceWorker.js ~/src/ITK/Utilities/Doxygen/
  #
  # If workbox has changed version, we also need to update workbox-sw.prod*.js
  # referenced below
  file(COPY workbox-cli-config.js serviceWorker.js.in
    DESTINATION ${ITK_BINARY_DIR}/Utilities/Doxygen/)

  execute_process(COMMAND ${Python3_EXECUTABLE} "${CMAKE_CURRENT_LIST_DIR}/datetime.py"
     RESULT_VARIABLE CMD_RESULT
     OUTPUT_VARIABLE _DATETIME)

   if (CMD_RESULT)
     message(FATAL_ERROR "Datetime failed!")
   endif()

   configure_file(${CMAKE_CURRENT_LIST_DIR}/build_text.js.in
     "${ITK_BINARY_DIR}/Utilities/Doxygen//html/build_text.js")

  find_package(LATEX)

  # Custom command to generate a examples page which include all ITK examples
  add_custom_command(OUTPUT "${ITK_BINARY_DIR}/Documentation/Doxygen/Examples.dox"
    COMMAND ${CMAKE_COMMAND} -D "PROJECT_SOURCE_DIR:PATH=${ITK_SOURCE_DIR}"
    -D "OUTPUT_FILE:PATH=${ITK_BINARY_DIR}/Documentation/Doxygen/Examples.dox"
    -P "${ITK_SOURCE_DIR}/Utilities/Doxygen/GenerateExamplesDox.cmake"
    WORKING_DIRECTORY "${ITK_SOURCE_DIR}/Examples"
    DEPENDS "${ITK_SOURCE_DIR}/Examples" "${ITK_SOURCE_DIR}/Utilities/Doxygen/GenerateExamplesDox.cmake"
    )

  set(ITK_DOXYGEN_INPUT ${DOXYGEN_INCLUDE_DIRS}
    ${ITK_SOURCE_DIR}/Documentation/Doxygen
    ${ITK_BINARY_DIR}/Documentation/Doxygen
    ${ITK_BINARY_DIR}/Documentation/Doxygen/Examples.dox
    ${ITK_BINARY_DIR}/Utilities/Doxygen/Modules
    ${ITK_DOXYGEN_INPUT}
    )
  list(REMOVE_DUPLICATES ITK_DOXYGEN_INPUT)

  # The DoxygenConfig.cmake is a replacement for configuring a doxygen.config.in file
  # https://cmake.org/cmake/help/v3.16/module/FindDoxygen.html
  include(${CMAKE_CURRENT_LIST_DIR}/DoxygenConfig.cmake)
  doxygen_add_docs(Documentation
          ${ITK_DOXYGEN_INPUT}
          # -- Only build if 'Documentation' target is requested ALL
          WORKING_DIRECTORY ${ITK_BINARY_DIR}/Utilities/Doxygen
          # -- Some of the ITK_DOXYGEN_INPUT are directories, USE_STAMP_FILE requires only files be listed
          COMMENT "-- Building ITK Doxygen."
      )
  unset(ITK_DOXYGEN_INPUT)

        #  add_custom_target(Documentation ALL
        #    COMMAND ${DOXYGEN_EXECUTABLE} ${ITK_BINARY_DIR}/Utilities/Doxygen/doxygen.config
        #    MAIN_DEPENDENCY ${ITK_BINARY_DIR}/Utilities/Doxygen/doxygen.config
        #    DEPENDS         ${ITK_BINARY_DIR}/Utilities/Doxygen/itkdoxygen.pl
        #    DEPENDS         ${ITK_BINARY_DIR}/Documentation/Doxygen/Examples.dox
        #    WORKING_DIRECTORY ${ITK_BINARY_DIR}/Utilities/Doxygen
        #    )

  # Somehow the logo image is not copied into the html folder.
  # Here we ensure that the logo image ends up in the right directory
  # where html pages could find it.
  add_custom_command(TARGET Documentation
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
      ${ITK_SOURCE_DIR}/Documentation/Art/itkLogoSmall.png # logo
      ${ITK_DOXYGEN_OUTPUT_DIR}/html # output doxygen directory
    )

  message(STATUS
           "To generate Doxygen's documentation, you need to build the Documentation target"
         )

endif ()

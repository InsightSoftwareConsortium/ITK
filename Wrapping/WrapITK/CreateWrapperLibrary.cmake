# Macro definitions for creating cableswig input libraries from cable-formatted
# input cxx files.
# Convention: Variable names in ALL_CAPS are global, and are shared between macros
# lower-case variable names indicate that that variable is to be considered local.

macro(END_WRAP_LIBRARY)
  WRAPPER_LIBRARY_CREATE_LIBRARY()
endmacro(END_WRAP_LIBRARY)

macro(WRAPPER_LIBRARY_CREATE_LIBRARY)

  # STEP 1
  # Make lists of all of the files that are to be generated

  # Add the generated module wrappers. These files are not included in the general
  # WRAPPER_LIBRARY_CABLESWIG_INPUTS list because they are specific for each language.
  set(wrap_perl_sources "${WRAPPER_LIBRARY_OUTPUT_DIR}/wrap_${WRAPPER_LIBRARY_NAME}PerlPerl.cxx")
  set(wrap_tcl_sources "${WRAPPER_LIBRARY_OUTPUT_DIR}/wrap_${WRAPPER_LIBRARY_NAME}TclTcl.cxx")
  set(wrap_python_sources "${WRAPPER_LIBRARY_OUTPUT_DIR}/wrap_${WRAPPER_LIBRARY_NAME}PythonPython.cxx")
  set(wrap_java_sources "${WRAPPER_LIBRARY_OUTPUT_DIR}/wrap_${WRAPPER_LIBRARY_NAME}JavaJava.cxx")

  # Loop over cable config files creating three lists:
  # wrap_xxx_sources: list of generated files for each language
  # [install_]index_file_content: list of idx files which will be generated, to create
  # the master index file from.
  # library_idx_files: differently-formatted list of idx files
  set(index_file_content "%JavaLoader=InsightToolkit.itkbase.LoadLibrary(\"${WRAPPER_LIBRARY_NAME}Java\")\n")
  set(install_index_file_content "%JavaLoader=InsightToolkit.itkbase.LoadLibrary(\"${WRAPPER_LIBRARY_NAME}Java\")\n")
  set(library_idx_files)
  foreach(source ${WRAPPER_LIBRARY_CABLESWIG_INPUTS})
    get_filename_component(base_name ${source} NAME_WE)
    set(wrap_perl_sources ${wrap_perl_sources} "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}Perl.cxx")
    set(wrap_tcl_sources ${wrap_tcl_sources} "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}Tcl.cxx")
    set(wrap_python_sources ${wrap_python_sources} "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}Python.cxx")
    set(wrap_java_sources ${wrap_java_sources} "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}Java.cxx")
    # add each source's name to a java dependencies list for later use
    string(REGEX REPLACE wrap_ "" JAVA_DEP ${base_name})
    set(${WRAPPER_LIBRARY_NAME}_java_depends_init ${${WRAPPER_LIBRARY_NAME}_java_depends_init} ${JAVA_DEP})
    set(library_idx_files ${library_idx_files} "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}.idx" )
    set(index_file_content "${index_file_content}${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}.idx\n")
    if(EXTERNAL_WRAP_ITK_PROJECT)
      set(install_index_file_content "${install_index_file_content}${WRAP_ITK_INSTALL_LOCATION}/ClassIndex/${base_name}.idx\n")
    else(EXTERNAL_WRAP_ITK_PROJECT)
      set(install_index_file_content "${install_index_file_content}${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/ClassIndex/${base_name}.idx\n")
    endif(EXTERNAL_WRAP_ITK_PROJECT)
  endforeach(source)

  # Loop over the extra swig input files and add them to the generated files
  # lists. Guess that the generated cxx output will have the same name as
  # the .i input file.
  foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})
    get_filename_component(base_name ${source} NAME_WE)
    set(wrap_perl_sources ${wrap_perl_sources} "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}Perl.cxx")
    set(wrap_tcl_sources ${wrap_tcl_sources} "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}Tcl.cxx")
    set(wrap_python_sources ${wrap_python_sources} "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}Python.cxx")
    set(wrap_java_sources ${wrap_java_sources} "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}Java.cxx")
  endforeach(source)

  set(${WRAPPER_LIBRARY_NAME}_JAVA_DEPENDS  "${${WRAPPER_LIBRARY_NAME}_java_depends_init}" CACHE INTERNAL "" FORCE)

  # Mark each of the generated sources as being generated, so CMake knows not to
  # expect them to already exist.
  set_source_files_properties(
    ${wrap_perl_sources}
    ${wrap_tcl_sources}
    ${wrap_python_sources}
    ${wrap_java_sources}
    GENERATED )

  # STEP 2
  # Configure the master index file and SWIG include file, and provide an install
  # version and install rule for the former.

  set(library_master_index_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.mdx")
  set(gccxml_inc_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/gcc_xml.inc")

  set(CONFIG_INDEX_FILE_CONTENT ${index_file_content})
  configure_file("${WRAP_ITK_CONFIG_DIR}/Master.mdx.in" "${library_master_index_file}"
     @ONLY IMMEDIATE )

  set(CONFIG_INDEX_FILE_CONTENT ${install_index_file_content})
  configure_file( "${WRAP_ITK_CONFIG_DIR}/Master.mdx.in"
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/InstallOnly/${WRAPPER_LIBRARY_NAME}.mdx"
    @ONLY IMMEDIATE )
  WRAP_ITK_INSTALL("/ClassIndex" "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/InstallOnly/${WRAPPER_LIBRARY_NAME}.mdx")

  set(CONFIG_GCCXML_INC_CONTENTS)
  get_directory_property(include_dir_list INCLUDE_DIRECTORIES)
  foreach(dir ${include_dir_list})
    set(CONFIG_GCCXML_INC_CONTENTS "${CONFIG_GCCXML_INC_CONTENTS}-I${dir}\n")
  endforeach(dir)
  configure_file("${WRAP_ITK_CONFIG_DIR}/gcc_xml.inc.in" "${gccxml_inc_file}"
    @ONLY IMMEDIATE)

  # STEP 3
  # Create a list of needed master index (mdx) files and SWIG library files
  # from the WRAPPER_LIBRARY_DEPENDS information.
  # In both cases, check first in WRAP_ITK_MASTER_INDEX_DIRECTORY or
  # WRAP_ITK_SWIG_LIBRARY_DIRECTORY (respectively), which is where these files are
  # installed. These directories may not exist if WrapITK hasn't been installed,
  # so some care is required.
  # If installed versions aren't found, assume that the files exist
  # in WRAPPER_MASTER_INDEX_OUTPUT_DIR or WRAPPER_SWIG_LIBRARY_OUTPUT_DIR
  # (respectively), where they would be put by the current build process.
  # If the files don't even exist in the output dirs by the time the library
  # is being built, then we've got a problem.

  # First set the index and swig lib file lists to their proper initial state.
  set(master_index_files )
  set(swig_library_files ${WRAPPER_SWIG_LIBRARY_FILES})

  # Now churn through the mdx file requirements. Remember we add the current
  # library's mdx file to the list of index files.
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(no_mdx_found 1)

    if(WRAP_ITK_MASTER_INDEX_DIRECTORY) # WRAP_ITK_MASTER_INDEX_DIRECTORY may not be set
      set(wrapitk_mdx "${WRAP_ITK_MASTER_INDEX_DIRECTORY}/${dep}.mdx")
      if(EXISTS "${wrapitk_mdx}")
        set(master_index_files ${master_index_files} "${wrapitk_mdx}")
        set(no_mdx_found 0)
      endif(EXISTS "${wrapitk_mdx}")
    endif(WRAP_ITK_MASTER_INDEX_DIRECTORY)

    if(no_mdx_found)
      set(local_mdx "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${dep}.mdx")
      set(master_index_files ${master_index_files} "${local_mdx}")
    endif(no_mdx_found)
  endforeach(dep)

  # Always use the local version of the mdx file for the current library. The
  # installed version is (by definition) out of date.
  set(master_index_files ${master_index_files} "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.mdx")

  # Now the required .swg files. Remember we add itk.swg and the current library's
  # swg file to the WRAPPER_SWIG_LIBRARY_FILES list.
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS} "itk" "${WRAPPER_LIBRARY_NAME}")
    set(no_swg_found 1)

    if(WRAP_ITK_SWIG_LIBRARY_DIRECTORY) # WRAP_ITK_SWIG_LIBRARY_DIR may not be set
      set(wrapitk_swg "${WRAP_ITK_SWIG_LIBRARY_DIRECTORY}/${dep}.swg")
      if(EXISTS "${wrapitk_swg}")
        set(swig_library_files ${swig_library_files} "${wrapitk_swg}")
        set(no_swg_found 0)
      endif(EXISTS "${wrapitk_swg}")
    endif(WRAP_ITK_SWIG_LIBRARY_DIRECTORY)

    if(no_swg_found)
      set(local_swg "${WRAPPER_SWIG_LIBRARY_OUTPUT_DIR}/${dep}.swg")
      set(swig_library_files ${swig_library_files} "${local_swg}")
    endif(no_swg_found)
  endforeach(dep)

  # STEP 4
  # Generate the XML, index, and CXX files from the Cable input files, and add
  # the wrapper library.
  if(WRAP_ITK_PERL)
    set(library_type "SHARED")
    set(custom_library_prefix "")
    CREATE_WRAPPER_FILES_AND_LIBRARY("Perl" "pl" "${wrap_perl_sources}"
      "${master_index_files}" "${library_idx_files}" "${gccxml_inc_file}"
      "${swig_library_files}" "${library_type}" "${custom_library_prefix}")
  endif(WRAP_ITK_PERL)

  if(WRAP_ITK_TCL AND WRAPPER_LIBRARY_TCL)
    set(library_type "SHARED")
    set(custom_library_prefix "")
    # no .tcl files are created by SWIG, so pass an empty extension for the
    # "created file type" variable, so we know not to try to install the tcl files
    # that aren't made.
    CREATE_WRAPPER_FILES_AND_LIBRARY("Tcl" "" "${wrap_tcl_sources}"
      "${master_index_files}" "${library_idx_files}" "${gccxml_inc_file}"
      "${swig_library_files}" "${library_type}" "${custom_library_prefix}")
  endif(WRAP_ITK_TCL AND WRAPPER_LIBRARY_TCL)

  if(WRAP_ITK_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    set(library_type "MODULE")
    set(custom_library_prefix "_")
    CREATE_WRAPPER_FILES_AND_LIBRARY("Python" "py" "${wrap_python_sources}"
      "${master_index_files}" "${library_idx_files}" "${gccxml_inc_file}"
      "${swig_library_files}" "${library_type}" "${custom_library_prefix}")
  endif(WRAP_ITK_PYTHON AND WRAPPER_LIBRARY_PYTHON)

  if(WRAP_ITK_JAVA AND WRAPPER_LIBRARY_JAVA)
    # .java files are placed in InsightToolkit.jar - there is no need to install
    # so pass an empty extension for the "created file type" variable, so we know
    # not to try to install the .java files
    set(library_type "SHARED")
    set(custom_library_prefix "")
    CREATE_WRAPPER_FILES_AND_LIBRARY("Java" "" "${wrap_java_sources}"
      "${master_index_files}" "${library_idx_files}" "${gccxml_inc_file}"
      "${swig_library_files}" "${library_type}" "${custom_library_prefix}")
  endif(WRAP_ITK_JAVA AND WRAPPER_LIBRARY_JAVA)

  # Create the sub directories for each CMAKE_CONFIGURATION_TYPES
  # The generation of the XML, index and CXX files from the Cable input files
  # will use these sub directories to store some alternative files. But if
  # the directory doesn't exist, the generation fails !
  if(CMAKE_CONFIGURATION_TYPES)
    foreach(config ${CMAKE_CONFIGURATION_TYPES})
      file(MAKE_DIRECTORY "${LIBRARY_OUTPUT_PATH}/${config}")
    endforeach(config)
  endif(CMAKE_CONFIGURATION_TYPES)

  # STEP 5
  # Call a macro from CreateLanguageSupport.cmake
  # to create needed support files for wrapped languages.
  LANGUAGE_SUPPORT_CONFIGURE_FILES()
endmacro(WRAPPER_LIBRARY_CREATE_LIBRARY)


macro(CREATE_WRAPPER_FILES_AND_LIBRARY language extension library_sources
      master_index_files library_idx_files gccxml_inc_file
      swig_library_files library_type custom_library_prefix)

  set(library_name "${custom_library_prefix}${WRAPPER_LIBRARY_NAME}${language}")
  set(cable_files "${WRAPPER_LIBRARY_OUTPUT_DIR}/wrap_${WRAPPER_LIBRARY_NAME}${language}.cxx"
    ${WRAPPER_LIBRARY_CABLESWIG_INPUTS})
  # We add the library first so that there is a target to hang the file-creation dependencies on.
  CREATE_WRAPPER_LIBRARY("${library_name}" "${library_sources}" "${language}" "${library_type}" "${custom_library_prefix}")
  CREATE_WRAPPER_FILES("${library_name}" "${language}" "${extension}" "${master_index_files}" "${library_idx_files}"
    "${cable_files}" "${gccxml_inc_file}" "${swig_library_files}")
endmacro(CREATE_WRAPPER_FILES_AND_LIBRARY)


macro(CREATE_WRAPPER_FILES library_name language extension mdx_files library_idx_files
    cable_input_files gccxml_inc_file swig_library_files)

  if("${language}" STREQUAL "Java")
    get_filename_component(outdir ${WrapITK_BINARY_DIR}/Java/InsightToolkit ABSOLUTE)
  else("${language}" STREQUAL "Java")
    get_filename_component(outdir ${LIBRARY_OUTPUT_PATH}/${WRAP_ITK_BUILD_INTDIR} ABSOLUTE)
  endif("${language}" STREQUAL "Java")


  foreach(cable_file ${cable_input_files})
    get_filename_component(base_name "${cable_file}" NAME_WE)
    set(xml_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}.xml")
    set(idx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}.idx")
    set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}${language}.cxx")

    # Create the XML file
    GCCXML_CREATE_XML_FILE("${library_name}" "${cable_file}" "${xml_file}" "${gccxml_inc_file}")

    # Create the idx file and provide an install rule
    CINDEX_CREATE_IDX_FILE("${library_name}" "${xml_file}" "${idx_file}")
    WRAP_ITK_INSTALL("/ClassIndex" "${idx_file}")

    # Create the wrapper CXX file with cswig and an install rule for the generated language file
    CSWIG_CREATE_CXX_FILE("${library_name}" "${language}" "${idx_file}" "${xml_file}" "${cxx_file}"
      "${master_index_files}" "${library_idx_files}" "${swig_library_files}" "${outdir}")
    string(REGEX REPLACE "wrap_" "" simple_base_name "${base_name}")
    if(NOT "${extension}" STREQUAL "")
      set(swig_language_file "${LIBRARY_OUTPUT_PATH}/${WRAP_ITK_INSTALL_INTDIR}${simple_base_name}.${extension}")
      WRAP_ITK_INSTALL("/lib" "${swig_language_file}")
    endif(NOT "${extension}" STREQUAL "")
  endforeach(cable_file)

  # Create any extra CXX files from raw swig .i input files specified, and provide
  # install rules for the generated language output. Guess that the generated language
  # output will have the same name as the .i input file.
  foreach(swig_input ${WRAPPER_LIBRARY_SWIG_INPUTS})
    get_filename_component(base_name ${swig_input} NAME_WE)
    set(cxx_output "${WRAPPER_LIBRARY_OUTPUT_DIR}/${base_name}${language}.cxx")
    CREATE_EXTRA_SWIG_FILE("${library_name}" "${language}" "${swig_input}" "${cxx_output}" "${outdir}")
    if(NOT "${extension}" STREQUAL "")
      set(swig_language_file "${LIBRARY_OUTPUT_PATH}/${WRAP_ITK_INSTALL_INTDIR}${base_name}.${extension}")
      WRAP_ITK_INSTALL("/lib" "${swig_language_file}")
    endif(NOT "${extension}" STREQUAL "")
  endforeach(swig_input)

endmacro(CREATE_WRAPPER_FILES)


macro(GCCXML_CREATE_XML_FILE library_name input_cxx output_xml gccxml_inc_file)
# First, deal with dependencies. Specifically, this XML file will need to be
# re-generated when files that it included (and so on recursively) change.
# The cswig program, run later, will write out a 'depends' file based on this
# XML file that specifies what these dependencies are. Thus, when 'cmake' is
# run a second time, if any of those files has changed then the XML file will
# be re-generated. There is an additional wrinkle, however: if the dependencies
# have changed enough that one of the files doesn't exist any more, specifying
# a dependence on that now-defunct file will cause a make error (because make
# won't know how to generate that file). So, if certain dependencies are gone,
# then the XML file will need to be regenerated too.

  set(CABLE_SWIG_DEPEND)
  set(regenerate_xml)

  if(${CMAKE_MAKE_PROGRAM} MATCHES "make")
    # If the make program is not an IDE then include the depend file in a way
    # that will make cmake re-run if it changes
    if(EXISTS "${output_xml}.depend")
    else(EXISTS "${output_xml}.depend")
      configure_file(
       "${WRAP_ITK_CONFIG_DIR}/empty.depend.in"
       "${output_xml}.depend" COPYONLY IMMEDIATE)
    endif(EXISTS "${output_xml}.depend")
    include("${output_xml}.depend")
  else(${CMAKE_MAKE_PROGRAM} MATCHES "make")
    # for IDE generators like MS dev only include the depend files
    # if they exist. This is to prevent excessive reloading of
    # workspaces after each build. This also means
    # that the depends will not be correct until cmake
    # is run once after the build has completed once, and the depend files have
    # been created by cswig.
    include("${output_xml}.depend" OPTIONAL)
  endif(${CMAKE_MAKE_PROGRAM} MATCHES "make")

  if(CABLE_SWIG_DEPEND)
    # There are dependencies.  Make sure all the files are present.
    # If not, regenerate the XML file.
    foreach(f ${CABLE_SWIG_DEPEND})
      if(EXISTS ${f})
      else(EXISTS ${f})
        set(regenerate_xml 1)
      endif(EXISTS ${f})
    endforeach(f)
  else(CABLE_SWIG_DEPEND)
    # No dependencies found. This means that either the XML hasn't been generated
    # yet, or the dependency file itself has disappeared. In case of the latter,
    # (simultaneous with a change in the dependent files themselves, we need to
    # re-generate the XML.
    set(regenerate_xml 1)
  endif(CABLE_SWIG_DEPEND)

  if(regenerate_xml)
    # Force the XML file to be (re)made by making it depend on its dependency
    # file, which we then create again so that it is newer than the current XML
    # file (if the latter even exists). This forces the XML-creation rule to run.
    set(CABLE_SWIG_DEPEND "${output_xml}.depend")
    configure_file("${WRAP_ITK_CONFIG_DIR}/empty.depend.in"
      "${output_xml}.depend" COPYONLY IMMEDIATE)
  endif(regenerate_xml)


  # Finally, the dependencies are taken care of. Add the XML-generation rule!
  add_custom_command(
    SOURCE ${input_cxx}
    COMMAND ${GCCXML}
    ARGS -fxml-start=_cable_
         -fxml=${output_xml}
         --gccxml-gcc-options ${gccxml_inc_file}
         -DCSWIG
         -DCABLE_CONFIGURATION
         ${input_cxx}
    TARGET ${library_name}
    OUTPUTS ${output_xml}
    DEPENDS ${GCCXML} ${CABLE_SWIG_DEPEND})
endmacro(GCCXML_CREATE_XML_FILE)


macro(CINDEX_CREATE_IDX_FILE library_name input_xml output_idx)
   add_custom_command(
     SOURCE ${input_xml}
     COMMAND ${CABLE_INDEX}
     ARGS ${input_xml} ${output_idx}
     TARGET ${library_name}
     OUTPUTS ${output_idx}
     DEPENDS ${CABLE_INDEX})
endmacro(CINDEX_CREATE_IDX_FILE)


# Set the language-specific cswig args to be used by CSWIG_CREATE_CXX_FILE
set(CSWIG_ARGS_Tcl
  "-I${CSWIG_DEFAULT_LIB}/tcl"
  -tcl
  -pkgversion "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}.${ITK_VERSION_PATCH}")
set(CSWIG_ARGS_Perl
  "-I${CSWIG_DEFAULT_LIB}/perl5"
  -perl5)
set(CSWIG_ARGS_Python
  "-I${CSWIG_DEFAULT_LIB}/python"
  -python)
set(CSWIG_ARGS_Java
  "-I${CSWIG_DEFAULT_LIB}/java"
  -java
  -package InsightToolkit)
set(CSWIG_NO_EXCEPTION_REGEX_Python "ContinuousIndex\\.xml$")

macro(CSWIG_CREATE_CXX_FILE library_name language input_idx input_xml output_cxx
  master_index_files library_idx_files swig_library_files outdir)
   set(cindex)
   foreach(mdx ${master_index_files})
     set(cindex ${cindex} -Cindex "${mdx}")
   endforeach(mdx)

   set(swig_libs)
   foreach(file ${swig_library_files})
       set(swig_libs "-l${file}" ${swig_libs})
   endforeach(file)

  # Some files shouldn't have swig-exception handling turned on. Currently they're
  # identified by regular expressions. If we find one, we set a CSWIG flag
  # to define the NO_EXCEPTIONS symbol, which we use in itk.swg for conditional
  # compilation of the exception handling.
  # TODO: Is any of this NO_EXCEPTIONS stuff really necessary?
  set(no_exception_regex "${language}\\.xml$")
  set(extra_args)
  set(lang_no_exception_regex "${CSWIG_NO_EXCEPTION_REGEX_${language}}")
  if("${lang_no_exception_regex}")
      set(no_exception_regex "(${no_exception_regex})|(${lang_no_exception_regex})")
  endif("${lang_no_exception_regex}")
  if("${input_xml}" MATCHES "${no_exception_regex}")
     set(extra_args "-DNO_EXCEPTIONS")
  endif("${input_xml}" MATCHES "${no_exception_regex}")

  # we have to get rid of the trailing /, because on windows, cswig will append
  # \filename, creating 'path/\filename', which it can't deal with. Without
  # the trailing /, things work fine for some reason.
#   get_filename_component(outdir ${LIBRARY_OUTPUT_PATH}/${WRAP_ITK_BUILD_INTDIR} ABSOLUTE)

  add_custom_command(
   SOURCE ${input_idx}
   COMMAND ${CSWIG}
   ARGS ${swig_libs}
        -I${CSWIG_DEFAULT_LIB}
        ${CSWIG_IGNORE_WARNINGS}
        -noruntime
        -fcompact
        ${cindex}
        -depend ${input_xml}.depend
        -outdir ${outdir}
        -o ${output_cxx}
        -c++
        ${CSWIG_ARGS_${language}}
        ${extra_args}
        ${input_xml}
   TARGET ${library_name}
   OUTPUTS ${output_cxx}
   DEPENDS ${library_idx_files} ${master_index_files} ${swig_library_files} ${input_xml} ${CSWIG})
endmacro(CSWIG_CREATE_CXX_FILE)


macro(CREATE_EXTRA_SWIG_FILE library_name language swig_input cxx_output outdir)
  # we have to get rid of the trailing /, because on windows, cswig will append
  # \filename, creating 'path/\filename', which it can't deal with. Without
  # the trailing /, things work fine for some reason.
#   get_filename_component(outdir ${LIBRARY_OUTPUT_PATH}/${WRAP_ITK_BUILD_INTDIR} ABSOLUTE)

  add_custom_command(
    COMMENT "run native swig on ${swig_input}"
    SOURCE ${swig_input}
    COMMAND ${CSWIG}
    ARGS  -nocable
          -noruntime
          ${CSWIG_IGNORE_WARNINGS}
          -outdir ${outdir}
          -o ${cxx_output}
          -c++
          ${CSWIG_ARGS_${language}}
          ${swig_input}
    TARGET ${library_name}
    OUTPUTS ${cxx_output}
    DEPENDS ${CSWIG})
endmacro(CREATE_EXTRA_SWIG_FILE)


# Set the language-specific link libraries to be used by CREATE_WRAPPER_LIBRARY
# TODO: Are there really no Java link libs required? The ITK wrappers specify
# that ${JAVA_LIBRARY} be linked in, but that variable is never defined!
set(LINK_LIBRARIES_Tcl ${TCL_LIBRARY})
set(LINK_LIBRARIES_Perl ${PERL_LIBRARY})
set(LINK_LIBRARIES_Python ${PYTHON_LIBRARY})
set(LINK_LIBRARIES_Java )

macro(CREATE_WRAPPER_LIBRARY library_name sources language library_type custom_library_prefix)
  if(COMMAND cmake_policy)
    cmake_policy(SET CMP0003 NEW)
  endif(COMMAND cmake_policy)

  add_library(${library_name} ${library_type}
    ${sources} ${WRAPPER_LIBRARY_CXX_SOURCES})

  if(EXTERNAL_WRAP_ITK_PROJECT)
    # Don't add dependencies on modules created by WrapITK
    remove(dep_list "${WRAPPER_LIBRARY_DEPENDS}" "${WRAP_ITK_MODULES}")
  else(EXTERNAL_WRAP_ITK_PROJECT)
    set(dep_list ${WRAPPER_LIBRARY_DEPENDS})
  endif(EXTERNAL_WRAP_ITK_PROJECT)
  foreach(dep ${dep_list})
    add_dependencies(${library_name} ${custom_library_prefix}${dep}${language})
  endforeach(dep)

  if(custom_library_prefix)
    set_target_properties(${library_name} PROPERTIES PREFIX "")
  endif(custom_library_prefix)

  set(SWIG_RUNTIME_LANGUAGE SwigRuntime${language} )
  if("${language}" STREQUAL "Java" AND APPLE)
    set_target_properties(${library_name} PROPERTIES SUFFIX .jnilib)
    set(SWIG_RUNTIME_LANGUAGE "")
  endif("${language}" STREQUAL "Java" AND APPLE)

  if("${language}" STREQUAL "Python" AND WIN32)
    # message("Setting file extension to .pyd for python modules ${library_name}")
    set_target_properties(${library_name} PROPERTIES SUFFIX .pyd)
  endif("${language}" STREQUAL "Python" AND WIN32)

  if(CMAKE_CXX_COMPILER MATCHES "icpc")
    # disable warning #191: type qualifier is meaningless on cast type
    set_target_properties(${library_name} PROPERTIES COMPILE_FLAGS -wd191 )
  endif(CMAKE_CXX_COMPILER MATCHES "icpc")

  set_target_properties(${library_name} PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")
  target_link_libraries(${library_name}
    ${WRAPPER_LIBRARY_LINK_LIBRARIES}
    ${SWIG_RUNTIME_LANGUAGE}
    ${LINK_LIBRARIES_${language}} )

  get_target_property(library_location ${library_name} LOCATION)
  # Yet another horrible hack. get_target_property() returns a path
  # with the value of ${CMAKE_CFG_INTDIR} in it (with Xcode on mac os,
  # $(CONFIGURATION) ), but what we want is \${BUILD_TYPE} for the
  # install, so we have to replace "${CMAKE_CFG_INTDIR}" by
  # "\${BUILD_TYPE}" (backslash is not so important, as ${BUILD_TYPE}
  # is expanded to ${BUILD_TYPE})
  # Because it can't be simple (otherwise it's not fun), we have to generate
  # a regular expression with the content of ${CMAKE_CFG_INTDIR},
  # but this content can contain some special characters for regular expression
  # like parenthesis. The most logical approach would be to backslash
  # all the characters, so we are sure to match the exact string,
  # but  string(REGEX REPLACE "(.)" "\\\\1" ...) replace all the
  # char by \1. The [] are used to woraround that, but will fail if some
  # [ or ] are in ${CMAKE_CFG_INTDIR}
  # On linux (without configuration), "${CMAKE_CFG_INTDIR}" is ".", so
  # we must avoid the REPLACE step on that case, or all the "." are
  # removed from the path

  if(CMAKE_CONFIGURATION_TYPES)
    string(REGEX REPLACE "(.)" "[\\1]" escaped_regexp "${CMAKE_CFG_INTDIR}")
    string(REGEX REPLACE "${escaped_regexp}" "\${BUILD_TYPE}" clean_library_location "${library_location}")
  else(CMAKE_CONFIGURATION_TYPES)
    set(clean_library_location "${library_location}")
  endif(CMAKE_CONFIGURATION_TYPES)

  WRAP_ITK_INSTALL("/lib" ${clean_library_location})

endmacro(CREATE_WRAPPER_LIBRARY)


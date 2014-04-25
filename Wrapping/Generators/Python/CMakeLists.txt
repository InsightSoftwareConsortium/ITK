###############################################################################
# configure python (find PythonInterp first, as of cmake 3.1)
find_package(PythonInterp)
find_package(PythonLibs)

# require at least python 2.6
if(PYTHON_VERSION_STRING VERSION_LESS 2.6)
  message(WARNING "Python version less than 2.6: \"${PYTHON_VERSION_STRING}\".")
endif()

# check for version mismatch.
if(PYTHONLIBS_FOUND AND PYTHONINTERP_FOUND
    AND NOT(PYTHON_VERSION_STRING VERSION_EQUAL PYTHONLIBS_VERSION_STRING))
  message(WARNING "Python executable (\"${PYTHON_VERSION_STRING}\") and library (\"${PYTHONLIBS_VERSION_STRING}\") version mismatch.")
endif()

mark_as_advanced(PYTHON_EXECUTABLE)
include_directories(${PYTHON_INCLUDE_PATH})


###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_PYTHON_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "python source dir")
set(ITK_WRAP_PYTHON_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "python binary dir")


###############################################################################
# create the python directory in the classindex dir
file(MAKE_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python)

###############################################################################
# python 2.6 search for the modules in a "64" subdirectory, and don't provide
# any obvious way to find that. Create a link 64 -> . to make it find its
# files.
# TODO: file a bug report at opensolaris
if(CMAKE_SYSTEM MATCHES "SunOS-." AND NOT EXISTS "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/64")
  file(MAKE_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
  execute_process(COMMAND ln -sf . "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/64")
endif()


###############################################################################
# Configure the path-dependent itkConfig.py

# we specify these directories with relative paths  so that the file can be
# bundled up into an install conventiently. Python will take care of turning
# the / path separator into \ on windows if needed.

if(NOT EXTERNAL_WRAP_ITK_PROJECT)

  if(CMAKE_CONFIGURATION_TYPES)
    set(CONFIG_WRAP_ITK_DATA_ROOT "../../Data")
    set(CONFIG_WRAP_ITK_TEST_ROOT "../../Testing")
    set(CONFIG_PYTHON_CONFIGPY_DIR "../Configuration")

    foreach(config ${CMAKE_CONFIGURATION_TYPES})
      # SWIG-generated libs and *.py files are sent to ${config} subdir
      # This assumes that CMAKE_LIBRARY_OUTPUT_DIRECTORY is WrapITK_BINARY_DIR/bin (bad!)
      # TODO: We need a better way to do this.
      set(CONFIG_PYTHON_SWIGPY_DIR "../../lib/${config}")
      set(CONFIG_PYTHON_SWIGLIB_DIR "../../lib/${config}")
      configure_file("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
                     "${ITK_WRAP_PYTHON_BINARY_DIR}/${config}/itkConfig.py"
                     @ONLY)
    endforeach()
  else()
    set(CONFIG_WRAP_ITK_DATA_ROOT "../Data")
    set(CONFIG_WRAP_ITK_TEST_ROOT "../Testing")
    set(CONFIG_PYTHON_CONFIGPY_DIR "Configuration")

    set(CONFIG_PYTHON_SWIGPY_DIR "../lib")
    set(CONFIG_PYTHON_SWIGLIB_DIR "../lib")
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
                   "${ITK_WRAP_PYTHON_BINARY_DIR}/itkConfig.py"
                   @ONLY)
  endif()

  set(CONFIG_WRAP_ITK_TEST_ROOT "None")
  set(CONFIG_PYTHON_SWIGLIB_DIR "../lib")
  set(CONFIG_PYTHON_SWIGPY_DIR "../lib")
  set(CONFIG_PYTHON_CONFIGPY_DIR "Configuration")
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
                 "${ITK_WRAP_PYTHON_BINARY_DIR}/InstallOnly/itkConfig.py"
                @ONLY)

  WRAP_ITK_BINDINGS_INSTALL(/Python "${ITK_WRAP_PYTHON_BINARY_DIR}/InstallOnly/itkConfig.py")

  # copy the files to expose build options in python
  set(ITK_WRAP_PYTHON_VECTOR_REAL )
  foreach(t ${WRAP_ITK_VECTOR_REAL})
    foreach(d ${ITK_WRAP_DIMS})
      set(ITK_WRAP_PYTHON_VECTOR_REAL ${ITK_WRAP_PYTHON_VECTOR_REAL} ${ITKT_${t}${d}})
    endforeach()
  endforeach()
  set(ITK_WRAP_PYTHON_COV_VECTOR_REAL )
  foreach(t ${WRAP_ITK_COV_VECTOR_REAL})
    foreach(d ${ITK_WRAP_DIMS})
      set(ITK_WRAP_PYTHON_COV_VECTOR_REAL ${ITK_WRAP_PYTHON_COV_VECTOR_REAL} ${ITKT_${t}${d}})
    endforeach()
  endforeach()
  set(ITK_WRAP_PYTHON_RGB )
  foreach(t ${WRAP_ITK_RGB})
    set(ITK_WRAP_PYTHON_RGB ${ITK_WRAP_PYTHON_RGB} ${ITKT_${t}})
  endforeach()
  set(ITK_WRAP_PYTHON_RGBA )
  foreach(t ${WRAP_ITK_RGBA})
    set(ITK_WRAP_PYTHON_RGBA ${ITK_WRAP_PYTHON_RGBA} ${ITKT_${t}})
  endforeach()
  set(ITK_WRAP_PYTHON_COMPLEX_REAL )
  foreach(t ${WRAP_ITK_COMPLEX_REAL})
    set(ITK_WRAP_PYTHON_COMPLEX_REAL ${ITK_WRAP_PYTHON_COMPLEX_REAL} ${ITKT_${t}})
  endforeach()
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITKBuildOptionsPython.py.in"
                 "${ITK_WRAP_PYTHON_BINARY_DIR}/WrapITKBuildOptionsPython.py"
                @ONLY)
  WRAP_ITK_BINDINGS_INSTALL(/Python "${ITK_WRAP_PYTHON_BINARY_DIR}/WrapITKBuildOptionsPython.py")

  file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/WrapITKBuildOptionsConfig.py"
       DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/Configuration/")
  WRAP_ITK_BINDINGS_INSTALL(/Python/Configuration "${CMAKE_CURRENT_SOURCE_DIR}/WrapITKBuildOptionsConfig.py")

endif()


###############################################################################
# Copy python files for out-of-source builds, and set up install of same.

if(NOT EXTERNAL_WRAP_ITK_PROJECT)

  # Create a list of Python files.
  # WrapITK/Python/*.py
  set(ITK_PYTHON_FILES
    itk
    itkBase
    itkTemplate
    itkTypes
    itkExtras
    itkLazy
    )
  # Done listing files.

  # Now copy these files if necessary.

  if("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
    # In source build -- no need to copy Python file. Do need to set up the install.
    foreach(file ${ITK_PYTHON_FILES})
      set(install_tgt "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
      set(ITK_WRAP_PYTHON_INSTALL_FILES ${ITK_WRAP_PYTHON_FILES} "${install_tgt}")
    endforeach()
  else()
    set(ITK_WRAP_PYTHON_FILES)
    foreach(file ${ITK_PYTHON_FILES})
      set(src "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
      # recall that WRAP_ITK_BUILD_INTDIR expands to nothing if no config types are set,
      # or to "CMAKE_CONFIG_INTDIR/" if there are such. Likewise, WRAP_ITK_INSTALL_INTDIR
      # expands to ${BUILD_TYPE}/ or nothing.
      set(copy_tgt "${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_BUILD_INTDIR}${file}.py")
      set(ITK_WRAP_PYTHON_FILES ${ITK_WRAP_PYTHON_FILES} "${copy_tgt}")
      set(install_tgt "${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_INSTALL_INTDIR}${file}.py")
      set(ITK_WRAP_PYTHON_INSTALL_FILES ${ITK_WRAP_PYTHON_INSTALL_FILES} "${install_tgt}")

      # create the directory to avoid loosing case on windows
      file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_INSTALL_INTDIR}${path})

      add_custom_command(OUTPUT ${copy_tgt}
        COMMAND ${CMAKE_COMMAND} -E copy ${src} ${copy_tgt}
        DEPENDS ${src}
        COMMENT "Copying ${file}.py to build dir.")
    endforeach()
    add_custom_target(copy_python_files ALL DEPENDS ${ITK_WRAP_PYTHON_FILES})

  endif()

  # Install the package python files.
  WRAP_ITK_BINDINGS_INSTALL(/Python ${ITK_WRAP_PYTHON_INSTALL_FILES})

endif()


###############################################################################
# Configure and install the custom python .pth files

if(CMAKE_CONFIGURATION_TYPES)

  foreach(config ${CMAKE_CONFIGURATION_TYPES})
    set(CONFIG_ITK_WRAP_PYTHON_DIR "${CMAKE_CURRENT_BINARY_DIR}/${config}")
    set(CONFIG_ITK_WRAP_LIBRARY_DIR "${ITK_BINARY_DIR}/${ITK_INSTALL_LIBRARY_DIR}/${config}")

    # SWIG-generated libs and *.py files are sent to ${config} subdir
    # This assumes that CMAKE_LIBRARY_OUTPUT_DIRECTORY is WrapITK_BINARY_DIR/bin (bad!)
    # TODO: We need a better way to do this.
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/${config}/WrapITK.pth"
                   @ONLY)
  endforeach()
else()
  set(CONFIG_ITK_WRAP_PYTHON_DIR "${CMAKE_CURRENT_BINARY_DIR}")
  set(CONFIG_ITK_WRAP_LIBRARY_DIR "${ITK_BINARY_DIR}/${ITK_INSTALL_LIBRARY_DIR}")

  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/WrapITK.pth"
                 @ONLY)

endif()

set(CONFIG_ITK_WRAP_PYTHON_DIR "${CMAKE_INSTALL_PREFIX}/lib/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Python")
set(CONFIG_ITK_WRAP_LIBRARY_DIR "${CMAKE_INSTALL_PREFIX}/${ITK_INSTALL_LIBRARY_DIR}")
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
            "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/WrapITK.pth"
            @ONLY)

if(PYTHON_EXECUTABLE)
  set(python_check "from __future__ import print_function\ntry:\n    import distutils.sysconfig\n    print(distutils.sysconfig.get_python_lib(1))\nexcept:\n    pass")
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/det_spp.py ${python_check})
  execute_process(COMMAND "${PYTHON_EXECUTABLE}" "${CMAKE_CURRENT_BINARY_DIR}/det_spp.py"
    OUTPUT_VARIABLE py_spp
    ERROR_VARIABLE py_spp
  )
endif()

string(REGEX REPLACE "\n" "" py_spp_no_newline "${py_spp}")
string(REGEX REPLACE "\\\\" "/" py_spp_nobackslashes "${py_spp_no_newline}")
set(PY_SITE_PACKAGES_PATH "${py_spp_nobackslashes}" CACHE PATH "Python site-packages directory to install a .pth file pointing at WrapITK Python modules.")
mark_as_advanced(PY_SITE_PACKAGES_PATH)
if(PY_SITE_PACKAGES_PATH)
  INSTALL_AT_ABSOLUTE_PATH(install_wrapitk_compatibility "${PY_SITE_PACKAGES_PATH}" "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/WrapITK.pth")
endif()

###############################################################################
# Install python stuff for external projects
if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  WRAP_ITK_INSTALL(/Configuration/Generators/Python
    CMakeLists.txt
    ExternalProjectConfig.pth.in
    ExternalProjectConfig.py.in
    ModuleConfig.py.in
    ModuleLoader.py.in
    WrapITK.pth.in
    itkConfig.py.in
    main_module_ext.i.in
    module_ext.i.in
  )
endif()

###############################################################################
# Define the wrapping macros

macro(itk_wrap_module_python library_name)
  set(ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES "")
  set(ITK_WRAP_PYTHON_LIBRARY_IMPORTS "")
  set(ITK_WRAP_PYTHON_LIBRARY_DEPS )
  set(ITK_WRAP_PYTHON_LIBRARY_DECLS )
  set(ITK_WRAP_PYTHON_LIBRARY_CALLS )
  set(ITK_WRAP_PYTHON_CXX_FILES )
endmacro()


macro(itk_setup_swig_python type base_name interface_file python_file cpp_file doc_file)

  #############################################################################
  # Runs swig to produce the .so and the .py file
  # Called by itk_end_wrap_module_python and by itk_end_wrap_submodule_python
  # Type will then be either "module" or "submodule"
  #############################################################################

  set(swig_command ${SWIG_EXECUTABLE})
  if(ITK_USE_CCACHE)
    set(swig_command ${CCACHE_EXECUTABLE} ${swig_command})
  endif()

  set(_swig_depend)
  if(NOT ITK_USE_SYSTEM_SWIG)
    # The ExternalProject SWIG install.
    set(_swig_depend swig)
  endif()

  set(dependencies
    "${DEPS}"
    "${interface_file}"
    "${WRAP_ITK_TYPEDEFS_DIRECTORY}/pyBase.i"
    "${_swig_depend}")

  if("${type}" STREQUAL "module")
    # Module
    set(dependencies ${dependencies}
      "${ITK_WRAP_PYTHON_LIBRARY_DEPS}"
      "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${WRAPPER_LIBRARY_NAME}_ext.i")
  else()
    # Submodule
    set(dependencies ${dependencies}
      "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${base_name}_ext.i"
      "${doc_file}")
  endif()

  add_custom_command(
    OUTPUT ${cpp_file} ${python_file}
    COMMAND ${swig_command} -c++ -python -O -features autodoc=1
    -Werror
    -w302 # Identifier 'name' redefined (ignored)
    -w303 # %extend defined for an undeclared class 'name' (to avoid warning about customization in pyBase.i)
    -w312 # Unnamed nested class not currently supported (ignored)
    -w314 # 'identifier' is a lang keyword
    -w361 # operator! ignored
    -w362 # operator= ignored
    -w350 # operator new ignored
    -w383 # operator++ ignored
    -w384 # operator-- ignored
    -w389 # operator[] ignored
    -w394 # operator new[] ignored
    -w395 # operator delete[] ignored
    -w467 # Overloaded declaration not supported (no type checking rule for 'type')
    -w508 # Declaration of 'name' shadows declaration accessible via operator->(), previous declaration of'declaration'
    -w509 # Overloaded method declaration effectively ignored, as it is shadowed by declaration
    -o ${cpp_file}
    -I${GENERATORS_SRC_DIR}
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}/python
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}
    ${WRAP_ITK_SWIG_ARGS_PYTHON}
    -outdir ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    ${interface_file}
    WORKING_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python
    DEPENDS ${dependencies}
  )
  WRAP_ITK_BINDINGS_INSTALL("/Python" "${python_file}")

endmacro()


macro(itk_end_wrap_module_python)

  # Loop over the extra swig input files and add them to the generated files
  # lists. Guess that the generated cxx output will have the same name as
  # the .i input file.
  set(ITK_WRAP_PYTHON_PROCCESS_SWIG_INPUTS ON)
  foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})
    get_filename_component(base_name ${source} NAME_WE)
    itk_wrap_submodule_python("${base_name}")
    itk_end_wrap_submodule_python("${base_name}")
  endforeach()
  set(ITK_WRAP_PYTHON_PROCCESS_SWIG_INPUTS OFF)

  # create the python config file
  # this file store all the name - type association and a dependencies list for the modules
  #
  # first build the dependency list
  set(ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS "")

  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS "'${dep}', ${ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS}")
    set(ITK_WRAP_PYTHON_LIBRARY_IMPORTS "import ${dep}Python\n${ITK_WRAP_PYTHON_LIBRARY_IMPORTS}")
  endforeach()

  # ITKPyBase is always included, excepted ITKPyBase itself
  if(NOT "${WRAPPER_LIBRARY_NAME}" STREQUAL "ITKPyBase")
    set(ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS "'ITKPyBase', ${ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS}")
    set(ITK_WRAP_PYTHON_LIBRARY_IMPORTS "import ITKPyBasePython\n${ITK_WRAP_PYTHON_LIBRARY_IMPORTS}")
  endif()

  # and create the file, with the var ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES and
  # ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS created earlier
  configure_file("${ITK_WRAP_PYTHON_SOURCE_DIR}/ModuleConfig.py.in"
    "${ITK_WRAP_PYTHON_BINARY_DIR}/Configuration/${WRAPPER_LIBRARY_NAME}Config.py"
    @ONLY)
  WRAP_ITK_BINDINGS_INSTALL("/Python/Configuration"
    "${ITK_WRAP_PYTHON_BINARY_DIR}/Configuration/${WRAPPER_LIBRARY_NAME}Config.py"
  )

  # create the advanced lib module python file
  # this file let the final user _not_ use the itk module, but rather
  # something like "import Base"
  set(CONFIG_LIBRARY_NAME "${WRAPPER_LIBRARY_NAME}")
  configure_file("${ITK_WRAP_PYTHON_SOURCE_DIR}/ModuleLoader.py.in"
    "${ITK_WRAP_PYTHON_BINARY_DIR}/${WRAPPER_LIBRARY_NAME}.py"
    @ONLY)
  WRAP_ITK_BINDINGS_INSTALL("/Python" "${ITK_WRAP_PYTHON_BINARY_DIR}/${WRAPPER_LIBRARY_NAME}.py")

  # create the python customization stuff in the main module
  # it allow to group the pythons module in a single shared lib, by loading the int
  # functions of the module. I also import the objects from the submodules in the
  # main module.
  #
  # It uses ITK_WRAP_PYTHON_LIBRARY_DECLS, ITK_WRAP_PYTHON_LIBRARY_CALLS and
  # ITK_WRAP_PYTHON_LIBRARY_IMPORTS
  configure_file("${ITK_WRAP_PYTHON_SOURCE_DIR}/main_module_ext.i.in"
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${WRAPPER_LIBRARY_NAME}_ext.i"
    @ONLY)
  WRAP_ITK_INSTALL("/Configuration/Typedefs/python"
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${WRAPPER_LIBRARY_NAME}_ext.i"
  )

  # set some var reused later
  set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.i")
  set(lib ${WRAPPER_LIBRARY_NAME}Python)
  set(python_file "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${lib}.py")
  set(cpp_file "${CMAKE_CURRENT_BINARY_DIR}/${WRAPPER_LIBRARY_NAME}Python.cpp")

  # if this is for an external library, let the user add extra swig args
  if(EXTERNAL_WRAP_ITK_PROJECT)
    set(WRAP_ITK_SWIG_ARGS_PYTHON "" CACHE STRING "Extra user-defined swig arguments to be to the swig executable.")
  endif()

  # Run swig to produce the .so and the .py file
  itk_setup_swig_python("module" ${base_name} ${interface_file} ${python_file} ${cpp_file} "")

  # build all the c++ files from this module in a common lib
  add_library(${lib} MODULE ${cpp_file} ${ITK_WRAP_PYTHON_CXX_FILES} ${WRAPPER_LIBRARY_CXX_SOURCES})
  set_target_properties(${lib} PROPERTIES PREFIX "_")
  # gcc 4.4 complains a lot without this flag when building in release mode
  if(CMAKE_COMPILER_IS_GNUCC)
    set_target_properties(${lib} PROPERTIES COMPILE_FLAGS "-fno-strict-aliasing -Wno-shadow")
  endif()
  # work around linkage problem on some solaris systems
  if(CMAKE_SYSTEM MATCHES "SunOS-." AND CMAKE_COMPILER_IS_GNUCXX AND CMAKE_COMPILER_IS_GNUCC)
    target_link_libraries(${lib} stdc++)
  endif()
  # extension is not the same on windows
  if(WIN32)
    set_target_properties(${lib} PROPERTIES SUFFIX .pyd)
  endif()
  target_link_libraries(${lib} ${WRAPPER_LIBRARY_LINK_LIBRARIES} ${PYTHON_LIBRARY})
  add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Swig)
  if(ITK_WRAP_DOC)
    add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Doxygen)
  endif()
  if(ITK_WRAP_EXPLICIT AND NOT ${WRAPPER_LIBRARY_NAME} STREQUAL ITKPyBase)
    target_link_libraries(${lib} ${WRAPPER_LIBRARY_NAME}Explicit)
    add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Explicit)
  endif()
  # 64 bit binaries are not installed in the same directories on solaris
  set(sun64 )
  if(CMAKE_SYSTEM MATCHES "SunOS.*" AND CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(sun64 "/64")
  endif()
  install(TARGETS "${lib}" DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Python${sun64}")
  if(NOT EXTERNAL_WRAP_ITK_PROJECT)
    # don't depends on the targets from wrapitk in external projects
    foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
      add_dependencies(${lib} ${dep}Swig)
      if(ITK_WRAP_DOC)
        add_dependencies(${lib} ${dep}Doxygen)
      endif()
    endforeach()
  endif()

endmacro()


macro(itk_end_wrap_submodule_python group_name)

  set(base_name ${group_name})

  if("${group_name}" STREQUAL "ITKQuadEdgeMeshBase")
    # add a template definition for the superclass which is not in ITK
    set(text )
    foreach(d ${ITK_WRAP_DIMS})
      set(text "${text}%template(mapULitkQuadEdgeMeshPointF${d}) std::map< unsigned long, itkQuadEdgeMeshPointF${d}, std::less< unsigned long > >;\n")
      ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "mapULitkQuadEdgeMeshPointF${d}" "unsigned long, itk::QuadEdgeMeshPoint< float, ${d} >")

      # set(text "${text}%template(itkMapContainerMD${d}QBAIUL_Superclass) std::map< itkMeshD${d}Q::BoundaryAssignmentIdentifier, unsigned long, std::less< itkMeshD${d}Q::BoundaryAssignmentIdentifier > >;\n")
      # ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "itkMapContainerMD${d}QBAIUL_Superclass" "itk::Mesh<double, ${d}u, itk::QuadEdgeMeshTraits<double, ${d}, bool, bool, float, float> >::BoundaryAssignmentIdentifier, unsigned long")

      set(text "${text}%traits_swigtype(itkCellInterfaceDQEMCTI${d});\n")
      set(text "${text}%fragment(SWIG_Traits_frag(itkCellInterfaceDQEMCTI${d}));\n")
      set(text "${text}%template(mapULitkCellInterfaceDQEMCTI${d}) std::map< unsigned long, itkCellInterfaceDQEMCTI${d} *, std::less< unsigned long > >;\n")
      ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "mapULitkCellInterfaceDQEMCTI${d}" "unsigned long, itk::CellInterface< double, itk::QuadEdgeMeshCellTraitsInfo< ${d} > >*")
    endforeach()

    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}${text}")
  endif()

  # is there a docstring file?
  if(ITK_WRAP_DOC AND NOT ITK_WRAP_PYTHON_PROCCESS_SWIG_INPUTS)
    # yes. Include the docstring file
    set(doc_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_MODULE_NAME}_doc.i")
    set(ITK_WRAP_PYTHON_SWIG_EXT "%include ${WRAPPER_MODULE_NAME}_doc.i\n\n${ITK_WRAP_PYTHON_SWIG_EXT}")
  else()
    # no. Clear the doc_file var
    set(doc_file "")
  endif()

  # the default typemaps, exception handler, and includes
  set(ITK_WRAP_PYTHON_SWIG_EXT "%import pyBase.i\n\n${ITK_WRAP_PYTHON_SWIG_EXT}")

  # create the swig interface for all the groups in the module
  set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${base_name}.i")
  set(lib ${group_name}Python)
  set(python_file "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${lib}.py")
  set(cpp_file "${CMAKE_CURRENT_BINARY_DIR}/${base_name}Python.cpp")

  # create the python customization for that wrap_*.cmake file.
  configure_file("${ITK_WRAP_PYTHON_SOURCE_DIR}/module_ext.i.in"
  "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${group_name}_ext.i"
  @ONLY)
  WRAP_ITK_INSTALL("/Configuration/Typedefs/python" "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${group_name}_ext.i"
  )

  # prepare dependencies
  set(DEPS )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(DEPS ${DEPS} ${${dep}SwigFiles})
  endforeach()

  # Run swig to produce the .so and the .py file
  itk_setup_swig_python("submodule" ${base_name} ${interface_file} ${python_file} ${cpp_file} "${doc_file}")

  # add the c++ files which will be generated by the swig command to the
  # list of python related c++ files, so they can be built at the end
  # of the current module.
  set(ITK_WRAP_PYTHON_CXX_FILES ${ITK_WRAP_PYTHON_CXX_FILES} ${cpp_file})

  # add needed files to the deps list
  set(ITK_WRAP_PYTHON_LIBRARY_DEPS ${ITK_WRAP_PYTHON_LIBRARY_DEPS} "${python_file}" "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${base_name}_ext.i" "${cpp_file}")

  # add this wrap_*.cmake stuff to the list of modules to init in the main module.
  # first the extern c declaration
  set(ITK_WRAP_PYTHON_LIBRARY_DECLS "${ITK_WRAP_PYTHON_LIBRARY_DECLS}extern \"C\" int init_${group_name}Python();\n")
  # and the call of the extern function
  set(ITK_WRAP_PYTHON_LIBRARY_CALLS "${ITK_WRAP_PYTHON_LIBRARY_CALLS}  init_${group_name}Python();\n")

endmacro()



macro(itk_wrap_one_type_python wrap_method wrap_class swig_name template_params)
  string(REGEX REPLACE "(.*::)" "" base_name "${wrap_class}")

  if(NOT "${wrap_class}" STREQUAL "MetaEvent" AND NOT "${wrap_method}" MATCHES "ENUM")
    ADD_PYTHON_CONFIG_TEMPLATE("${base_name}" "${wrap_class}" "${swig_name}" "${template_params}")
  endif()

  # std::complex and vcl_complex are the same classes, but python don't know that
  if("${wrap_class}" STREQUAL "vcl_complex")
    ADD_PYTHON_CONFIG_TEMPLATE("complex" "std::complex" "${swig_name}" "${template_params}")
  endif()

endmacro()


macro(ADD_PYTHON_CONFIG_TEMPLATE base_name wrap_class swig_name template_params)
  # build the name - type association list used in *Config.py

  if("${template_params}" STREQUAL "")
    set(ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES "${ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES}  ('${base_name}', '${wrap_class}', '${swig_name}'),\n")
  else()
    set(ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES "${ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES}  ('${base_name}', '${wrap_class}', '${swig_name}', '${template_params}'),\n")
  endif()

endmacro()


macro(itk_wrap_submodule_python module)
  set(ITK_WRAP_PYTHON_SWIG_EXT "")

  # register the module for the lib module
  set(ITK_WRAP_PYTHON_LIBRARY_IMPORTS "${ITK_WRAP_PYTHON_LIBRARY_IMPORTS}from ${module}Python import *\n")

endmacro()


macro(itk_wrap_named_class_python class swig_name)
  # store the current class wrapped, so we can generate the typemaps for itk::ImageSource
  set(ITK_WRAP_PYTHON_CURRENT_CLASS "${class}")
  set(ITK_WRAP_PYTHON_CURRENT_SWIG_NAME "${swig_name}")
endmacro()


macro(itk_wrap_template_python name types)
  if("${ITK_WRAP_PYTHON_CURRENT_CLASS}" STREQUAL "itk::ImageSource")
    # generate the typemap which let pass an ImageSource instead of an Image
    set(image_source "${ITK_WRAP_PYTHON_CURRENT_SWIG_NAME}${name}")
    set(image "${ITKN_${name}}")

    set(text "\n\n")
    set(text "${text}%typemap(in) ${image} * {\n")
#    set(text "${text}  // ======================\n")
    set(text "${text}  ${image_source} * imgsrc;\n")
    set(text "${text}  ${image} * img;\n")
    set(text "${text}  if( $input != Py_None && SWIG_ConvertPtr($input,(void **)(&imgsrc),\$descriptor(${image_source} *), 0) == 0 )\n")
    set(text "${text}    {\n")
    set(text "${text}    \$1 = imgsrc->GetOutput(0);\n")
    set(text "${text}    }\n")
    set(text "${text}  else if( SWIG_ConvertPtr($input,(void **)(&img),\$descriptor(${image} *), 0) == 0 )\n")
    set(text "${text}    {\n")
    set(text "${text}    \$1 = img;\n")
    set(text "${text}    }\n")
    set(text "${text}  else\n")
    set(text "${text}    {\n")
    set(text "${text}    PyErr_SetString(PyExc_TypeError, \"Expecting argument of type ${image} or ${image_source}.\");\n")
    set(text "${text}    SWIG_fail;\n")
    set(text "${text}    }\n")
    set(text "${text}}\n")
    set(text "${text}\n")
    set(text "${text}\n")
    set(text "${text}%typemap(typecheck) ${image} * {\n")
#    set(text "${text}  // //////////////////////////\n")
    set(text "${text}  ${image_source} * imgsrc;\n")
    set(text "${text}  ${image} * img;\n")
    set(text "${text}  if( $input != Py_None && SWIG_ConvertPtr($input,(void **)(&imgsrc),\$descriptor(${image_source} *), 0) == 0 )\n")
    set(text "${text}    {\n")
    set(text "${text}    \$1 = 1;\n")
    set(text "${text}    }\n")
    set(text "${text}  else if( SWIG_ConvertPtr($input,(void **)(&img),\$descriptor(${image} *), 0) == 0 )\n")
    set(text "${text}    {\n")
    set(text "${text}    \$1 = 1;\n")
    set(text "${text}    }\n")
    set(text "${text}  else\n")
    set(text "${text}    {\n")
    set(text "${text}    PyErr_Clear();\n")
    set(text "${text}    \$1 = 0;\n")
    set(text "${text}    }\n")
    set(text "${text}}\n")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}${text}")

  endif()
endmacro()


macro(itk_wrap_simple_type_python wrap_class swig_name)
  # split the class name and the template parameters
  if("${wrap_class}" MATCHES "<.*>")
    string(REGEX REPLACE "^([^<]+)< *(.+) *>([^>]*)$" "\\1" cpp_name "${wrap_class}")
    string(REGEX REPLACE "^([^<]+)< *(.+) *>([^>]*)$" "\\2" template_params "${wrap_class}")
    string(REGEX REPLACE "^([^<]+)< *(.+) *>([^>]*)$" "\\3" ext_def "${wrap_class}")
  else()
    set(cpp_name "${wrap_class}")
    set(template_params NO_TEMPLATE)
    set(ext_def "")
  endif()
  string(REGEX REPLACE ".*::" "" simple_name "${cpp_name}")

  # must be done first so the typemap are used in the %template commands
  if("${swig_name}" MATCHES "_Pointer$")
    string(REGEX REPLACE "_Pointer$" "" smart_pointed "${swig_name}")
    ADD_PYTHON_POINTER_TYPEMAP("${smart_pointed}")
  endif()


  # and now, generate the typemaps and other customizations

  if("${cpp_name}" STREQUAL "vcl_complex")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}DECL_PYTHON_VCL_COMPLEX_CLASS(${swig_name})\n")
  endif()

  if("${swig_name}" STREQUAL "itkLightObject")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(listitkLightObject) std::list< itkLightObject_Pointer >;\n\n")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "listitkLightObject" "itk::LightObject")
  endif()

  if("${swig_name}" STREQUAL "itkObject")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}DECL_PYTHON_OBJECT_CLASS(${swig_name})\n")
  endif()

  if("${swig_name}" STREQUAL "itkProcessObject")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}DECL_PYTHON_PROCESSOBJECT_CLASS(${swig_name})\n\n")
  endif()

  if("${swig_name}" STREQUAL "itkDataObject")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(vectoritkDataObject) std::vector< itkDataObject_Pointer >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkDataObject" "itk::DataObject")
  endif()

  if("${swig_name}" STREQUAL "itkObjectFactoryBase")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(listitkObjectFactoryBase) std::list< itkObjectFactoryBase * >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "listitkObjectFactoryBase" "itk::ObjectFactoryBase")
  endif()

  if("${swig_name}" STREQUAL "itkMetaDataDictionary")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(vectoritkMetaDataDictionary) std::vector< itkMetaDataDictionary * >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkMetaDataDictionary" "itk::MetaDataDictionary")
  endif()

  if("${swig_name}" STREQUAL "itkCommand")
    # make itk::Command hineritable in python
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%feature(\"director\") itkCommand;\n")
  endif()

  if("${cpp_name}" STREQUAL "itk::ImageBase" AND NOT "${swig_name}" MATCHES "Pointer$")
    # add the templated method non seen by gccxml, in a more python-friendly way
    # than the c++ version
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}DECL_PYTHON_IMAGEBASE_CLASS(${swig_name}, ${template_params})\n")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%inline %{\n")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}#include \"itkContinuousIndexSwigInterface.h\"\n")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%}\n")
  endif()

  if("${cpp_name}" STREQUAL "itk::StatisticsLabelObject" AND NOT "${swig_name}" MATCHES "Pointer$")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(map${swig_name}) std::map< unsigned long, ${swig_name}_Pointer, std::less< unsigned long > >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "map${swig_name}" "unsigned long, ${cpp_name}< ${template_params} >")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name}_Pointer >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
 endif()

  if("${cpp_name}" STREQUAL "itk::LabelMap" AND NOT "${swig_name}" MATCHES "Pointer$")
      set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}${ITK_WRAP_PYTHON_SWIG_EXT}DECL_PYTHON_LABELMAP_CLASS(${swig_name})\n")
  endif()

  if("${cpp_name}" STREQUAL "itk::ComponentTreeNode")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(list${swig_name}) std::list< ${swig_name}* >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "list${swig_name}" "${cpp_name}< ${template_params} > *")
  endif()

  if("${cpp_name}" STREQUAL "itk::ImageRegion")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}${ITK_WRAP_PYTHON_SWIG_EXT}DECL_PYTHON_IMAGEREGION_CLASS(${swig_name})\n")
  endif()


  if("${cpp_name}" STREQUAL "itk::Index")
    ADD_PYTHON_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Size")
    ADD_PYTHON_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::RGBPixel")
    # number of elements is not in the template parameters so use the
    # macro which get it with Size() instead
    ADD_PYTHON_VARIABLE_LENGTH_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::RGBAPixel")
    # number of elements is not in the template parameters so use the
    # macro which get it with Size() instead
    ADD_PYTHON_VARIABLE_LENGTH_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Offset")
    ADD_PYTHON_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::FixedArray")
    ADD_PYTHON_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Vector")
    ADD_PYTHON_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::CovariantVector")
    ADD_PYTHON_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Point")
    ADD_PYTHON_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::ContinuousIndex")
    ADD_PYTHON_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Array")
    ADD_PYTHON_VARIABLE_LENGTH_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${swig_name}" STREQUAL "itkTransformBase")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(list${swig_name}_Pointer) std::list< ${swig_name}_Pointer >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "list${swig_name}_Pointer" "itk::TransformBase")
  endif()

  if("${cpp_name}" STREQUAL "itk::SpatialObjectPoint")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}DECL_PYTHON_SPATIALOBJECTPPOINT_CLASS(${swig_name})%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif()

  if("${cpp_name}" STREQUAL "itk::ContourSpatialObjectPoint")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif()

  if("${cpp_name}" STREQUAL "itk::LineSpatialObjectPoint")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif()

  if("${cpp_name}" STREQUAL "itk::SurfaceSpatialObjectPoint")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif()

  if("${cpp_name}" STREQUAL "itk::SpatialObject" AND NOT "${ext_def}" MATCHES "Pointer")
    set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}%template(list${swig_name}_Pointer) std::list< ${swig_name}_Pointer >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "list${swig_name}_Pointer" "${cpp_name}< ${template_params} >")
  endif()

endmacro()


macro(ADD_PYTHON_SEQ_TYPEMAP swig_name dim)
  set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}DECL_PYTHON_SEQ_TYPEMAP(${swig_name}, ${dim})\n")
endmacro()


macro(ADD_PYTHON_VEC_TYPEMAP swig_name template_params)
  string(REGEX REPLACE "(.*),(.*)" "\\1" type "${template_params}")
  string(REGEX REPLACE "(.*),(.*)" "\\2" dim "${template_params}")
  set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}DECL_PYTHON_VEC_TYPEMAP(${swig_name}, ${type}, ${dim})\n")
endmacro()

macro(ADD_PYTHON_VARIABLE_LENGTH_SEQ_TYPEMAP type value_type)
  set(ITK_WRAP_PYTHON_SWIG_EXT "${ITK_WRAP_PYTHON_SWIG_EXT}DECL_PYTHON_VARLEN_SEQ_TYPEMAP(${type}, ${value_type})\n")
endmacro()


macro(ADD_PYTHON_POINTER_TYPEMAP template_params)
  set(text "DECLARE_REF_COUNT_CLASS(${template_params})\n")

# set(text "\n\n")
# set(text "${text} // Python typemaps for Smart Pointers to ${template_params} class. \n\n")
#        set(text "${text}// pointers and references\n")
#        set(text "${text}%typemap(out) ${template_params} *, ${template_params} & {\n")
#        set(text "${text}        // always tell SWIG_NewPointerObj we're the owner\n")
#        set(text "${text}        \$result = SWIG_NewPointerObj((void *) \$1, \$1_descriptor, 1);\n")
#        set(text "${text}        if (\$1) {\n")
#        set(text "${text}                \$1->Register();\n")
#        set(text "${text}        }\n")
#        set(text "${text}}\n")
# set(text "${text}\n")
# set(text "${text}// transform smart pointers in raw pointers\n")
#        set(text "${text}%typemap(out) ${template_params}_Pointer {\n")
#        set(text "${text}  // get the raw pointer from the smart pointer\n")
#        set(text "${text}  ${template_params} * ptr = \$1.GetPointer();\n")
#        set(text "${text}        // always tell SWIG_NewPointerObj we're the owner\n")
#        set(text "${text}        \$result = SWIG_NewPointerObj((void *) ptr, \$descriptor(${template_params} *), 1);\n")
#        set(text "${text}        // register the object, it it exists\n")
#        set(text "${text}        if (ptr) {\n")
#        set(text "${text}                ptr->Register();\n")
#        set(text "${text}        }\n")
#        set(text "${text}}\n")
# set(text "${text}\n")
#        set(text "${text}// make deletion in scripting language just decrement ref. count\n")
#        set(text "${text}%extend ${template_params} {\n")
#        set(text "${text}        public:\n")
#        set(text "${text}        ~${template_params}() {self->UnRegister();};\n")
#        set(text "${text}}\n")
# set(text "${text}\n")
#        set(text "${text}%ignore ${template_params}::~${template_params};\n")
# set(text "${text}\n")
# set(text "${text}%ignore ${template_params}_Pointer;\n")
# set(text "${text}\n\n")

  set(ITK_WRAP_PYTHON_SWIG_EXT "${text}${ITK_WRAP_PYTHON_SWIG_EXT}")
endmacro()


###############################################################################
# Create the PyUtils library

if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  add_subdirectory(Tests)

  macro(itk_end_wrap_modules_python)
    add_subdirectory(${ITK_WRAP_PYTHON_SOURCE_DIR}/PyUtils)
  endmacro()

  macro(itk_wrap_modules_python)
    add_subdirectory(${ITK_WRAP_PYTHON_SOURCE_DIR}/PyBase)
  endmacro()

else()
  macro(itk_end_wrap_modules_python)
    # just do nothing
  endmacro()

  macro(itk_wrap_modules_python)
    # just do nothing
  endmacro()

endif()

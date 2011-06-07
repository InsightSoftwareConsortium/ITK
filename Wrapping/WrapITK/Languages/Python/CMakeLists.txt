
###############################################################################
# configure python
find_package(PythonLibs)
find_package(PythonInterp)
mark_as_advanced(PYTHON_EXECUTABLE)
include_directories(${PYTHON_INCLUDE_PATH})


###############################################################################
# store the current dir, so it can be reused later
set(WRAP_ITK_PYTHON_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "python source dir")
set(WRAP_ITK_PYTHON_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "python binary dir")


###############################################################################
# create the python directory in the classindex dir
file(MAKE_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python)

###############################################################################
# should we use autodoc? swig 1.3.37 produce invalid code with autodoc.
# see https://sourceforge.net/tracker/index.php?func=detail&aid=2552048&group_id=1645&atid=101645
if("${SWIG_VERSION}" STREQUAL "1.3.37")
  set(WRAP_ITK_PYTHON_AUTODOC "" CACHE INTERNAL "python autodoc option")
else("${SWIG_VERSION}" STREQUAL "1.3.37")
  set(WRAP_ITK_PYTHON_AUTODOC -features autodoc=1 CACHE INTERNAL "python autodoc option")
endif("${SWIG_VERSION}" STREQUAL "1.3.37")

###############################################################################
# python 2.6 search for the modules in a "64" subdirectory, and don't provide
# any obvious way to find that. Create a link 64 -> . to make it find its
# files.
# TODO: file a bug report at opensolaris
if(CMAKE_SYSTEM MATCHES "SunOS-." AND NOT EXISTS "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/64")
  file(MAKE_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
  exec_program(ln ARGS -sf . "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/64")
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
                     "${WRAP_ITK_PYTHON_BINARY_DIR}/${config}/itkConfig.py"
                     @ONLY IMMEDIATE)
    endforeach(config)
  else(CMAKE_CONFIGURATION_TYPES)
    set(CONFIG_WRAP_ITK_DATA_ROOT "../Data")
    set(CONFIG_WRAP_ITK_TEST_ROOT "../Testing")
    set(CONFIG_PYTHON_CONFIGPY_DIR "Configuration")

    set(CONFIG_PYTHON_SWIGPY_DIR "../lib")
    set(CONFIG_PYTHON_SWIGLIB_DIR "../lib")
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
                   "${WRAP_ITK_PYTHON_BINARY_DIR}/itkConfig.py"
                   @ONLY IMMEDIATE)
  endif(CMAKE_CONFIGURATION_TYPES)

  set(CONFIG_WRAP_ITK_TEST_ROOT "None")
  set(CONFIG_PYTHON_SWIGLIB_DIR "../lib")
  set(CONFIG_PYTHON_SWIGPY_DIR "../lib")
  set(CONFIG_PYTHON_CONFIGPY_DIR "Configuration")
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/itkConfig.py.in"
                 "${WRAP_ITK_PYTHON_BINARY_DIR}/InstallOnly/itkConfig.py"
                @ONLY IMMEDIATE)

  WRAP_ITK_BINDINGS_INSTALL(/Python "${WRAP_ITK_PYTHON_BINARY_DIR}/InstallOnly/itkConfig.py")

  # copy the files to expose build options in python
  set(WRAP_ITK_PYTHON_VECTOR_REAL )
  foreach(t ${WRAP_ITK_VECTOR_REAL})
    foreach(d ${WRAP_ITK_DIMS})
      set(WRAP_ITK_PYTHON_VECTOR_REAL ${WRAP_ITK_PYTHON_VECTOR_REAL} ${ITKT_${t}${d}})
    endforeach(d)
  endforeach(t)
  set(WRAP_ITK_PYTHON_COV_VECTOR_REAL )
  foreach(t ${WRAP_ITK_COV_VECTOR_REAL})
    foreach(d ${WRAP_ITK_DIMS})
      set(WRAP_ITK_PYTHON_COV_VECTOR_REAL ${WRAP_ITK_PYTHON_COV_VECTOR_REAL} ${ITKT_${t}${d}})
    endforeach(d)
  endforeach(t)
  set(WRAP_ITK_PYTHON_RGB )
  foreach(t ${WRAP_ITK_RGB})
    set(WRAP_ITK_PYTHON_RGB ${WRAP_ITK_PYTHON_RGB} ${ITKT_${t}})
  endforeach(t)
  set(WRAP_ITK_PYTHON_RGBA )
  foreach(t ${WRAP_ITK_RGBA})
    set(WRAP_ITK_PYTHON_RGBA ${WRAP_ITK_PYTHON_RGBA} ${ITKT_${t}})
  endforeach(t)
  set(WRAP_ITK_PYTHON_COMPLEX_REAL )
  foreach(t ${WRAP_ITK_COMPLEX_REAL})
    set(WRAP_ITK_PYTHON_COMPLEX_REAL ${WRAP_ITK_PYTHON_COMPLEX_REAL} ${ITKT_${t}})
  endforeach(t)
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITKBuildOptionsPython.py.in"
                 "${WRAP_ITK_PYTHON_BINARY_DIR}/WrapITKBuildOptionsPython.py"
                @ONLY IMMEDIATE)
  WRAP_ITK_BINDINGS_INSTALL(/Python "${WRAP_ITK_PYTHON_BINARY_DIR}/WrapITKBuildOptionsPython.py")

  exec_program(${CMAKE_COMMAND} ARGS -E copy_if_different "\"${CMAKE_CURRENT_SOURCE_DIR}/WrapITKBuildOptionsConfig.py\"" "\"${CMAKE_CURRENT_BINARY_DIR}/Configuration/WrapITKBuildOptionsConfig.py\"")
  WRAP_ITK_BINDINGS_INSTALL(/Python/Configuration "${CMAKE_CURRENT_SOURCE_DIR}/WrapITKBuildOptionsConfig.py")

endif(NOT EXTERNAL_WRAP_ITK_PROJECT)


###############################################################################
# Configure the old style module

if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  set(WRAP_ITK_PYTHON_OLD_STYLE_IMPORTS )
  foreach(lib ${WRAP_ITK_LIBRARIES})
    set(WRAP_ITK_PYTHON_OLD_STYLE_IMPORTS "${WRAP_ITK_PYTHON_OLD_STYLE_IMPORTS}from ${lib}Python import *\n")
  endforeach(lib)
  configure_file("${WRAP_ITK_PYTHON_SOURCE_DIR}/InsightToolkit.py.in"
             "${WRAP_ITK_PYTHON_BINARY_DIR}/InsightToolkit.py"
             @ONLY IMMEDIATE)
  WRAP_ITK_BINDINGS_INSTALL(/Python "${WRAP_ITK_PYTHON_BINARY_DIR}/InsightToolkit.py")
endif(NOT EXTERNAL_WRAP_ITK_PROJECT)


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
  #  itkExtras/__init__
    itkLazy
    )
  # Done listing files.

  # Now copy these files if necessary.

  if("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
    # In source build -- no need to copy Python file. Do need to set up the install.
    foreach(file ${ITK_PYTHON_FILES})
      set(install_tgt "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
      set(WRAP_ITK_PYTHON_INSTALL_FILES ${WRAP_ITK_PYTHON_FILES} "${install_tgt}")
    endforeach(file)
  else("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")
    set(WRAP_ITK_PYTHON_FILES)
    add_custom_target(copy_python_files ALL)
    foreach(file ${ITK_PYTHON_FILES})
      set(src "${CMAKE_CURRENT_SOURCE_DIR}/${file}.py")
      # recall that WRAP_ITK_BUILD_INTDIR expands to nothing if no config types are set,
      # or to "CMAKE_CONFIG_INTDIR/" if there are such. Likewise, WRAP_ITK_INSTALL_INTDIR
      # expands to ${BUILD_TYPE}/ or nothing.
      set(copy_tgt "${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_BUILD_INTDIR}${file}.py")
      set(WRAP_ITK_PYTHON_FILES ${WRAP_ITK_PYTHON_FILES} "${copy_tgt}")
      set(install_tgt "${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_INSTALL_INTDIR}${file}.py")
      set(WRAP_ITK_PYTHON_INSTALL_FILES ${WRAP_ITK_PYTHON_INSTALL_FILES} "${install_tgt}")

      # create the directory to avoid loosing case on windows
      file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${WRAP_ITK_INSTALL_INTDIR}${path})

      add_custom_command(SOURCE ${src}
        COMMAND ${CMAKE_COMMAND}
        ARGS -E copy ${src} ${copy_tgt}
        OUTPUTS ${copy_tgt}
        TARGET copy_python_files
        COMMENT "Copying ${file}.py to build dir.")
    endforeach(file)

    # driver command to make the copy happen.
    add_custom_command(
      SOURCE copy_python_files
      DEPENDS ${WRAP_ITK_PYTHON_FILES}
      TARGET copy_python_files)
  endif("${WrapITK_BINARY_DIR}" MATCHES "^${WrapITK_SOURCE_DIR}$")

  # Install the package python files.
  WRAP_ITK_BINDINGS_INSTALL(/Python ${WRAP_ITK_PYTHON_INSTALL_FILES})

endif(NOT EXTERNAL_WRAP_ITK_PROJECT)


###############################################################################
# Configure and install the custom python .pth files

if(CMAKE_CONFIGURATION_TYPES)

  foreach(config ${CMAKE_CONFIGURATION_TYPES})
    set(CONFIG_WRAP_ITK_PYTHON_DIR "${CMAKE_CURRENT_BINARY_DIR}/${config}")

    # SWIG-generated libs and *.py files are sent to ${config} subdir
    # This assumes that CMAKE_LIBRARY_OUTPUT_DIRECTORY is WrapITK_BINARY_DIR/bin (bad!)
    # TODO: We need a better way to do this.
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/${config}/WrapITK.pth"
                   @ONLY IMMEDIATE)
  endforeach(config)
else(CMAKE_CONFIGURATION_TYPES)
  set(CONFIG_WRAP_ITK_PYTHON_DIR "${CMAKE_CURRENT_BINARY_DIR}")

  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/WrapITK.pth"
                 @ONLY IMMEDIATE)

endif(CMAKE_CONFIGURATION_TYPES)

set(CONFIG_WRAP_ITK_PYTHON_DIR "${CMAKE_INSTALL_PREFIX}/lib/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Python")
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/WrapITK.pth.in"
            "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/WrapITK.pth"
            @ONLY IMMEDIATE)

if(PYTHON_EXECUTABLE)
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/det_spp.py "try:\n  import distutils.sysconfig; print distutils.sysconfig.get_python_lib(1)\nexcept: pass")
  exec_program("${PYTHON_EXECUTABLE}"
    ARGS "\"${CMAKE_CURRENT_BINARY_DIR}/det_spp.py\""
    OUTPUT_VARIABLE py_spp
  )
endif(PYTHON_EXECUTABLE)

string(REGEX REPLACE "\\\\" "/" py_spp_nobackslashes "${py_spp}")
set(PY_SITE_PACKAGES_PATH "${py_spp_nobackslashes}" CACHE PATH "Python site-packages directory to install a .pth file pointing at WrapITK Python modules.")
mark_as_advanced(PY_SITE_PACKAGES_PATH)
if(PY_SITE_PACKAGES_PATH)
  INSTALL_AT_ABSOLUTE_PATH(install_wrapitk_compatibility "${PY_SITE_PACKAGES_PATH}" "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/WrapITK.pth")
endif(PY_SITE_PACKAGES_PATH)



# # compile python files
# set(PYTHON_MODULES "")
# foreach(m ${WRAP_ITK_MODULES})
#   set(PYTHON_MODULES ${PYTHON_MODULES} _${m}Python)
# endforeach(m)
#
# add_custom_target(PythonCompile ALL
#   ${PYTHON_EXECUTABLE}
#   ${PY_SITE_PACKAGES_PATH}/../compileall.py -l
#   -d "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/lib"
#   ${WrapITK_BINARY_DIR}/lib
#
#   COMMAND ${PYTHON_EXECUTABLE}
#   ${PY_SITE_PACKAGES_PATH}/../compileall.py -l
#   -d "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/Python"
#   ${WrapITK_BINARY_DIR}/Python
#
#   DEPENDS SwigRuntimePython ${PYTHON_MODULES}
# )


###############################################################################
# Install python stuff for external projects
if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  WRAP_ITK_INSTALL(/Configuration/Languages/Python
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
endif(NOT EXTERNAL_WRAP_ITK_PROJECT)





macro(WRAP_LIBRARY_PYTHON library_name)
  set(WRAP_ITK_PYTHON_CONFIGURATION_TEMPLATES "")
  set(WRAP_ITK_PYTHON_LIBRARY_IMPORTS "")
  set(WRAP_ITK_PYTHON_LIBRARY_DEPS )
  set(WRAP_ITK_PYTHON_LIBRARY_DECLS )
  set(WRAP_ITK_PYTHON_LIBRARY_CALLS )
  set(WRAP_ITK_PYTHON_CXX_FILES )
endmacro(WRAP_LIBRARY_PYTHON)


macro(END_WRAP_LIBRARY_PYTHON)

  # Loop over the extra swig input files and add them to the generated files
  # lists. Guess that the generated cxx output will have the same name as
  # the .i input file.
  set(WRAP_ITK_PYTHON_PROCCESS_SWIG_INPUTS ON)
  foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})
    get_filename_component(base_name ${source} NAME_WE)
    WRAP_MODULE_PYTHON("${base_name}")
    END_WRAP_MODULE_PYTHON("${base_name}")
  endforeach(source)
  set(WRAP_ITK_PYTHON_PROCCESS_SWIG_INPUTS OFF)

  # create the python config file
  # this file store all the name - type association and a dependencies list for the modules
  #
  # first build the dependency list
  set(WRAP_ITK_PYTHON_CONFIGURATION_DEPENDS "")

  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(WRAP_ITK_PYTHON_CONFIGURATION_DEPENDS "'${dep}', ${WRAP_ITK_PYTHON_CONFIGURATION_DEPENDS}")
    set(WRAP_ITK_PYTHON_LIBRARY_IMPORTS "import ${dep}Python\n${WRAP_ITK_PYTHON_LIBRARY_IMPORTS}")
  endforeach(dep)

  # ITKPyBase is always included, excepted ITKPyBase itself
  if(NOT "${WRAPPER_LIBRARY_NAME}" STREQUAL "ITKPyBase")
    set(WRAP_ITK_PYTHON_CONFIGURATION_DEPENDS "'ITKPyBase', ${WRAP_ITK_PYTHON_CONFIGURATION_DEPENDS}")
    set(WRAP_ITK_PYTHON_LIBRARY_IMPORTS "import ITKPyBasePython\n${WRAP_ITK_PYTHON_LIBRARY_IMPORTS}")
  endif(NOT "${WRAPPER_LIBRARY_NAME}" STREQUAL "ITKPyBase")

  # and create the file, with the var WRAP_ITK_PYTHON_CONFIGURATION_TEMPLATES and
  # WRAP_ITK_PYTHON_CONFIGURATION_DEPENDS created earlier
  configure_file("${WRAP_ITK_PYTHON_SOURCE_DIR}/ModuleConfig.py.in"
    "${WRAP_ITK_PYTHON_BINARY_DIR}/Configuration/${WRAPPER_LIBRARY_NAME}Config.py"
    @ONLY IMMEDIATE)
  WRAP_ITK_BINDINGS_INSTALL("/Python/Configuration"
    "${WRAP_ITK_PYTHON_BINARY_DIR}/Configuration/${WRAPPER_LIBRARY_NAME}Config.py"
  )


  # create the advanced lib module python file
  # this file let the final user _not_ use the itk module, but rather
  # something like "import Base"
  set(CONFIG_LIBRARY_NAME "${WRAPPER_LIBRARY_NAME}")
  configure_file("${WRAP_ITK_PYTHON_SOURCE_DIR}/ModuleLoader.py.in"
    "${WRAP_ITK_PYTHON_BINARY_DIR}/${WRAPPER_LIBRARY_NAME}.py"
    @ONLY IMMEDIATE)
  WRAP_ITK_BINDINGS_INSTALL("/Python" "${WRAP_ITK_PYTHON_BINARY_DIR}/${WRAPPER_LIBRARY_NAME}.py")

#   add_custom_target(${WRAPPER_LIBRARY_NAME}SwigPython DEPENDS ${cpp_files} ${python_files})
#   add_custom_target(${WRAPPER_LIBRARY_NAME}Python DEPENDS ${modules})

  # create the python customization stuff in the main module
  # it allow to group the pythons module in a single shared lib, by loading the int
  # functions of the module. I also import the objects from the submodules in the
  # main module.
  #
  # It uses WRAP_ITK_PYTHON_LIBRARY_DECLS, WRAP_ITK_PYTHON_LIBRARY_CALLS and
  # WRAP_ITK_PYTHON_LIBRARY_IMPORTS
  configure_file("${WRAP_ITK_PYTHON_SOURCE_DIR}/main_module_ext.i.in"
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${WRAPPER_LIBRARY_NAME}_ext.i"
    @ONLY IMMEDIATE)
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
  endif(EXTERNAL_WRAP_ITK_PROJECT)

  # and generate c++ code from the main module swig interface.
  set(swig_command ${SWIG_EXECUTABLE})
  if(WRAP_ITK_USE_CCACHE)
    set(swig_command ${CCACHE_EXECUTABLE} ${swig_command})
  endif(WRAP_ITK_USE_CCACHE)

  add_custom_command(
    OUTPUT ${cpp_file} ${python_file}
    COMMAND ${swig_command} -c++ -python -O ${WRAP_ITK_PYTHON_AUTODOC} -Werror
#       -fcompact
    -w508 -w312 -w314 -w509 -w302 -w362
    -w389 # operator[], to be suppressed later...
    -w384 -w383 # operator++ ane operator--
    -w361 # operator!
    -w467 # overloaded functions (with typemaps)
    -w350 -w394 -w395  # operator new/new[]/delete[] ignored
    -w303 # to avoid warning about customization in pyBase.i
    -o ${cpp_file}
    -I${LANGUAGES_SRC_DIR}
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}/python
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}
    ${WRAP_ITK_SWIG_ARGS_PYTHON}
    -outdir ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    ${interface_file}
    WORKING_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python
    DEPENDS ${DEPS} ${WRAP_ITK_PYTHON_LIBRARY_DEPS} ${interface_file} ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${WRAPPER_LIBRARY_NAME}_ext.i ${WRAP_ITK_TYPEDEFS_DIRECTORY}/pyBase.i ${SWIG_EXECUTABLE}
  )
  WRAP_ITK_BINDINGS_INSTALL("/Python" "${python_file}")

  # build all the c++ files from this module in a common lib
  set(lib ${WRAPPER_LIBRARY_NAME}Python)
  add_library(${lib} MODULE ${cpp_file} ${WRAP_ITK_PYTHON_CXX_FILES} ${WRAPPER_LIBRARY_CXX_SOURCES})
  set_target_properties(${lib} PROPERTIES PREFIX "_")
  # gcc 4.4 complains a lot without this flag when building in release mode
  if(CMAKE_COMPILER_IS_GNUCC)
    set_target_properties(${lib} PROPERTIES COMPILE_FLAGS -fno-strict-aliasing)
  endif(CMAKE_COMPILER_IS_GNUCC)
  # work around linkage problem on some solaris systems
  if(CMAKE_SYSTEM MATCHES "SunOS-." AND CMAKE_COMPILER_IS_GNUCXX AND CMAKE_COMPILER_IS_GNUCC)
    target_link_libraries(${lib} stdc++)
  endif(CMAKE_SYSTEM MATCHES "SunOS-." AND CMAKE_COMPILER_IS_GNUCXX AND CMAKE_COMPILER_IS_GNUCC)
  # extension is not the same on windows
  if(WIN32)
    set_target_properties(${lib} PROPERTIES SUFFIX .pyd)
  endif(WIN32)
  target_link_libraries(${lib} ${WRAPPER_LIBRARY_LINK_LIBRARIES} ${PYTHON_LIBRARY})
  add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Swig)
  if(WRAP_ITK_DOC)
    add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Doxygen)
  endif(WRAP_ITK_DOC)
  if(WRAP_ITK_EXPLICIT AND NOT ${WRAPPER_LIBRARY_NAME} STREQUAL ITKPyBase)
    target_link_libraries(${lib} ${WRAPPER_LIBRARY_NAME}Explicit)
    add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Explicit)
  endif(WRAP_ITK_EXPLICIT AND NOT ${WRAPPER_LIBRARY_NAME} STREQUAL ITKPyBase)
  # 64 bit binaries are not installed in the same directories on solaris
  set(sun64 )
  if(CMAKE_SYSTEM MATCHES "SunOS.*" AND CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(sun64 "/64")
  endif(CMAKE_SYSTEM MATCHES "SunOS.*" AND CMAKE_SIZEOF_VOID_P EQUAL 8)
  install(TARGETS "${lib}" DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Python${sun64}")
  if(NOT EXTERNAL_WRAP_ITK_PROJECT)
    # don't depends on the targets from wrapitk in external projects
    foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
      add_dependencies(${lib} ${dep}Swig)
      if(WRAP_ITK_DOC)
        add_dependencies(${lib} ${dep}Doxygen)
      endif(WRAP_ITK_DOC)
    endforeach(dep)
  endif(NOT EXTERNAL_WRAP_ITK_PROJECT)

endmacro(END_WRAP_LIBRARY_PYTHON)


macro(END_WRAP_MODULE_PYTHON group_name)

  set(base_name ${group_name})

  if("${group_name}" STREQUAL "itkVectorContainer")
    # add a template definition for the superclass which is not in ITK
    set(text )
    foreach(d ${WRAP_ITK_DIMS})
      foreach(t ${WRAP_ITK_SCALAR})
        set(text "${text}%template(vectoritkLevelSetNode${t}${d}) std::vector< itkLevelSetNode${t}${d} >;\n")
        ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkLevelSetNode${t}${d}" "${ITKT_LSN${t}${d}}")
      endforeach(t)

      set(text "${text}%template(vectoritkVectorD${d}) std::vector< itkVectorD${d} >;\n")
      ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkVectorD${d}" "${ITKT_VD${d}}")

      set(text "${text}%template(vectoritkPointD${d}) std::vector< itkPointD${d} >;\n")
      ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkPointD${d}" "${ITKT_PD${d}}")

      set(text "${text}%template(vectoritkOffset${d}) std::vector< itkOffset${d} >;\n")
      ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkOffset${d}" "${ITKT_O${d}}")

      set(text "${text}%template(vectoritkContinuousIndexD${d}) std::vector< itkContinuousIndexD${d} >;\n")
      ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkContinuousIndexD${d}" "${ITKT_CID${d}}")
    endforeach(d)

    set(text "${text}%template(vectoritkVectorUC1) std::vector< itkVectorUC1 >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkVectorUC1" "${ITKT_VUC1}")

    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}${text}")
  endif("${group_name}" STREQUAL "itkVectorContainer")

  if("${group_name}" STREQUAL "itkMapContainer")
    # add a template definition for the superclass which is not in ITK
    set(text )
    foreach(d ${WRAP_ITK_DIMS})
      set(text "${text}%template(mapULitkVectorD${d}) std::map< unsigned long, itkVectorD${d}, std::less< unsigned long > >;\n")
      ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "mapULitkVectorD${d}" "unsigned long, ${ITKT_VD${d}}")

      set(text "${text}%template(mapULitkPointD${d}) std::map< unsigned long, itkPointD${d}, std::less< unsigned long > >;\n")
      ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "mapULitkPointD${d}" "unsigned long, ${ITKT_PD${d}}")
    endforeach(d)

    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}${text}")
  endif("${group_name}" STREQUAL "itkMapContainer")

  if("${group_name}" STREQUAL "itkQuadEdgeMeshBase")
    # add a template definition for the superclass which is not in ITK
    set(text )
    foreach(d ${WRAP_ITK_DIMS})
      set(text "${text}%template(mapULitkQuadEdgeMeshPointF${d}) std::map< unsigned long, itkQuadEdgeMeshPointF${d}, std::less< unsigned long > >;\n")
      ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "mapULitkQuadEdgeMeshPointF${d}" "unsigned long, itk::QuadEdgeMeshPoint< float, ${d} >")

      # set(text "${text}%template(itkMapContainerMD${d}QBAIUL_Superclass) std::map< itkMeshD${d}Q::BoundaryAssignmentIdentifier, unsigned long, std::less< itkMeshD${d}Q::BoundaryAssignmentIdentifier > >;\n")
      # ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "itkMapContainerMD${d}QBAIUL_Superclass" "itk::Mesh<double, ${d}u, itk::QuadEdgeMeshTraits<double, ${d}, bool, bool, float, float> >::BoundaryAssignmentIdentifier, unsigned long")

      set(text "${text}%traits_swigtype(itkCellInterfaceDQEMCTI${d});\n")
      set(text "${text}%fragment(SWIG_Traits_frag(itkCellInterfaceDQEMCTI${d}));\n")
      set(text "${text}%template(mapULitkCellInterfaceDQEMCTI${d}) std::map< unsigned long, itkCellInterfaceDQEMCTI${d} *, std::less< unsigned long > >;\n")
      ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "mapULitkCellInterfaceDQEMCTI${d}" "unsigned long, itk::CellInterface< double, itk::QuadEdgeMeshCellTraitsInfo< ${d} > >*")
    endforeach(d)

    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}${text}")
  endif("${group_name}" STREQUAL "itkQuadEdgeMeshBase")

  # is there a docstring file?
  if(WRAP_ITK_DOC AND NOT WRAP_ITK_PYTHON_PROCCESS_SWIG_INPUTS)
    # yes. Include the docstring file
    set(doc_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_MODULE_NAME}_doc.i")
    set(WRAP_ITK_PYTHON_SWIG_EXT "%include ${WRAPPER_MODULE_NAME}_doc.i\n\n${WRAP_ITK_PYTHON_SWIG_EXT}")
  else(WRAP_ITK_DOC AND NOT WRAP_ITK_PYTHON_PROCCESS_SWIG_INPUTS)
    # no. Clear the doc_file var
    set(doc_file "")
  endif(WRAP_ITK_DOC AND NOT WRAP_ITK_PYTHON_PROCCESS_SWIG_INPUTS)

  # the default typemaps, exception handler, and includes
  set(WRAP_ITK_PYTHON_SWIG_EXT "%import pyBase.i\n\n${WRAP_ITK_PYTHON_SWIG_EXT}")

  # create the swig interface for all the groups in the module
  #
  set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${base_name}.i")
  set(lib ${group_name}Python)
  set(python_file "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${lib}.py")
  set(cpp_file "${CMAKE_CURRENT_BINARY_DIR}/${base_name}Python.cpp")

  # create the python customization for that wrap_*.cmake file.
  configure_file("${WRAP_ITK_PYTHON_SOURCE_DIR}/module_ext.i.in"
  "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${group_name}_ext.i"
  @ONLY IMMEDIATE)
  WRAP_ITK_INSTALL("/Configuration/Typedefs/python" "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${group_name}_ext.i"
  )

  # prepare dependencies
  set(DEPS )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(DEPS ${DEPS} ${${dep}SwigFiles})
  endforeach(dep)

  # and run swig to produce the c++ file and the .py file
  set(swig_command ${SWIG_EXECUTABLE})
  if(WRAP_ITK_USE_CCACHE)
    set(swig_command ${CCACHE_EXECUTABLE} ${swig_command})
  endif(WRAP_ITK_USE_CCACHE)

  add_custom_command(
    OUTPUT ${cpp_file} ${python_file}
    COMMAND ${swig_command} -c++ -python -O ${WRAP_ITK_PYTHON_AUTODOC} -Werror
#       -fcompact
    -w508 -w312 -w314 -w509 -w302 -w362
    -w389 # operator[], to be suppressed later...
    -w384 -w383 # operator++ ane operator--
    -w361 # operator!
    -w467 # overloaded functions (with typemaps)
    -w350 -w394 -w395  # operator new/new[]/delete[] ignored
    -w303 # to avoid warning about customization in pyBase.i
    -o ${cpp_file}
    -I${LANGUAGES_SRC_DIR}
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}/python
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}
    ${WRAP_ITK_SWIG_ARGS_PYTHON}
    -outdir ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    ${interface_file}
    WORKING_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python
    DEPENDS ${DEPS} ${interface_file} ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${base_name}_ext.i ${WRAP_ITK_TYPEDEFS_DIRECTORY}/pyBase.i ${doc_file} ${SWIG_EXECUTABLE}
    # ${WRAP_ITK_PYTHON_LIBRARY_DEPS}
  )
  WRAP_ITK_BINDINGS_INSTALL("/Python" "${python_file}")

# gcc visibility can't be used without getting errors when passing objects
# from one module to an other
#
#    if(CMAKE_COMPILER_IS_GNUCC)
#      set_target_properties(${lib} PROPERTIES COMPILE_FLAGS "-fvisibility=hidden")
#    endif(CMAKE_COMPILER_IS_GNUCC)

  # add the c++ files which will be generated by the swig command to the
  # list of python related c++ files, so they can be built at the end
  # of the current module.
  set(WRAP_ITK_PYTHON_CXX_FILES ${WRAP_ITK_PYTHON_CXX_FILES} ${cpp_file})

  # add needed files to the deps list
  set(WRAP_ITK_PYTHON_LIBRARY_DEPS ${WRAP_ITK_PYTHON_LIBRARY_DEPS} "${python_file}" "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${base_name}_ext.i" "${cpp_file}")

  # add this wrap_*.cmake stuff to the list of modules to init in the main module.
  # first the extern c declaration
  set(WRAP_ITK_PYTHON_LIBRARY_DECLS "${WRAP_ITK_PYTHON_LIBRARY_DECLS}extern \"C\" int init_${group_name}Python();\n")
  # and the call of the extern function
  set(WRAP_ITK_PYTHON_LIBRARY_CALLS "${WRAP_ITK_PYTHON_LIBRARY_CALLS}  init_${group_name}Python();\n")

endmacro(END_WRAP_MODULE_PYTHON group_name)



macro(ADD_ONE_TYPEDEF_PYTHON wrap_method wrap_class swig_name template_params)
  string(REGEX REPLACE "(.*::)" "" base_name "${wrap_class}")

  if(NOT "${wrap_class}" STREQUAL "MetaEvent" AND NOT "${wrap_method}" MATCHES "ENUM")
    ADD_PYTHON_CONFIG_TEMPLATE("${base_name}" "${wrap_class}" "${swig_name}" "${template_params}")
  endif(NOT "${wrap_class}" STREQUAL "MetaEvent" AND NOT "${wrap_method}" MATCHES "ENUM")

  # std::complex and vcl_complex are the same classes, but python don't know that
  if("${wrap_class}" STREQUAL "vcl_complex")
    ADD_PYTHON_CONFIG_TEMPLATE("complex" "std::complex" "${swig_name}" "${template_params}")
  endif("${wrap_class}" STREQUAL "vcl_complex")

endmacro(ADD_ONE_TYPEDEF_PYTHON)


macro(ADD_PYTHON_CONFIG_TEMPLATE base_name wrap_class swig_name template_params)
  # build the name - type association list used in *Config.py

  if("${template_params}" STREQUAL "")
    set(WRAP_ITK_PYTHON_CONFIGURATION_TEMPLATES "${WRAP_ITK_PYTHON_CONFIGURATION_TEMPLATES}  ('${base_name}', '${wrap_class}', '${swig_name}'),\n")
  else("${template_params}" STREQUAL "")
    set(WRAP_ITK_PYTHON_CONFIGURATION_TEMPLATES "${WRAP_ITK_PYTHON_CONFIGURATION_TEMPLATES}  ('${base_name}', '${wrap_class}', '${swig_name}', '${template_params}'),\n")
  endif("${template_params}" STREQUAL "")

endmacro(ADD_PYTHON_CONFIG_TEMPLATE)


macro(WRAP_MODULE_PYTHON module)
  set(WRAP_ITK_PYTHON_SWIG_EXT "")

  # register the module for the lib module
  set(WRAP_ITK_PYTHON_LIBRARY_IMPORTS "${WRAP_ITK_PYTHON_LIBRARY_IMPORTS}from ${module}Python import *\n")

endmacro(WRAP_MODULE_PYTHON)


macro(WRAP_NAMED_CLASS_PYTHON class swig_name)
  # store the current class wrapped, so we can generate the typemaps for itk::ImageSource
  set(WRAP_ITK_PYTHON_CURRENT_CLASS "${class}")
  set(WRAP_ITK_PYTHON_CURRENT_SWIG_NAME "${swig_name}")
endmacro(WRAP_NAMED_CLASS_PYTHON)


macro(WRAP_TEMPLATE_PYTHON name types)
  if("${WRAP_ITK_PYTHON_CURRENT_CLASS}" STREQUAL "itk::ImageSource")
    # generate the typemap which let pass an ImageSource instead of an Image
    set(image_source "${WRAP_ITK_PYTHON_CURRENT_SWIG_NAME}${name}")
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
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}${text}")

  endif("${WRAP_ITK_PYTHON_CURRENT_CLASS}" STREQUAL "itk::ImageSource")
endmacro(WRAP_TEMPLATE_PYTHON name types)


macro(ADD_SIMPLE_TYPEDEF_PYTHON wrap_class swig_name)
  # split the class name and the template parameters
  if("${wrap_class}" MATCHES "<.*>")
    string(REGEX REPLACE "^([^<]+)< *(.+) *>([^>]*)$" "\\1" cpp_name "${wrap_class}")
    string(REGEX REPLACE "^([^<]+)< *(.+) *>([^>]*)$" "\\2" template_params "${wrap_class}")
    string(REGEX REPLACE "^([^<]+)< *(.+) *>([^>]*)$" "\\3" ext_def "${wrap_class}")
  else("${wrap_class}" MATCHES "<.*>")
    set(cpp_name "${wrap_class}")
    set(template_params NO_TEMPLATE)
    set(ext_def "")
  endif("${wrap_class}" MATCHES "<.*>")
  string(REGEX REPLACE ".*::" "" simple_name "${cpp_name}")
#  message("${wrap_class} -- ${swig_name}")
#  message("${cpp_name} -- ${template_params} -- ${ext_def}")

  # must be done first so the typemap are used in the %template commands
  if("${swig_name}" MATCHES "_Pointer$")
    string(REGEX REPLACE "_Pointer$" "" smart_pointed "${swig_name}")
    ADD_PYTHON_POINTER_TYPEMAP("${smart_pointed}")
  endif()


  # and now, generate the typemaps and other customizations

  if("${cpp_name}" STREQUAL "vcl_complex")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}DECL_PYTHON_VCL_COMPLEX_CLASS(${swig_name})\n")
  endif("${cpp_name}" STREQUAL "vcl_complex")

  if("${swig_name}" STREQUAL "itkLightObject")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(listitkLightObject) std::list< itkLightObject_Pointer >;\n\n")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "listitkLightObject" "itk::LightObject")
  endif("${swig_name}" STREQUAL "itkLightObject")

  if("${swig_name}" STREQUAL "itkObject")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}DECL_PYTHON_OBJECT_CLASS(${swig_name})\n")
  endif("${swig_name}" STREQUAL "itkObject")

  if("${swig_name}" STREQUAL "itkProcessObject")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}DECL_PYTHON_PROCESSOBJECT_CLASS(${swig_name})\n\n")
  endif("${swig_name}" STREQUAL "itkProcessObject")

  if("${swig_name}" STREQUAL "itkDataObject")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(vectoritkDataObject) std::vector< itkDataObject_Pointer >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkDataObject" "itk::DataObject")
  endif("${swig_name}" STREQUAL "itkDataObject")

  if("${swig_name}" STREQUAL "itkObjectFactoryBase")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(listitkObjectFactoryBase) std::list< itkObjectFactoryBase * >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "listitkObjectFactoryBase" "itk::ObjectFactoryBase")
  endif("${swig_name}" STREQUAL "itkObjectFactoryBase")

  if("${swig_name}" STREQUAL "itkMetaDataDictionary")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(vectoritkMetaDataDictionary) std::vector< itkMetaDataDictionary * >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkMetaDataDictionary" "itk::MetaDataDictionary")
  endif("${swig_name}" STREQUAL "itkMetaDataDictionary")

  if("${swig_name}" STREQUAL "itkCommand")
    # make itk::Command hineritable in python
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%feature(\"director\") itkCommand;\n")
  endif("${swig_name}" STREQUAL "itkCommand")

  if("${cpp_name}" STREQUAL "itk::ImageBase" AND NOT "${swig_name}" MATCHES "Pointer$")
    # add the templated method non seen by gccxml, in a more python-friendly way
    # than the c++ version
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}DECL_PYTHON_IMAGEBASE_CLASS(${swig_name}, ${template_params})\n")
  endif("${cpp_name}" STREQUAL "itk::ImageBase" AND NOT "${swig_name}" MATCHES "Pointer$")

  if("${cpp_name}" STREQUAL "itk::StatisticsLabelObject" AND NOT "${swig_name}" MATCHES "Pointer$")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(map${swig_name}) std::map< unsigned long, ${swig_name}_Pointer, std::less< unsigned long > >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "map${swig_name}" "unsigned long, ${cpp_name}< ${template_params} >")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name}_Pointer >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
 endif("${cpp_name}" STREQUAL "itk::StatisticsLabelObject" AND NOT "${swig_name}" MATCHES "Pointer$")

  if("${cpp_name}" STREQUAL "itk::LabelMap" AND NOT "${swig_name}" MATCHES "Pointer$")
      set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}${WRAP_ITK_PYTHON_SWIG_EXT}DECL_PYTHON_LABELMAP_CLASS(${swig_name})\n")
  endif("${cpp_name}" STREQUAL "itk::LabelMap" AND NOT "${swig_name}" MATCHES "Pointer$")

  if("${cpp_name}" STREQUAL "itk::ComponentTreeNode")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(list${swig_name}) std::list< ${swig_name}* >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "list${swig_name}" "${cpp_name}< ${template_params} > *")
  endif()

  if("${cpp_name}" STREQUAL "itk::ImageRegion")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}${WRAP_ITK_PYTHON_SWIG_EXT}DECL_PYTHON_IMAGEREGION_CLASS(${swig_name})\n")
  endif("${cpp_name}" STREQUAL "itk::ImageRegion")


  if("${cpp_name}" STREQUAL "itk::Index")
    ADD_PYTHON_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::Index")

  if("${cpp_name}" STREQUAL "itk::Size")
    ADD_PYTHON_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::Size")

  if("${cpp_name}" STREQUAL "itk::RGBPixel")
    # number of elements is not in the template parameters so use the
    # macro which get it with Size() instead
    ADD_PYTHON_VARIABLE_LENGTH_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::RGBPixel")

  if("${cpp_name}" STREQUAL "itk::RGBAPixel")
    # number of elements is not in the template parameters so use the
    # macro which get it with Size() instead
    ADD_PYTHON_VARIABLE_LENGTH_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::RGBAPixel")

  if("${cpp_name}" STREQUAL "itk::Offset")
    ADD_PYTHON_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::Offset")

  if("${cpp_name}" STREQUAL "itk::FixedArray")
    ADD_PYTHON_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::FixedArray")

  if("${cpp_name}" STREQUAL "itk::Vector")
    ADD_PYTHON_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::Vector")

  if("${cpp_name}" STREQUAL "itk::CovariantVector")
    ADD_PYTHON_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::CovariantVector")

  if("${cpp_name}" STREQUAL "itk::Point")
    ADD_PYTHON_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::Point")

  if("${cpp_name}" STREQUAL "itk::ContinuousIndex")
    ADD_PYTHON_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::ContinuousIndex")

  if("${cpp_name}" STREQUAL "itk::Array")
    ADD_PYTHON_VARIABLE_LENGTH_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif("${cpp_name}" STREQUAL "itk::Array")

  if("${swig_name}" STREQUAL "itkTransformBase")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(list${swig_name}_Pointer) std::list< ${swig_name}_Pointer >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "list${swig_name}_Pointer" "itk::TransformBase")
  endif("${swig_name}" STREQUAL "itkTransformBase")

  if("${cpp_name}" STREQUAL "itk::SpatialObjectPoint")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}DECL_PYTHON_SPATIALOBJECTPPOINT_CLASS(${swig_name})%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif("${cpp_name}" STREQUAL "itk::SpatialObjectPoint")

  if("${cpp_name}" STREQUAL "itk::ContourSpatialObjectPoint")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif("${cpp_name}" STREQUAL "itk::ContourSpatialObjectPoint")

  if("${cpp_name}" STREQUAL "itk::LineSpatialObjectPoint")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif("${cpp_name}" STREQUAL "itk::LineSpatialObjectPoint")

  if("${cpp_name}" STREQUAL "itk::SurfaceSpatialObjectPoint")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif("${cpp_name}" STREQUAL "itk::SurfaceSpatialObjectPoint")

  if("${cpp_name}" STREQUAL "itk::SpatialObject" AND NOT "${ext_def}" MATCHES "Pointer")
    set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}%template(list${swig_name}_Pointer) std::list< ${swig_name}_Pointer >;\n")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "list${swig_name}_Pointer" "${cpp_name}< ${template_params} >")
  endif()

endmacro(ADD_SIMPLE_TYPEDEF_PYTHON)


macro(ADD_PYTHON_SEQ_TYPEMAP swig_name dim)
  set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}DECL_PYTHON_SEQ_TYPEMAP(${swig_name}, ${dim})\n")
endmacro(ADD_PYTHON_SEQ_TYPEMAP)


macro(ADD_PYTHON_VEC_TYPEMAP swig_name template_params)
  string(REGEX REPLACE "(.*),(.*)" "\\1" type "${template_params}")
  string(REGEX REPLACE "(.*),(.*)" "\\2" dim "${template_params}")
  set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}DECL_PYTHON_VEC_TYPEMAP(${swig_name}, ${type}, ${dim})\n")
endmacro(ADD_PYTHON_VEC_TYPEMAP)

macro(ADD_PYTHON_VARIABLE_LENGTH_SEQ_TYPEMAP type value_type)
  set(WRAP_ITK_PYTHON_SWIG_EXT "${WRAP_ITK_PYTHON_SWIG_EXT}DECL_PYTHON_VARLEN_SEQ_TYPEMAP(${type}, ${value_type})\n")
endmacro(ADD_PYTHON_VARIABLE_LENGTH_SEQ_TYPEMAP)


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

  set(WRAP_ITK_PYTHON_SWIG_EXT "${text}${WRAP_ITK_PYTHON_SWIG_EXT}")
endmacro(ADD_PYTHON_POINTER_TYPEMAP)


###############################################################################
# Create the PyUtils library

if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  subdirs(Tests)
  subdirs(itkExtras)

  macro(END_WRAP_LIBRARIES_PYTHON)
    add_subdirectory(${WRAP_ITK_PYTHON_SOURCE_DIR}/PyUtils)
  endmacro(END_WRAP_LIBRARIES_PYTHON)

  macro(WRAP_LIBRARIES_PYTHON)
    add_subdirectory(${WRAP_ITK_PYTHON_SOURCE_DIR}/PyBase)
  endmacro(WRAP_LIBRARIES_PYTHON)

else(NOT EXTERNAL_WRAP_ITK_PROJECT)
  macro(END_WRAP_LIBRARIES_PYTHON)
    # just do nothing
  endmacro(END_WRAP_LIBRARIES_PYTHON)

  macro(WRAP_LIBRARIES_PYTHON)
    # just do nothing
  endmacro(WRAP_LIBRARIES_PYTHON)

endif(NOT EXTERNAL_WRAP_ITK_PROJECT)

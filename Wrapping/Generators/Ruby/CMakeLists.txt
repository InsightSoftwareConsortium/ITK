###############################################################################
# configure ruby
# Ruby support is currently broken. Show the warning message.
message(WARNING "RUBY support in WrapITK is currently broken!")

if(WRAP_ITK_INSTALL_COMPONENT_PER_MODULE)
  message(WARNING "Option WRAP_ITK_INSTALL_COMPONENT_PER_MODULE is not supported for Ruby wrapping language")
endif()

find_package(Ruby)
mark_as_advanced(RUBY_EXECUTABLE)
include_directories(${RUBY_INCLUDE_PATH})


###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_RUBY_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "ruby source dir")
set(ITK_WRAP_RUBY_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "ruby binary dir")


###############################################################################
# create the ruby directory in the classindex dir
file(MAKE_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/ruby)





macro(itk_wrap_module_ruby library_name)
  set(ITK_WRAP_RUBY_CONFIGURATION_TEMPLATES "")
  set(ITK_WRAP_RUBY_LIBRARY_IMPORTS "")
  set(ITK_WRAP_RUBY_LIBRARY_DEPS )
  set(ITK_WRAP_RUBY_LIBRARY_DECLS )
  set(ITK_WRAP_RUBY_LIBRARY_CALLS )
  set(ITK_WRAP_RUBY_CXX_FILES )
endmacro()


macro(itk_end_wrap_module_ruby)

  # Loop over the extra swig input files and add them to the generated files
  # lists. Guess that the generated cxx output will have the same name as
  # the .i input file.
  foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})
    get_filename_component(base_name ${source} NAME_WE)
    string(REPLACE "wrap_" "" base_name "${base_name}")
    itk_wrap_submodule_ruby("${base_name}")
    itk_end_wrap_submodule_ruby("${base_name}")
  endforeach()

  # create the ruby config file
  # this file store all the name - type association and a dependencies list for the modules
  #
  # first build the dependency list
  set(ITK_WRAP_RUBY_CONFIGURATION_DEPENDS "")

  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(ITK_WRAP_RUBY_CONFIGURATION_DEPENDS "'${dep}', ${ITK_WRAP_RUBY_CONFIGURATION_DEPENDS}")
    set(ITK_WRAP_RUBY_LIBRARY_IMPORTS "import ${dep}Ruby\n${ITK_WRAP_RUBY_LIBRARY_IMPORTS}")
    # add the rb_require() calls for the real binary files
    set(ITK_WRAP_RUBY_LIBRARY_CALLS "  rb_require_wrapitk(\"${dep}Ruby\");\n${ITK_WRAP_RUBY_LIBRARY_CALLS}")
  endforeach()

  # ITKRbBase is always included, excepted ITKRbBase itself
  if(NOT "${WRAPPER_LIBRARY_NAME}" STREQUAL "ITKRbBase")
    set(ITK_WRAP_RUBY_CONFIGURATION_DEPENDS "'ITKRbBase', ${ITK_WRAP_RUBY_CONFIGURATION_DEPENDS}")
    set(ITK_WRAP_RUBY_LIBRARY_IMPORTS "import ITKRbBaseRuby\n${ITK_WRAP_RUBY_LIBRARY_IMPORTS}")
    set(ITK_WRAP_RUBY_LIBRARY_CALLS "  rb_require_wrapitk(\"ITKRbBaseRuby\");\n${ITK_WRAP_RUBY_LIBRARY_CALLS}")
  endif()

  # create the ruby customization stuff in the main module
  # it allow to group the rubys module in a single shared lib, by loading the int
  # functions of the module. I also import the objects from the submodules in the
  # main module.
  #
  # It uses ITK_WRAP_RUBY_LIBRARY_DECLS, ITK_WRAP_RUBY_LIBRARY_CALLS and
  # ITK_WRAP_RUBY_LIBRARY_IMPORTS
  configure_file("${ITK_WRAP_RUBY_SOURCE_DIR}/main_module_ext.i.in"
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/ruby/${WRAPPER_LIBRARY_NAME}_ext.i"
    @ONLY)
  )


  # set some var reused later
  set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}Module.i")
  set(lib ${WRAPPER_LIBRARY_NAME}Ruby)
  set(cpp_file "${CMAKE_CURRENT_BINARY_DIR}/${WRAPPER_LIBRARY_NAME}Ruby.cpp")

  # if this is for an external library, let the user add extra swig args
  if(EXTERNAL_WRAP_ITK_PROJECT)
    set(WRAP_ITK_SWIG_ARGS_RUBY "" CACHE STRING "Extra user-defined swig arguments to be to the swig executable.")
  endif()

  # and generate c++ code from the main module swig interface.
  add_custom_command(
    OUTPUT ${cpp_file}.rb_require
    COMMAND ${SWIG_EXECUTABLE} -c++ -ruby -O -features autodoc=1 -autorename # -Werror
#       -fcompact
    -w362 # operator= ignored
    -w366 # operator-= ignored
    -w367 # operator*= ignored
    -w368 # operator/= ignored
    -w509 # Overloaded method is shadowed
    -w389 # operator[] ignored
    -w378 # operator!=
    -w361 # operator!
    -w365 # operator+=
    -w801 # Wrong class name
    -w508 # Declaration of 'Print' shadows declaration accessible via operator->()
    -w302 # previous definition of 'a_method' (Renamed from 'AMethod')
    -o ${cpp_file}.rb_require
    -I${GENERATORS_SRC_DIR}
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}/ruby
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}
    ${WRAP_ITK_SWIG_ARGS_RUBY}
    ${interface_file}
    WORKING_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/ruby
    DEPENDS ${DEPS} ${ITK_WRAP_RUBY_LIBRARY_DEPS} ${interface_file} ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/ruby/${WRAPPER_LIBRARY_NAME}_ext.i ${WRAP_ITK_TYPEDEFS_DIRECTORY}/wrap_rbBase.i ${SWIG_EXECUTABLE}
  )

  add_custom_command(
    OUTPUT ${cpp_file}
    COMMAND ${RUBY_EXECUTABLE} ${ITK_WRAP_RUBY_SOURCE_DIR}/fix_require.rb
    ${cpp_file}.rb_require
    ${cpp_file}
    WORKING_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/ruby
    DEPENDS ${cpp_file}.rb_require
  )

  # build all the c++ files from this module in a common lib
  set(lib ${WRAPPER_LIBRARY_NAME}Ruby)
  add_library(${lib} MODULE ${cpp_file} ${ITK_WRAP_RUBY_CXX_FILES} ${WRAPPER_LIBRARY_CXX_SOURCES})
  set_target_properties(${lib} PROPERTIES PREFIX "")
  if(APPLE)
    set_target_properties(${lib} PROPERTIES SUFFIX ".bundle")
  endif()
  target_link_libraries(${lib} ${WRAPPER_LIBRARY_LINK_LIBRARIES} ${RUBY_LIBRARY})
  add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Swig)
  install(TARGETS "${lib}"
    DESTINATION "${ITK_INSTALL_LIBRARY_DIR}/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Ruby"
    COMPONENT ${WRAP_ITK_INSTALL_COMPONENT_IDENTIFIER}RuntimeLibraries
    )
  if(NOT EXTERNAL_WRAP_ITK_PROJECT)
    # don't depends on the targets from wrapitk in external projects
    foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
      add_dependencies(${lib} ${dep}Swig)
    endforeach()
  endif()

endmacro()


macro(itk_end_wrap_submodule_ruby group_name)

  set(base_name wrap_${group_name})

  # the default typemaps, exception handler, and includes
  set(ITK_WRAP_RUBY_SWIG_EXT "%import wrap_rbBase.i\n\n${ITK_WRAP_RUBY_SWIG_EXT}")


  # create the swig interface for all the groups in the module
  #
  set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${base_name}.i")
  set(lib ${group_name}Ruby)
  set(cpp_file "${CMAKE_CURRENT_BINARY_DIR}/${base_name}Ruby.cpp")

  # create the ruby customization for that wrap_*.cmake file.
  configure_file("${ITK_WRAP_RUBY_SOURCE_DIR}/module_ext.i.in"
  "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/ruby/wrap_${group_name}_ext.i"
  @ONLY)
  )

  # prepare dependencies
  set(DEPS )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    list(APPEND DEPS ${${dep}SwigFiles})
  endforeach()

  # and run swig to produce the c++ file and the .rb file
  add_custom_command(
    OUTPUT ${cpp_file}.rb_require
    COMMAND ${SWIG_EXECUTABLE} -c++ -ruby -O -features autodoc=1 -autorename # -Werror
#       -fcompact
    -w362 # operator= ignored
    -w366 # operator-= ignored
    -w367 # operator*= ignored
    -w368 # operator/= ignored
    -w509 # Overloaded method is shadowed
    -w389 # operator[] ignored
    -w378 # operator!=
    -w361 # operator!
    -w365 # operator+=
    -w801 # Wrong class name
    -w508 # Declaration of 'Print' shadows declaration accessible via operator->()
    -w302 # previous definition of 'a_method' (Renamed from 'AMethod')
    -o ${cpp_file}.rb_require
    -I${GENERATORS_SRC_DIR}
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}/ruby
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}
    ${WRAP_ITK_SWIG_ARGS_RUBY}
    ${interface_file}
    WORKING_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/ruby
    DEPENDS ${DEPS} ${interface_file} ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/ruby/${base_name}_ext.i ${WRAP_ITK_TYPEDEFS_DIRECTORY}/wrap_rbBase.i ${SWIG_EXECUTABLE}
    # ${ITK_WRAP_RUBY_LIBRARY_DEPS}
  )

  add_custom_command(
    OUTPUT ${cpp_file}
    COMMAND ${RUBY_EXECUTABLE} ${ITK_WRAP_RUBY_SOURCE_DIR}/fix_require.rb
    ${cpp_file}.rb_require
    ${cpp_file}
    WORKING_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/ruby
    DEPENDS ${cpp_file}.rb_require
  )

# gcc visibility can't be used without getting errors when passing objects
# from one module to an other
#
#    if(CMAKE_COMPILER_IS_GNUCC)
#      set_target_properties(${lib} PROPERTIES COMPILE_FLAGS "-fvisibility=hidden")
#    endif()

  # add the c++ files which will be generated by the swig command to the
  # list of ruby related c++ files, so they can be built at the end
  # of the current module.
  list(APPEND ITK_WRAP_RUBY_CXX_FILES ${cpp_file})

  # add needed files to the deps list
  list(APPEND ITK_WRAP_RUBY_LIBRARY_DEPS "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/ruby/${base_name}_ext.i" "${cpp_file}")

  # add this wrap_*.cmake stuff to the list of modules to init in the main module.
  # first the extern c declaration
  set(ITK_WRAP_RUBY_LIBRARY_DECLS "${ITK_WRAP_RUBY_LIBRARY_DECLS}extern \"C\" int Init_${group_name}Ruby();\n")
  # and the call of the extern function
  set(ITK_WRAP_RUBY_LIBRARY_CALLS "${ITK_WRAP_RUBY_LIBRARY_CALLS}  Init_${group_name}Ruby();\n")

endmacro()



macro(itk_wrap_one_type_ruby wrap_method wrap_class swig_name template_params)
  string(REGEX REPLACE "(.*::)" "" base_name "${wrap_class}")

  if(NOT "${wrap_class}" STREQUAL "MetaEvent")
    ADD_RUBY_CONFIG_TEMPLATE("${base_name}" "${wrap_class}" "${swig_name}" "${template_params}")
  endif()

  # std::complex and vcl_complex are the same classes, but ruby don't know that
  if("${wrap_class}" STREQUAL "vcl_complex")
    ADD_RUBY_CONFIG_TEMPLATE("complex" "std::complex" "${swig_name}" "${template_params}")
  endif()

endmacro()


macro(ADD_RUBY_CONFIG_TEMPLATE base_name wrap_class swig_name template_params)
  # build the name - type association list used in *Config.rb

  if("${template_params}" STREQUAL "")
    set(ITK_WRAP_RUBY_CONFIGURATION_TEMPLATES "${ITK_WRAP_RUBY_CONFIGURATION_TEMPLATES}  ('${base_name}', '${wrap_class}', '${swig_name}'),\n")
  else()
    set(ITK_WRAP_RUBY_CONFIGURATION_TEMPLATES "${ITK_WRAP_RUBY_CONFIGURATION_TEMPLATES}  ('${base_name}', '${wrap_class}', '${swig_name}', '${template_params}'),\n")
  endif()

endmacro()


macro(itk_wrap_submodule_ruby module)
  set(ITK_WRAP_RUBY_SWIG_EXT "")

  # register the module for the lib module
  set(ITK_WRAP_RUBY_LIBRARY_IMPORTS "${ITK_WRAP_RUBY_LIBRARY_IMPORTS}from ${module}Ruby import *\n")

endmacro()


macro(itk_wrap_named_class_ruby class swig_name)
  # store the current class wrapped, so we can generate the typemaps for itk::ImageSource
  set(ITK_WRAP_RUBY_CURRENT_CLASS "${class}")
  set(ITK_WRAP_RUBY_CURRENT_SWIG_NAME "${swig_name}")
endmacro()


macro(itk_wrap_template_ruby name types)
  if("${ITK_WRAP_RUBY_CURRENT_CLASS}" STREQUAL "itk::ImageSource")
    # generate the typemap which let pass an ImageSource instead of an Image
    set(image_source "${ITK_WRAP_RUBY_CURRENT_SWIG_NAME}${name}")
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
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")

  endif()
endmacro()


macro(itk_wrap_simple_type_ruby wrap_class swig_name)
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
#  message("${wrap_class} -- ${swig_name}")
#  message("${cpp_name} -- ${template_params} -- ${ext_def}")

  # must be done first so the typemap are used in the %template commands
  if("${wrap_class}" MATCHES "SmartPointer$")
    string(REGEX REPLACE "_Pointer$" "" smart_pointed "${swig_name}")
    ADD_RUBY_POINTER_TYPEMAP("${smart_pointed}")
  endif()


  # and now, generate the typemaps and other customizations

  if("${swig_name}" STREQUAL "itkLightObject")
    set(text "\n\n")
    set(text "${text}%extend itkLightObject {\n")
    set(text "${text}  std::string __str__() {\n")
    set(text "${text}    itk::OStringStream msg;\n")
    set(text "${text}    self->Print( msg );\n")
    set(text "${text}    return msg.str();\n")
    set(text "${text}  }\n")
    set(text "${text}}\n")
# swig generates invalid code with that simple typemap
#     set(text "${text}%typemap(in) ${swig_name}_Pointer const & {\n")
#     set(text "${text}  // tralala\n")
#     set(text "${text}  ${swig_name} * ptr;\n")
#     set(text "${text}//          if ((SWIG_ConvertPtr($input,(void **) &ptr, $descriptor(${swig_name}), 0)) == -1) return NULL;\n")
#     set(text "${text}$1 = ptr;\n")
#     set(text "${text}}\n")
    set(text "${text}\n\n")

#    set(text "${text}%template(listitkLightObject) std::list< itkLightObject_Pointer >;\n")
    ADD_RUBY_CONFIG_TEMPLATE("list" "std::list" "listitkLightObject" "itk::LightObject")

    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")
  endif()

  if("${swig_name}" STREQUAL "itkObject")
    set(text "\n\n")
    set(text "${text}%rubyprepend itkObject::AddObserver %{\n")
    set(text "${text}        import itk\n")
    set(text "${text}        if len(args) == 3 and not issubclass(args[2].__class__, itk.Command) and callable(args[2]):\n")
    set(text "${text}          # wrap the callable ruby object in a PyCommand\n")
    set(text "${text}          args = list(args)\n")
    set(text "${text}          pycommand = itk.PyCommand.New()\n")
    set(text "${text}          pycommand.SetCommandCallable( args[2] )\n")
    set(text "${text}          args[2] = pycommand\n")
    set(text "${text}          args = tuple(args)\n")
    set(text "${text}%}\n")
    set(text "${text}\n\n")

#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")
  endif()


  if("${swig_name}" STREQUAL "itkProcessObject")
    set(text "\n\n")
    set(text "${text}%extend itkProcessObject {\n")
    set(text "${text}  %rubycode {\n")
    set(text "${text}    def __len__(self):\n")
    set(text "${text}        return self.GetNumberOfOutputs()\n")
    set(text "${text}      \n")
    set(text "${text}    def __getitem__(self, item):\n")
    set(text "${text}        import itk\n")
    set(text "${text}        outputs = self.GetOutputs()\n")
    set(text "${text}        if isinstance(item, slice):\n")
    set(text "${text}            indices = item.indices(len(self))\n")
    set(text "${text}            return [itk.down_cast(outputs[i]) for i in range(*indices)]\n")
    set(text "${text}        else:\n")
    set(text "${text}            return itk.down_cast(outputs[item])\n")
    set(text "${text}      \n")
    set(text "${text}    def __call__(self, *args, **kargs):\n")
    set(text "${text}        import itk\n")
    set(text "${text}        itk.set_inputs( self, args, kargs )\n")
    set(text "${text}        self.UpdateLargestPossibleRegion()\n")
    set(text "${text}        return self\n")
    set(text "${text}\n")
    set(text "${text}  }\n")
    set(text "${text}}\n")
    set(text "${text}\n\n")

#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")
  endif()


  if("${swig_name}" STREQUAL "itkDataObject")
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(vectoritkDataObject) std::vector< itkDataObject_Pointer >;\n")
#    ADD_RUBY_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkDataObject" "itk::DataObject")
  endif()

  if("${swig_name}" STREQUAL "itkObjectFactoryBase")
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(listitkObjectFactoryBase) std::list< itkObjectFactoryBase * >;\n")
#    ADD_RUBY_CONFIG_TEMPLATE("list" "std::list" "listitkObjectFactoryBase" "itk::ObjectFactoryBase")
  endif()

  if("${swig_name}" STREQUAL "itkMetaDataDictionary")
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(vectoritkMetaDataDictionary) std::vector< itkMetaDataDictionary * >;\n")
#    ADD_RUBY_CONFIG_TEMPLATE("vector" "std::vector" "vectoritkMetaDataDictionary" "itk::MetaDataDictionary")
  endif()

  if("${swig_name}" STREQUAL "itkCommand")
    # make itk::Command hineritable in ruby
    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%feature(\"director\") itkCommand;\n")
  endif()

  if("${cpp_name}" STREQUAL "itk::ImageBase" AND NOT "${swig_name}" MATCHES "Pointer$")
    # add the templated method non seen by gccxml, in a more ruby-friendly way
    # than the c++ version
    set(d ${template_params})
    set(text "")
    set(text "${text}\n")
    set(text "${text}%extend ${swig_name} {\n")
    set(text "${text}  itkIndex${d} TransformPhysicalPointToIndex( itkPointD${d} & point ) {\n")
    set(text "${text}    itkIndex${d} idx;\n")
    set(text "${text}    self->TransformPhysicalPointToIndex<double>( point, idx );\n")
    set(text "${text}    return idx;\n")
    set(text "${text}  }\n")
    set(text "${text}  \n")
    set(text "${text}  itkContinuousIndexD${d} TransformPhysicalPointToContinuousIndex( itkPointD${d} & point ) {\n")
    set(text "${text}    itkContinuousIndexD${d} idx;\n")
    set(text "${text}    self->TransformPhysicalPointToContinuousIndex<double>( point, idx );\n")
    set(text "${text}    return idx;\n")
    set(text "${text}  }\n")
    set(text "${text}  \n")
    set(text "${text}  itkPointD${d} TransformContinuousIndexToPhysicalPoint( itkContinuousIndexD${d} & idx ) {\n")
    set(text "${text}    itkPointD${d} point;\n")
    set(text "${text}    self->TransformContinuousIndexToPhysicalPoint<double>( idx, point );\n")
    set(text "${text}    return point;\n")
    set(text "${text}  }\n")
    set(text "${text}  \n")
    set(text "${text}  itkPointD${d} TransformIndexToPhysicalPoint( itkIndex${d} & idx ) {\n")
    set(text "${text}    itkPointD${d} point;\n")
    set(text "${text}    self->TransformContinuousIndexToPhysicalPoint<double>( idx, point );\n")
    set(text "${text}    return point;\n")
    set(text "${text}  }\n")
    set(text "${text}  \n")
# TODO: also add that method. But with which types?
#  template<class TCoordRep>
#  void TransformLocalVectorToPhysicalVector(
#    const FixedArray<TCoordRep, VImageDimension> & inputGradient,
#          FixedArray<TCoordRep, VImageDimension> & outputGradient ) const
    set(text "${text}}\n")
    set(text "${text}\n")

    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")
  endif()

  if("${cpp_name}" STREQUAL "itk::StatisticsLabelObject" AND NOT "${swig_name}" MATCHES "Pointer$")
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(map${swig_name}) std::map< unsigned long, ${swig_name}_Pointer, std::less< unsigned long > >;\n")
#    ADD_RUBY_CONFIG_TEMPLATE("map" "std::map" "map${swig_name}" "unsigned long, ${cpp_name}< ${template_params} >")
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name}_Pointer >;\n")
#    ADD_RUBY_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
 endif()

  if("${cpp_name}" STREQUAL "itk::LabelMap" AND NOT "${swig_name}" MATCHES "Pointer$")
    set(text "${text}%extend ${swig_name} {\n")
    set(text "${text}  %rubycode {\n")
    set(text "${text}    def __len__(self):\n")
    set(text "${text}        return self.GetNumberOfLabelObjects()\n")
    set(text "${text}      \n")
    set(text "${text}    def __getitem__(self, label):\n")
    set(text "${text}        return self.GetLabelObject(label)\n")
    set(text "${text}      \n")
    set(text "${text}    def __iter__(self):\n")
    set(text "${text}        labels = self.GetLabels()\n")
    set(text "${text}        for label in labels:\n")
    set(text "${text}          yield self.GetLabelObject(label)\n")
    set(text "${text}\n")
    set(text "${text}  }\n")
    set(text "${text}}\n")
    set(text "${text}\n\n")

#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")
  endif()

  if("${cpp_name}" STREQUAL "itk::ImageRegion")
    set(text "")
    set(text "${text}%extend ${swig_name} {\n")
    set(text "${text}  std::string __repr__() {\n")
    set(text "${text}    itk::OStringStream msg;\n")
    set(text "${text}    msg << \"${swig_name}(\" << self->GetIndex() << \", \" << self->GetSize()  << \")\";\n")
    set(text "${text}    return msg.str();\n")
    set(text "${text}  }\n")
    set(text "${text}}\n")

    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Index")
#    ADD_RUBY_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Size")
#    ADD_RUBY_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::RGBPixel")
    # number of elements is not in the template parameters so use the
    # macro which get it with Size() instead
#    ADD_RUBY_VARIABLE_LENGHT_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::RGBAPixel")
    # number of elements is not in the template parameters so use the
    # macro which get it with Size() instead
#    ADD_RUBY_VARIABLE_LENGHT_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Offset")
#    ADD_RUBY_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::FixedArray")
#    ADD_RUBY_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Vector")
#    ADD_RUBY_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::CovariantVector")
#    ADD_RUBY_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Point")
#    ADD_RUBY_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::ContinuousIndex")
#    ADD_RUBY_VEC_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Array")
#    ADD_RUBY_VARIABLE_LENGHT_SEQ_TYPEMAP("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::VectorContainer" AND NOT "${swig_name}" MATCHES "Pointer$")
    # add a template definition for the superclass which is not in ITK
    string(REGEX REPLACE "^[^,]+, *(.+) *$" "\\1" superclass_template_param "${template_params}")
    if("${superclass_template_param}" MATCHES "::")
      set(param "${superclass_template_param}")
      string(REPLACE "::" "" superclass_template_param "${superclass_template_param}")
      string(REPLACE "unsigned" "U" superclass_template_param "${superclass_template_param}")
      string(REPLACE "signed" "S" superclass_template_param "${superclass_template_param}")
      string(REPLACE "char" "C" superclass_template_param "${superclass_template_param}")
      string(REPLACE "short" "S" superclass_template_param "${superclass_template_param}")
      string(REPLACE "long" "L" superclass_template_param "${superclass_template_param}")
      string(REPLACE "float" "F" superclass_template_param "${superclass_template_param}")
      string(REPLACE "double" "D" superclass_template_param "${superclass_template_param}")
      string(REPLACE " " "" superclass_template_param "${superclass_template_param}")
      string(REPLACE "<" "" superclass_template_param "${superclass_template_param}")
      string(REPLACE ">" "" superclass_template_param "${superclass_template_param}")
      string(REPLACE "," "" superclass_template_param "${superclass_template_param}")
#      set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(${swig_name}_Superclass) std::vector< ${superclass_template_param} >;\n")
#      ADD_RUBY_CONFIG_TEMPLATE("vector" "std::vector" "${swig_name}_Superclass" "${param}")
    endif()
  endif()

  if("${cpp_name}" STREQUAL "itk::MapContainer" AND NOT "${swig_name}" MATCHES "Pointer$")
    # add a template definition for the superclass which is not in ITK
    string(REGEX REPLACE "^[^,]+, *(.+) *$" "\\1" superclass_template_param "${template_params}")
    if("${superclass_template_param}" MATCHES "::")
      set(param "${superclass_template_param}")
      string(REPLACE "::" "" superclass_template_param "${superclass_template_param}")
      string(REPLACE "unsigned" "U" superclass_template_param "${superclass_template_param}")
      string(REPLACE "signed" "S" superclass_template_param "${superclass_template_param}")
      string(REPLACE "char" "C" superclass_template_param "${superclass_template_param}")
      string(REPLACE "short" "S" superclass_template_param "${superclass_template_param}")
      string(REPLACE "long" "L" superclass_template_param "${superclass_template_param}")
      string(REPLACE "float" "F" superclass_template_param "${superclass_template_param}")
      string(REPLACE "double" "D" superclass_template_param "${superclass_template_param}")
      string(REPLACE " " "" superclass_template_param "${superclass_template_param}")
      string(REPLACE "<" "" superclass_template_param "${superclass_template_param}")
      string(REPLACE ">" "" superclass_template_param "${superclass_template_param}")
      string(REPLACE "," "" superclass_template_param "${superclass_template_param}")
#      set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(${swig_name}_Superclass) std::map< unsigned long, ${superclass_template_param}, std::less< unsigned long > >;\n")
#      ADD_RUBY_CONFIG_TEMPLATE("map" "std::map" "${swig_name}_Superclass" "unsigned long, ${param}")
    endif()
  endif()

  if("${swig_name}" STREQUAL "itkTransformBase")
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(list${swig_name}_Pointer) std::list< ${swig_name}_Pointer >;\n")
#    ADD_RUBY_CONFIG_TEMPLATE("list" "std::list" "list${swig_name}_Pointer" "itk::TransformBase")
  endif()

  if("${cpp_name}" STREQUAL "itk::SpatialObjectPoint")
    set(text "${ITK_WRAP_RUBY_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
    set(text "${text}\n\n")
    set(text "${text}%extend ${swig_name} {\n")
    set(text "${text}  std::string __str__() {\n")
    set(text "${text}    itk::OStringStream msg;\n")
    set(text "${text}    self->Print( msg );\n")
    set(text "${text}    return msg.str();\n")
    set(text "${text}  }\n")
    set(text "${text}}\n")
    set(text "${text}\n\n")
    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")
    ADD_RUBY_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif()

  if("${cpp_name}" STREQUAL "itk::ContourSpatialObjectPoint")
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
#    ADD_RUBY_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif()

  if("${cpp_name}" STREQUAL "itk::LineSpatialObjectPoint")
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
#    ADD_RUBY_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif()

  if("${cpp_name}" STREQUAL "itk::SurfaceSpatialObjectPoint")
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(vector${swig_name}) std::vector< ${swig_name} >;\n")
#    ADD_RUBY_CONFIG_TEMPLATE("vector" "std::vector" "vector${swig_name}" "${cpp_name}< ${template_params} >")
  endif()

  if("${cpp_name}" STREQUAL "itk::SpatialObject" AND NOT "${ext_def}" MATCHES "SmartPointer")
#    set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}%template(list${swig_name}_Pointer) std::list< ${swig_name}_Pointer >;\n")
#    ADD_RUBY_CONFIG_TEMPLATE("list" "std::list" "list${swig_name}_Pointer" "${cpp_name}< ${template_params} >")
  endif()

endmacro()




macro(ADD_RUBY_SEQ_TYPEMAP swig_name dim)
  set(text "\n\n")
  set(text "${text}%typemap(in) ${swig_name}& (${swig_name} itks) {\n")
  set(text "${text}  if ((SWIG_ConvertPtr($input,(void **)(&$1),$1_descriptor, 0)) == -1) {\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}    if (PySequence_Check($input) && PyObject_Length($input) == ${dim}) {\n")
  set(text "${text}      for (int i =0; i < ${dim}; i++) {\n")
  set(text "${text}          PyObject *o = PySequence_GetItem($input,i);\n")
  set(text "${text}          if (!PyInt_Check(o)) {\n")
  set(text "${text}            PyErr_SetString(PyExc_ValueError,\"Expecting a sequence of int\");\n")
  set(text "${text}            return NULL;\n")
  set(text "${text}          }\n")
  set(text "${text}          itks[i] = PyInt_AsLong(o);\n")
  set(text "${text}      }\n")
  set(text "${text}      $1 = &itks;\n")
  set(text "${text}    }else if (PyInt_Check($input)) {\n")
  set(text "${text}      for (int i =0; i < ${dim}; i++) {\n")
  set(text "${text}          itks[i] = PyInt_AsLong($input);\n")
  set(text "${text}      }\n")
  set(text "${text}      $1 = &itks;\n")
  set(text "${text}    } else {\n")
  set(text "${text}      PyErr_SetString(PyExc_TypeError,\"Expecting an ${swig_name}, an int or sequence of int\");\n")
  set(text "${text}      SWIG_fail;\n")
  set(text "${text}    }\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%typemap(typecheck) ${swig_name}& {\n")
  set(text "${text}  void *ptr;\n")
  set(text "${text}  if (SWIG_ConvertPtr($input, &ptr, $1_descriptor, 0) == -1\n")
  set(text "${text}      && ( !PySequence_Check($input) || PyObject_Length($input) != ${dim} )\n")
  set(text "${text}      && !PyInt_Check($input) ) {\n")
  set(text "${text}    _v = 0;\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}  } else {\n")
  set(text "${text}    _v = 1;\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%typemap(in) ${swig_name} (${swig_name} itks) {\n")
  set(text "${text}  ${swig_name} * s;\n")
  set(text "${text}  if ((SWIG_ConvertPtr($input,(void **)(&s),$descriptor(${swig_name}*), 0)) == -1) {\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}    if (PySequence_Check($input) && PyObject_Length($input) == ${dim}) {\n")
  set(text "${text}      for (int i =0; i < ${dim}; i++) {\n")
  set(text "${text}          PyObject *o = PySequence_GetItem($input,i);\n")
  set(text "${text}          if (!PyInt_Check(o)) {\n")
  set(text "${text}            PyErr_SetString(PyExc_ValueError,\"Expecting a sequence of int\");\n")
  set(text "${text}            return NULL;\n")
  set(text "${text}          }\n")
  set(text "${text}         itks[i] = PyInt_AsLong(o);\n")
  set(text "${text}      }\n")
  set(text "${text}      $1 = itks;\n")
  set(text "${text}    }else if (PyInt_Check($input)) {\n")
  set(text "${text}      for (int i =0; i < ${dim}; i++) {\n")
  set(text "${text}          itks[i] = PyInt_AsLong($input);\n")
  set(text "${text}      }\n")
  set(text "${text}      $1 = itks;\n")
  set(text "${text}    } else {\n")
  set(text "${text}      PyErr_SetString(PyExc_TypeError,\"Expecting an ${swig_name}, an int or sequence of int\");\n")
  set(text "${text}      SWIG_fail;\n")
  set(text "${text}    }\n")
  set(text "${text}  } else if( s != NULL ) {\n")
  set(text "${text}    $1 = *s;\n")
  set(text "${text}  } else {\n")
  set(text "${text}    PyErr_SetString(PyExc_ValueError, \"Value can't be None\");\n")
  set(text "${text}    SWIG_fail;\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%typemap(typecheck) ${swig_name} {\n")
  set(text "${text}  void *ptr;\n")
  set(text "${text}  if (SWIG_ConvertPtr($input, &ptr, $descriptor(${swig_name}*), 0) == -1\n")
  set(text "${text}       && ( !PySequence_Check($input) || PyObject_Length($input) != ${dim} )\n")
  set(text "${text}       && !PyInt_Check($input) ) {\n")
  set(text "${text}    _v = 0;\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}  } else {\n")
  set(text "${text}    _v = 1;\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
#  set(text "${text}%{\n")
#  set(text "${text}#include <itkMacro.h>")
#  set(text "${text}%}\n")
  set(text "${text}%extend ${swig_name} {\n")
  set(text "${text}  long __getitem__(unsigned long dim) {\n")
  set(text "${text}    if (dim >= ${dim}) { throw std::out_of_range(\"${swig_name} index out of range.\"); }\n")
  set(text "${text}    return self->operator[]( dim );\n")
  set(text "${text}  }\n")
  set(text "${text}  void __setitem__(unsigned long dim, long int v) {\n")
  set(text "${text}    if (dim >= ${dim}) { throw std::out_of_range(\"${swig_name} index out of range.\"); }\n")
  set(text "${text}    self->operator[]( dim ) = v;\n")
  set(text "${text}  }\n")
  set(text "${text}  unsigned int __len__() {\n")
  set(text "${text}    return ${dim};\n")
  set(text "${text}  }\n")
  set(text "${text}  std::string __repr__() {\n")
  set(text "${text}    itk::OStringStream msg;\n")
  set(text "${text}    msg << \"${swig_name}(\" << *self << \")\";\n")
  set(text "${text}    return msg.str();\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}\n\n")

  set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")
endmacro()



macro(ADD_RUBY_VEC_TYPEMAP swig_name template_params)
  string(REGEX REPLACE "(.*),(.*)" "\\1" type "${template_params}")
  string(REGEX REPLACE "(.*),(.*)" "\\2" dim "${template_params}")

  set(text "\n\n")
  set(text "${text}%typemap(in) ${swig_name}& (${swig_name} itks) {\n")
  set(text "${text}  if ((SWIG_ConvertPtr($input,(void **)(&$1),$1_descriptor, 0)) == -1) {\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}    if (PySequence_Check($input) && PyObject_Length($input) == ${dim}) {\n")
  set(text "${text}      for (int i =0; i < ${dim}; i++) {\n")
  set(text "${text}          PyObject *o = PySequence_GetItem($input,i);\n")
  set(text "${text}          if (PyInt_Check(o)) {\n")
  set(text "${text}            itks[i] = PyInt_AsLong(o);\n")
  set(text "${text}          } else if (PyFloat_Check(o)) {\n")
  set(text "${text}            itks[i] = (${type})PyFloat_AsDouble(o);\n")
  set(text "${text}          } else {\n")
  set(text "${text}            PyErr_SetString(PyExc_ValueError,\"Expecting a sequence of int or float\");\n")
  set(text "${text}            return NULL;\n")
  set(text "${text}          }\n")
  set(text "${text}      }\n")
  set(text "${text}      $1 = &itks;\n")
  set(text "${text}    }else if (PyInt_Check($input)) {\n")
  set(text "${text}      for (int i =0; i < ${dim}; i++) {\n")
  set(text "${text}          itks[i] = PyInt_AsLong($input);\n")
  set(text "${text}      }\n")
  set(text "${text}      $1 = &itks;\n")
  set(text "${text}    }else if (PyFloat_Check($input)) {\n")
  set(text "${text}      for (int i =0; i < ${dim}; i++) {\n")
  set(text "${text}          itks[i] = (${type})PyFloat_AsDouble($input);\n")
  set(text "${text}      }\n")
  set(text "${text}      $1 = &itks;\n")
  set(text "${text}    } else {\n")
  set(text "${text}      PyErr_SetString(PyExc_TypeError,\"Expecting an ${swig_name}, an int, a float, a sequence of int or a sequence of float.\");\n")
  set(text "${text}      SWIG_fail;\n")
  set(text "${text}    }\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%typemap(typecheck) ${swig_name}& {\n")
  set(text "${text}  void *ptr;\n")
  set(text "${text}  if (SWIG_ConvertPtr($input, &ptr, $1_descriptor, 0) == -1\n")
  set(text "${text}      && ( !PySequence_Check($input) || PyObject_Length($input) != ${dim} )\n")
  set(text "${text}      && !PyInt_Check($input) && !PyFloat_Check($input) ) {\n")
  set(text "${text}    _v = 0;\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}  } else {\n")
  set(text "${text}    _v = 1;\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%typemap(in) ${swig_name} (${swig_name} itks) {\n")
  set(text "${text}  ${swig_name} * s;\n")
  set(text "${text}  if ((SWIG_ConvertPtr($input,(void **)(&s),$descriptor(${swig_name}*), 0)) == -1) {\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}    if (PySequence_Check($input) && PyObject_Length($input) == ${dim}) {\n")
  set(text "${text}      for (int i =0; i < ${dim}; i++) {\n")
  set(text "${text}          PyObject *o = PySequence_GetItem($input,i);\n")
  set(text "${text}          if (PyInt_Check(o)) {\n")
  set(text "${text}            itks[i] = PyInt_AsLong(o);\n")
  set(text "${text}          } else if (PyFloat_Check(o)) {\n")
  set(text "${text}            itks[i] = (${type})PyFloat_AsDouble(o);\n")
  set(text "${text}          } else {\n")
  set(text "${text}            PyErr_SetString(PyExc_ValueError,\"Expecting a sequence of int or float\");\n")
  set(text "${text}            return NULL;\n")
  set(text "${text}          }\n")
  set(text "${text}      }\n")
  set(text "${text}      $1 = itks;\n")
  set(text "${text}    }else if (PyInt_Check($input)) {\n")
  set(text "${text}      for (int i =0; i < ${dim}; i++) {\n")
  set(text "${text}          itks[i] = PyInt_AsLong($input);\n")
  set(text "${text}      }\n")
  set(text "${text}      $1 = itks;\n")
  set(text "${text}    }else if (PyFloat_Check($input)) {\n")
  set(text "${text}      for (int i =0; i < ${dim}; i++) {\n")
  set(text "${text}          itks[i] = (${type})PyFloat_AsDouble($input);\n")
  set(text "${text}      }\n")
  set(text "${text}      $1 = itks;\n")
  set(text "${text}    } else {\n")
  set(text "${text}      PyErr_SetString(PyExc_TypeError,\"Expecting an ${swig_name}, an int, a float, a sequence of int or a sequence of float.\");\n")
  set(text "${text}      SWIG_fail;\n")
  set(text "${text}    }\n")
  set(text "${text}  } else if( s != NULL ) {\n")
  set(text "${text}    $1 = *s;\n")
  set(text "${text} } else {\n")
  set(text "${text}   PyErr_SetString(PyExc_ValueError, \"Value can't be None\");\n")
  set(text "${text}   SWIG_fail;\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%typemap(typecheck) ${swig_name} {\n")
  set(text "${text}  void *ptr;\n")
  set(text "${text}  if (SWIG_ConvertPtr($input, &ptr, $descriptor(${swig_name}*), 0) == -1\n")
  set(text "${text}       && ( !PySequence_Check($input) || PyObject_Length($input) != ${dim} )\n")
  set(text "${text}      && !PyInt_Check($input) && !PyFloat_Check($input) ) {\n")
  set(text "${text}    _v = 0;\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}  } else {\n")
  set(text "${text}    _v = 1;\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%extend ${swig_name} {\n")
  set(text "${text}  ${type} __getitem__(unsigned long dim) {\n")
  set(text "${text}    if (dim >= ${dim}) { throw std::out_of_range(\"${swig_name} index out of range.\"); }\n")
  set(text "${text}    return self->operator[]( dim );\n")
  set(text "${text}  }\n")
  set(text "${text}  void __setitem__(unsigned long dim, ${type} v) {\n")
  set(text "${text}    if (dim >= ${dim}) { throw std::out_of_range(\"${swig_name} index out of range.\"); }\n")
  set(text "${text}    self->operator[]( dim ) = v;\n")
  set(text "${text}  }\n")
  set(text "${text}  unsigned int __len__() {\n")
  set(text "${text}    return ${dim};\n")
  set(text "${text}  }\n")
  set(text "${text}  std::string __repr__() {\n")
  set(text "${text}    itk::OStringStream msg;\n")
  set(text "${text}    msg << \"${swig_name}(\" << *self << \")\";\n")
  set(text "${text}    return msg.str();\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}\n\n")

  set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")
endmacro()

macro(ADD_RUBY_VARIABLE_LENGHT_SEQ_TYPEMAP type value_type)
  set(text "\n\n")
  set(text "${text}%typemap(in) ${type}& (${type} itks) {\n")
  set(text "${text}  if ((SWIG_ConvertPtr($input,(void **)(&$1),$1_descriptor, 0)) == -1) {\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}    itks = ${type}( PyObject_Length($input) );\n")
  set(text "${text}    for (unsigned int i =0; i < itks.Size(); i++) {\n")
  set(text "${text}      PyObject *o = PySequence_GetItem($input,i);\n")
  set(text "${text}      if (PyInt_Check(o)) {\n")
  set(text "${text}        itks[i] = (${value_type})PyInt_AsLong(o);\n")
  set(text "${text}      } else if (PyFloat_Check(o)) {\n")
  set(text "${text}        itks[i] = (${value_type})PyFloat_AsDouble(o);\n")
  set(text "${text}      } else {\n")
  set(text "${text}        PyErr_SetString(PyExc_ValueError,\"Expecting a sequence of int or float\");\n")
  set(text "${text}        return NULL;\n")
  set(text "${text}      }\n")
  set(text "${text}    }\n")
  set(text "${text}    $1 = &itks;\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%typemap(typecheck) ${type}& {\n")
  set(text "${text}  void *ptr;\n")
  set(text "${text}  if (SWIG_ConvertPtr($input, &ptr, $1_descriptor, 0) == -1\n")
  set(text "${text}      && !PySequence_Check($input) ) {\n")
  set(text "${text}    _v = 0;\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}  } else {\n")
  set(text "${text}    _v = 1;\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%typemap(in) ${type} (${type} itks) {\n")
  set(text "${text}  ${type} * s;\n")
  set(text "${text}  if ((SWIG_ConvertPtr($input,(void **)(&s),$descriptor(${type}*), 0)) == -1) {\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}    itks = ${type}( PyObject_Length($input) );\n")
  set(text "${text}    for (unsigned int i =0; i < itks.Size(); i++) {\n")
  set(text "${text}      PyObject *o = PySequence_GetItem($input,i);\n")
  set(text "${text}      if (PyInt_Check(o)) {\n")
  set(text "${text}        itks[i] = (${value_type})PyInt_AsLong(o);\n")
  set(text "${text}      } else if (PyFloat_Check(o)) {\n")
  set(text "${text}        itks[i] = (${value_type})PyFloat_AsDouble(o);\n")
  set(text "${text}      } else {\n")
  set(text "${text}        PyErr_SetString(PyExc_ValueError,\"Expecting a sequence of int or float\");\n")
  set(text "${text}        return NULL;\n")
  set(text "${text}      }\n")
  set(text "${text}    }\n")
  set(text "${text}    $1 = itks;\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%typemap(typecheck) ${type} {\n")
  set(text "${text}  void *ptr;\n")
  set(text "${text}  if (SWIG_ConvertPtr($input, &ptr, $descriptor(${type}*), 0) == -1\n")
  set(text "${text}      && !PySequence_Check($input) ) {\n")
  set(text "${text}    _v = 0;\n")
  set(text "${text}    PyErr_Clear();\n")
  set(text "${text}  } else {\n")
  set(text "${text}    _v = 1;\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}%extend ${type} {\n")
  set(text "${text}  ${value_type} __getitem__(unsigned long dim) {\n")
  set(text "${text}    if (dim >= self->Size()) { throw std::out_of_range(\"${type} index out of range.\"); }\n")
  set(text "${text}    return self->operator[]( dim );\n")
  set(text "${text}  }\n")
  set(text "${text}  void __setitem__(unsigned long dim, ${value_type} v) {\n")
  set(text "${text}    if (dim >= self->Size()) { throw std::out_of_range(\"${type} index out of range.\"); }\n")
  set(text "${text}    self->operator[]( dim ) = v;\n")
  set(text "${text}  }\n")
  set(text "${text}  unsigned int __len__() {\n")
  set(text "${text}    return self->Size();\n")
  set(text "${text}  }\n")
  set(text "${text}  std::string __repr__() {\n")
  set(text "${text}    itk::OStringStream msg;\n")
  set(text "${text}    msg << \"${swig_name}(\" << *self << \")\";\n")
  set(text "${text}    return msg.str();\n")
  set(text "${text}  }\n")
  set(text "${text}}\n")
  set(text "${text}\n\n")

  set(ITK_WRAP_RUBY_SWIG_EXT "${ITK_WRAP_RUBY_SWIG_EXT}${text}")
endmacro()


macro(ADD_RUBY_POINTER_TYPEMAP template_params)
  set(text "DECLARE_REF_COUNT_CLASS(${template_params})\n")

# set(text "\n\n")
# set(text "${text} // Ruby typemaps for Smart Pointers to ${template_params} class. \n\n")
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

  set(ITK_WRAP_RUBY_SWIG_EXT "${text}${ITK_WRAP_RUBY_SWIG_EXT}")
endmacro()


###############################################################################
# Create the RbUtils library

if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  add_subdirectory(Tests)

  macro(itk_end_wrap_modules_ruby)
    add_subdirectory(${ITK_WRAP_RUBY_SOURCE_DIR}/RbUtils)
  endmacro()

  macro(itk_wrap_modules_ruby)
    add_subdirectory(${ITK_WRAP_RUBY_SOURCE_DIR}/RbBase)
  endmacro()

else()
  macro(itk_end_wrap_modules_ruby)
    # just do nothing
  endmacro()

  macro(itk_wrap_modules_ruby)
    # just do nothing
  endmacro()

endif()

macro( itk_module_doxygen _name )

  set( _content "/**\n" )
  set( _content "${_content} \\defgroup ${_name} ${_name} \n" )
  set( _content "${_content} ${ITK_MODULE_${_name}_DESCRIPTION} \n" )

  foreach( d ${ITK_MODULE_${_name}_DEPENDS} )
    set( _content "${_content} \\sa ${d} \n" )
  endforeach()

  set( _content "${_content} */\n" )

  configure_file(
    "${ITK_SOURCE_DIR}/Utilities/Doxygen/Module.dox.in"
    "${ITK_BINARY_DIR}/Utilities/Doxygen/Modules/${_name}.dox"
    @ONLY
    )
endmacro()

WRAP_LIBRARY("ITKPyBase" Python)

  set(WRAPPER_LIBRARY_DEPENDS )
  set(WRAPPER_LIBRARY_LINK_LIBRARIES )

  set(WRAPPER_LIBRARY_SWIG_INPUTS "${CMAKE_CURRENT_SOURCE_DIR}/pyBase.i")

  # no wrap_*.cmake file to include, no need to run that
  # AUTO_INCLUDE_MODULES()

  # add the vars defined by including std_iostream and std_stream to the python config
  if("${SWIG_VERSION}" STRGREATER "1.3.36")
    ADD_PYTHON_CONFIG_TEMPLATE("SwigPyIterator" "SwigPyIterator" "SwigPyIterator" "")
    ADD_PYTHON_CONFIG_TEMPLATE("PySwigIterator" "SwigPyIterator" "SwigPyIterator" "")
  else("${SWIG_VERSION}" STRGREATER "1.3.36")
    ADD_PYTHON_CONFIG_TEMPLATE("SwigPyIterator" "PySwigIterator" "PySwigIterator" "")
    ADD_PYTHON_CONFIG_TEMPLATE("PySwigIterator" "PySwigIterator" "PySwigIterator" "")
  endif("${SWIG_VERSION}" STRGREATER "1.3.36")
  ADD_PYTHON_CONFIG_TEMPLATE("cerr" "std::cerr" "cerr" "")
  ADD_PYTHON_CONFIG_TEMPLATE("cin" "std::cin" "cin" "")
  ADD_PYTHON_CONFIG_TEMPLATE("clog" "std::clog" "clog" "")
  ADD_PYTHON_CONFIG_TEMPLATE("cout" "std::cout" "cout" "")
  ADD_PYTHON_CONFIG_TEMPLATE("cvar" "std::cvar" "cvar" "")
  ADD_PYTHON_CONFIG_TEMPLATE("endl" "std::endl" "endl" "")
  ADD_PYTHON_CONFIG_TEMPLATE("ends" "std::ends" "ends" "")
  ADD_PYTHON_CONFIG_TEMPLATE("flush" "std::flush" "flush" "")
  ADD_PYTHON_CONFIG_TEMPLATE("ios" "std::ios" "ios" "")
  ADD_PYTHON_CONFIG_TEMPLATE("ios_base" "std::ios_base" "ios_base" "")
  ADD_PYTHON_CONFIG_TEMPLATE("ios_base_sync_with_stdio" "std::ios_base_sync_with_stdio" "ios_base_sync_with_stdio" "")
  ADD_PYTHON_CONFIG_TEMPLATE("ios_base_xalloc" "std::ios_base_xalloc" "ios_base_xalloc" "")
  ADD_PYTHON_CONFIG_TEMPLATE("iostream" "std::iostream" "iostream" "")
  ADD_PYTHON_CONFIG_TEMPLATE("istream" "std::istream" "istream" "")
#  ADD_PYTHON_CONFIG_TEMPLATE("istringstream" "std::istringstream" "istringstream" "")
  ADD_PYTHON_CONFIG_TEMPLATE("ostream" "std::ostream" "ostream" "")
#  ADD_PYTHON_CONFIG_TEMPLATE("ostringstream" "std::ostringstream" "ostringstream" "")
#  ADD_PYTHON_CONFIG_TEMPLATE("streambuf" "std::streambuf" "streambuf" "")
#  ADD_PYTHON_CONFIG_TEMPLATE("string" "std::string" "string" "")
#  ADD_PYTHON_CONFIG_TEMPLATE("stringstream" "std::stringstream" "stringstream" "")

# TODO: readd the bool (B) once supported by sunstudio
#  foreach(t B UC US UL SC SS SL F D)
  foreach(t UC US UL SC SS SL F D)
    ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "map${t}${t}" "${ITKT_${t}}, ${ITKT_${t}}")
    ADD_PYTHON_CONFIG_TEMPLATE("list" "std::list" "list${t}" "${ITKT_${t}}")
    ADD_PYTHON_CONFIG_TEMPLATE("set" "std::set" "set${t}" "${ITKT_${t}}")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vector${t}" "${ITKT_${t}}")
    ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectorvector${t}" "std::vector< ${ITKT_${t}} >")
  endforeach(t)

  ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "mapULD" "unsigned long, double")
  ADD_PYTHON_CONFIG_TEMPLATE("vector" "std::vector" "vectorsetUL" "std::set< unsigned long >")
  ADD_PYTHON_CONFIG_TEMPLATE("map" "std::map" "mapsetUL" "unsigned long, std::set< unsigned long >")

END_WRAP_LIBRARY()

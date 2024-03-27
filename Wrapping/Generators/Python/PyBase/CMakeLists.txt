itk_wrap_module("ITKPyBase" Python)

set(WRAPPER_LIBRARY_DEPENDS)
set(WRAPPER_LIBRARY_LINK_LIBRARIES)

set(WRAPPER_LIBRARY_SWIG_INPUTS "${CMAKE_CURRENT_SOURCE_DIR}/pyBase.i")

add_python_config_template(
  "cerr"
  "std::cerr"
  "cerr"
  "")
add_python_config_template(
  "cin"
  "std::cin"
  "cin"
  "")
add_python_config_template(
  "clog"
  "std::clog"
  "clog"
  "")
add_python_config_template(
  "cout"
  "std::cout"
  "cout"
  "")
add_python_config_template(
  "cvar"
  "std::cvar"
  "cvar"
  "")
add_python_config_template(
  "endl"
  "std::endl"
  "endl"
  "")
add_python_config_template(
  "ends"
  "std::ends"
  "ends"
  "")
add_python_config_template(
  "flush"
  "std::flush"
  "flush"
  "")
add_python_config_template(
  "ios"
  "std::ios"
  "ios"
  "")
add_python_config_template(
  "ios_base"
  "std::ios_base"
  "ios_base"
  "")
add_python_config_template(
  "iostream"
  "std::iostream"
  "iostream"
  "")
add_python_config_template(
  "istream"
  "std::istream"
  "istream"
  "")
#  ADD_PYTHON_CONFIG_TEMPLATE("istringstream" "std::istringstream" "istringstream" "")
add_python_config_template(
  "ostream"
  "std::ostream"
  "ostream"
  "")
#  ADD_PYTHON_CONFIG_TEMPLATE("ostringstream" "std::ostringstream" "ostringstream" "")
#  ADD_PYTHON_CONFIG_TEMPLATE("streambuf" "std::streambuf" "streambuf" "")
add_python_config_template(
  "string"
  "std::string"
  "str"
  "")
#  ADD_PYTHON_CONFIG_TEMPLATE("stringstream" "std::stringstream" "stringstream" "")

foreach(
  t
  B
  UC
  US
  UL
  SC
  SS
  SL
  F
  D)
  add_python_config_template(
    "map"
    "std::map"
    "map${t}${t}"
    "${ITKT_${t}}, ${ITKT_${t}}")
  add_python_config_template(
    "list"
    "std::list"
    "list${t}"
    "${ITKT_${t}}")
  add_python_config_template(
    "set"
    "std::set"
    "set${t}"
    "${ITKT_${t}}")
  add_python_config_template(
    "vector"
    "std::vector"
    "vector${t}"
    "${ITKT_${t}}")
  add_python_config_template(
    "vector"
    "std::vector"
    "vectorvector${t}"
    "std::vector< ${ITKT_${t}} >")
endforeach()
foreach(t ULL SLL)
  add_python_config_template(
    "set"
    "std::set"
    "set${t}"
    "${ITKT_${t}}")
endforeach()

add_python_config_template(
  "map"
  "std::map"
  "mapULD"
  "unsigned long, double")
add_python_config_template(
  "vector"
  "std::vector"
  "vectorsetUL"
  "std::set< unsigned long >")
add_python_config_template(
  "map"
  "std::map"
  "mapsetUL"
  "unsigned long, std::set< unsigned long >")

itk_end_wrap_module()

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

macro(CREATE_SWIG_RUNTIME lang_option lang_postfix lang_link_libs)
  set(source_file "${CMAKE_CURRENT_BINARY_DIR}/swigrun${lang_postfix}.cxx")
  set(lib_name "SwigRuntime${lang_postfix}")

  add_library(${lib_name} SHARED "${source_file}")
  target_link_libraries(${lib_name} "${lang_link_libs}")

  # work around linkage problem on some solaris systems
  if(CMAKE_SYSTEM MATCHES "SunOS-." AND CMAKE_COMPILER_IS_GNUCXX AND CMAKE_COMPILER_IS_GNUCC)
    target_link_libraries(${lib_name} stdc++)
  endif(CMAKE_SYSTEM MATCHES "SunOS-." AND CMAKE_COMPILER_IS_GNUCXX AND CMAKE_COMPILER_IS_GNUCC)

  install_targets("${WRAP_ITK_INSTALL_PREFIX}/lib" ${lib_name})
  set_target_properties(${lib_name} PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")

  add_custom_command(
   SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/swigrun.h
   COMMAND ${GCCXML}
   ARGS -fxml-start=_cable_ -DCABLE_CONFIGURATION ${CMAKE_CURRENT_SOURCE_DIR}/swigrun.h
      -fxml=${CMAKE_CURRENT_BINARY_DIR}/swigrun.xml
   TARGET ${lib_name}
   OUTPUTS ${CMAKE_CURRENT_BINARY_DIR}/swigrun.xml
   DEPENDS ${GCCXML})

  add_custom_command(
    SOURCE ${CMAKE_CURRENT_BINARY_DIR}/swigrun.xml
    COMMAND ${CSWIG}
    ARGS -o "${source_file}"
         ${lang_option}
         -c++ ${CMAKE_CURRENT_BINARY_DIR}/swigrun.xml
    TARGET ${lib_name}
    OUTPUTS "${source_file}"
    DEPENDS ${CSWIG})
endmacro(CREATE_SWIG_RUNTIME)

if(WRAP_ITK_TCL)
   CREATE_SWIG_RUNTIME(-tcl Tcl "${TCL_LIBRARY}")
endif(WRAP_ITK_TCL)

if(WRAP_ITK_PYTHON)
   CREATE_SWIG_RUNTIME(-python Python "${PYTHON_LIBRARY}")
endif(WRAP_ITK_PYTHON)

if(WRAP_ITK_PERL)
   CREATE_SWIG_RUNTIME(-perl5 Perl "${PERL_LIBRARY}")
endif(WRAP_ITK_PERL)

# java seems to not share the code with the other here, because of the JavaCWD class
# if(WRAP_ITK_JAVA)
#    CREATE_SWIG_RUNTIME(-java Java "${JAVA_LIBRARY}")
# endif(WRAP_ITK_JAVA)
#


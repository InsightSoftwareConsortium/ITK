INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

MACRO(CREATE_SWIG_RUNTIME lang_option lang_postfix lang_link_libs)
  SET(source_file "${CMAKE_CURRENT_BINARY_DIR}/swigrun${lang_postfix}.cxx")
  SET(lib_name "SwigRuntime${lang_postfix}")
  
  ADD_LIBRARY(${lib_name} SHARED "${source_file}")
  TARGET_LINK_LIBRARIES(${lib_name} "${lang_link_libs}")
  INSTALL_TARGETS("${WRAP_ITK_INSTALL_PREFIX}/lib" ${lib_name})
  SET_TARGET_PROPERTIES(${lib_name} PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")

  ADD_CUSTOM_COMMAND(
   SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/swigrun.h
   COMMAND ${GCCXML}
   ARGS -fxml-start=_cable_ -DCABLE_CONFIGURATION ${CMAKE_CURRENT_SOURCE_DIR}/swigrun.h
      -fxml=${CMAKE_CURRENT_BINARY_DIR}/swigrun.xml 
   TARGET ${lib_name}
   OUTPUTS ${CMAKE_CURRENT_BINARY_DIR}/swigrun.xml
   DEPENDS ${GCCXML})

  ADD_CUSTOM_COMMAND(
    SOURCE ${CMAKE_CURRENT_BINARY_DIR}/swigrun.xml
    COMMAND ${CSWIG}
    ARGS -o "${source_file}"
         ${lang_option}
         -c++ ${CMAKE_CURRENT_BINARY_DIR}/swigrun.xml
    TARGET ${lib_name}
    OUTPUTS "${source_file}"
    DEPENDS ${CSWIG})
ENDMACRO(CREATE_SWIG_RUNTIME)

IF(WRAP_ITK_TCL)
   CREATE_SWIG_RUNTIME(-tcl Tcl "${TCL_LIBRARY}")
ENDIF(WRAP_ITK_TCL)

IF(WRAP_ITK_PYTHON)
   CREATE_SWIG_RUNTIME(-python Python "${PYTHON_LIBRARY}")
ENDIF(WRAP_ITK_PYTHON)

IF(WRAP_ITK_PERL)
   CREATE_SWIG_RUNTIME(-perl5 Perl "${PERL_LIBRARY}")
ENDIF(WRAP_ITK_PERL)

# java seems to not share the code with the other here, because of the JavaCWD class
# IF(WRAP_ITK_JAVA)
#    CREATE_SWIG_RUNTIME(-java Java "${JAVA_LIBRARY}")
# ENDIF(WRAP_ITK_JAVA)
# 


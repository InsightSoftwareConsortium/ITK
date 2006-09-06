
SUBDIRS(Tests)

###############################################################################
# Add the ITKTclSupport library which contains the itkTclCommand class.

BEGIN_WRAPPER_LIBRARY("ITKTclSupport" Tcl)

SET(WRAPPER_LIBRARY_DEPENDS Base)
SET(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon)
# Need to include the current dir because the compiler needs to find itkTclCommand.h
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
SET(WRAPPER_LIBRARY_CXX_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/itkTclCommand.cxx") 

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()

###############################################################################
# Configure pkgIndex.tcl for the build tree.
SET(WRAP_ITK_DATA_ROOT "${WrapITK_BINARY_DIR}/Data")
SET(WRAP_ITK_TEST_ROOT "${WrapITK_BINARY_DIR}/Testing")
SET(WRAP_ITK_SCRIPT_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

IF(CMAKE_CONFIGURATION_TYPES)
  FOREACH(config ${CMAKE_CONFIGURATION_TYPES})
    SET(WRAP_ITK_PACKAGE_DIR "${LIBRARY_OUTPUT_PATH}/${config}")
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/pkgIndex.tcl.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/${config}/pkgIndex.tcl"
                   @ONLY IMMEDIATE)
  ENDFOREACH(config)
ELSE(CMAKE_CONFIGURATION_TYPES)
  SET(WRAP_ITK_PACKAGE_DIR "${LIBRARY_OUTPUT_PATH}")
  CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/pkgIndex.tcl.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/pkgIndex.tcl"
                 @ONLY IMMEDIATE)
ENDIF(CMAKE_CONFIGURATION_TYPES)

# Configure pkgIndex.tcl for the install tree.
SET(WRAP_ITK_SCRIPT_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/Tcl")
SET(WRAP_ITK_PACKAGE_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/lib")
SET(WRAP_ITK_DATA_ROOT "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/Data")
SET(WRAP_ITK_TEST_ROOT "<NO_DEFAULT>")
CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/pkgIndex.tcl.in"
               "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/pkgIndex.tcl"
               @ONLY IMMEDIATE)
WRAP_ITK_INSTALL(/Tcl "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/pkgIndex.tcl")

SET(ITK_TCL_EXE_DIR_BUILD "${ITK_EXECUTABLE_PATH}")
SET(ITK_TCL_EXE_NAME_ROOT "itkwish")

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/itkTclConfigure.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/itkTclConfigure.h
               @ONLY IMMEDIATE)

# Configure itkTclModules.h with the names of the initializer functions for the
# selected modules. For some pathological reason, the names are in the format:
# Modulename_Init
# where the first character is uppercase and the rest of the module name is 
# lower case. Which will entail some amount of pain to create in CMake.
SET(module_init_names )
FOREACH(module ${WRAP_ITK_MODULES})
  STRING(REGEX REPLACE "^(.)(.+)$" "\\1" first "${module}")
  STRING(REGEX REPLACE "^(.)(.+)$" "\\2" rest "${module}")
  STRING(TOUPPER "${first}" first)
  STRING(TOLOWER "${rest}" rest)
  SET(module_init_names ${module_init_names} "${first}${rest}tcl_Init")
ENDFOREACH(module)

# Now take those init functions and make a list of symbol declarations like:
# int Modulename_Init(Tcl_Interp*);
SET(CONFIG_MODULE_INIT_DECLARATIONS )
FOREACH(module ${module_init_names})
  SET(CONFIG_MODULE_INIT_DECLARATIONS "${CONFIG_MODULE_INIT_DECLARATIONS}  int ${module}(Tcl_Interp*);\n")
ENDFOREACH(module)

# Now we pack the addresses of the init function names into a comma-separated 
# string, to be used in an array initializer
SET(initializer )
FOREACH(module ${module_init_names})
  SET(initializer "${initializer}  &${module},\n")
ENDFOREACH(module)
# Let's trim off that last ',\n':
STRING(REGEX REPLACE ",\n$" "" CONFIG_MODULE_INIT_ARRAY_INITIALIZER "${initializer}")

# Let's get the number of modules configured. This is an amazing! pain!
# because there's no list-length function in CMake 2.2. So we use the string
# length function instead. 
SET(counter )
FOREACH(module ${WRAP_ITK_MODULES})
  SET(counter "${counter}.")
ENDFOREACH(module)
STRING(LENGTH "${counter}" CONFIG_MODULE_COUNT)

# And finally configure the file
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/itkTclModules.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/itkTclModules.h
               @ONLY IMMEDIATE)


INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})
ADD_EXECUTABLE(itkwish itkTclAppInit.cxx)
FOREACH(module ${WRAP_ITK_MODULES})
  TARGET_LINK_LIBRARIES(itkwish ${module}Tcl)
ENDFOREACH(module)

TARGET_LINK_LIBRARIES(itkwish ${TCL_LIBRARY} ${TK_LIBRARY})

# configure and install the driver itkwish script
SET(CONFIG_WRAP_ITK_TCL_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/Tcl")
SET(CONFIG_WRAP_ITK_TCL_SWIG_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/lib")
SET(CONFIG_WRAP_ITK_ITKWISH_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/bin")
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/itkwish.in
               ${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/itkwish
               @ONLY IMMEDIATE )
INSTALL_FILES(/bin FILES ${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/itkwish)

# configure a itkwish usable in build tree
# use WrapITK_BINARY_DIR instead of the current bianry dir because tcl search pkgIndex.tcl in 
# the subdirs before searching in the current dir, and so find the pikIndex.tcl file in InstallOnly
# first
SET(CONFIG_WRAP_ITK_TCL_DIR "${WrapITK_BINARY_DIR}")
SET(CONFIG_WRAP_ITK_TCL_SWIG_DIR "${LIBRARY_OUTPUT_PATH}")
SET(CONFIG_WRAP_ITK_ITKWISH_DIR "${EXECUTABLE_OUTPUT_PATH}")
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/itkwish.in
               ${CMAKE_CURRENT_BINARY_DIR}/itkwish
               @ONLY IMMEDIATE )


#install the actual executable
INSTALL_TARGETS("${WRAP_ITK_INSTALL_PREFIX}/bin" itkwish)
INSTALL_FILES("${WRAP_ITK_INSTALL_PREFIX}/Tcl" .tcl itkinteraction itktesting itkdata itkutils)


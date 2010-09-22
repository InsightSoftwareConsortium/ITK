
add_subdirectory(Tests)

###############################################################################
# Add the ITKTclSupport library which contains the itkTclCommand class.

BEGIN_WRAPPER_LIBRARY("ITKTclSupport" Tcl)

set(WRAPPER_LIBRARY_DEPENDS Base)
set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon)
# Need to include the current dir because the compiler needs to find itkTclCommand.h
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
set(WRAPPER_LIBRARY_CXX_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/itkTclCommand.cxx")

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()

###############################################################################
# Configure pkgIndex.tcl for the build tree.
set(WRAP_ITK_DATA_ROOT "${WrapITK_BINARY_DIR}/Data")
set(WRAP_ITK_TEST_ROOT "${WrapITK_BINARY_DIR}/Testing")
set(WRAP_ITK_SCRIPT_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

if(CMAKE_CONFIGURATION_TYPES)
  foreach(config ${CMAKE_CONFIGURATION_TYPES})
    set(WRAP_ITK_PACKAGE_DIR "${LIBRARY_OUTPUT_PATH}/${config}")
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/pkgIndex.tcl.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/${config}/pkgIndex.tcl"
                   @ONLY IMMEDIATE)
  endforeach(config)
else(CMAKE_CONFIGURATION_TYPES)
  set(WRAP_ITK_PACKAGE_DIR "${LIBRARY_OUTPUT_PATH}")
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/pkgIndex.tcl.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/pkgIndex.tcl"
                 @ONLY IMMEDIATE)
endif(CMAKE_CONFIGURATION_TYPES)

# Configure pkgIndex.tcl for the install tree.
set(WRAP_ITK_SCRIPT_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/Tcl")
set(WRAP_ITK_PACKAGE_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/lib")
set(WRAP_ITK_DATA_ROOT "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/Data")
set(WRAP_ITK_TEST_ROOT "<NO_DEFAULT>")
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/pkgIndex.tcl.in"
               "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/pkgIndex.tcl"
               @ONLY IMMEDIATE)
WRAP_ITK_INSTALL(/Tcl "${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/pkgIndex.tcl")

set(ITK_TCL_EXE_DIR_BUILD "${ITK_EXECUTABLE_PATH}")
set(ITK_TCL_EXE_NAME_ROOT "itkwish")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/itkTclConfigure.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/itkTclConfigure.h
               @ONLY IMMEDIATE)

# Configure itkTclModules.h with the names of the initializer functions for the
# selected modules. For some pathological reason, the names are in the format:
# Modulename_Init
# where the first character is uppercase and the rest of the module name is
# lower case. Which will entail some amount of pain to create in CMake.
set(module_init_names )
foreach(module ${WRAP_ITK_MODULES})
  string(REGEX REPLACE "^(.)(.+)$" "\\1" first "${module}")
  string(REGEX REPLACE "^(.)(.+)$" "\\2" rest "${module}")
  string(TOUPPER "${first}" first)
  string(TOLOWER "${rest}" rest)
  set(module_init_names ${module_init_names} "${first}${rest}tcl_Init")
endforeach(module)

# Now take those init functions and make a list of symbol declarations like:
# int Modulename_Init(Tcl_Interp*);
set(CONFIG_MODULE_INIT_DECLARATIONS )
foreach(module ${module_init_names})
  set(CONFIG_MODULE_INIT_DECLARATIONS "${CONFIG_MODULE_INIT_DECLARATIONS}  int ${module}(Tcl_Interp*);\n")
endforeach(module)

# Now we pack the addresses of the init function names into a comma-separated
# string, to be used in an array initializer
set(initializer )
foreach(module ${module_init_names})
  set(initializer "${initializer}  &${module},\n")
endforeach(module)
# Let's trim off that last ',\n':
string(REGEX REPLACE ",\n$" "" CONFIG_MODULE_INIT_ARRAY_INITIALIZER "${initializer}")

# Let's get the number of modules configured. This is an amazing! pain!
# because there's no list-length function in CMake 2.2. So we use the string
# length function instead.
set(counter )
foreach(module ${WRAP_ITK_MODULES})
  set(counter "${counter}.")
endforeach(module)
string(LENGTH "${counter}" CONFIG_MODULE_COUNT)

# And finally configure the file
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/itkTclModules.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/itkTclModules.h
               @ONLY IMMEDIATE)


include_directories(${CMAKE_CURRENT_BINARY_DIR})
add_executable(itkwish itkTclAppInit.cxx)
foreach(module ${WRAP_ITK_MODULES})
  target_link_libraries(itkwish ${module}Tcl)
endforeach(module)

target_link_libraries(itkwish ${TCL_LIBRARY} ${TK_LIBRARY})

# configure and install the driver itkwish script
set(CONFIG_WRAP_ITK_TCL_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/Tcl")
set(CONFIG_WRAP_ITK_TCL_SWIG_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/lib")
set(CONFIG_WRAP_ITK_ITKWISH_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}/bin")
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/itkwish.in
               ${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/itkwish
               @ONLY IMMEDIATE )
install_files(/bin FILES ${CMAKE_CURRENT_BINARY_DIR}/InstallOnly/itkwish)

# configure a itkwish usable in build tree
# use WrapITK_BINARY_DIR instead of the current bianry dir because tcl search pkgIndex.tcl in
# the subdirs before searching in the current dir, and so find the pikIndex.tcl file in InstallOnly
# first
set(CONFIG_WRAP_ITK_TCL_DIR "${WrapITK_BINARY_DIR}")
set(CONFIG_WRAP_ITK_TCL_SWIG_DIR "${LIBRARY_OUTPUT_PATH}")
set(CONFIG_WRAP_ITK_ITKWISH_DIR "${EXECUTABLE_OUTPUT_PATH}")
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/itkwish.in
               ${CMAKE_CURRENT_BINARY_DIR}/itkwish
               @ONLY IMMEDIATE )


#install the actual executable
install_targets("${WRAP_ITK_INSTALL_PREFIX}/bin" itkwish)
install_files("${WRAP_ITK_INSTALL_PREFIX}/Tcl" .tcl itkinteraction itktesting itkdata itkutils)


SET(ExplicitITK_SOURCE_DIR ${ITK_SOURCE_DIR}/Wrapping/ExplicitITK)

OPTION(EXPLICIT_unsigned_char "Wrap unsigned char type" ON)
OPTION(EXPLICIT_unsigned_short "Wrap unsigned short type" ON)
OPTION(EXPLICIT_unsigned_long "Wrap unsigned long type" ON)
OPTION(EXPLICIT_unsigned_int "Wrap unsigned int type" ON)

OPTION(EXPLICIT_signed_char "Wrap signed char type" OFF)
OPTION(EXPLICIT_signed_short "Wrap signed short type" OFF)
OPTION(EXPLICIT_signed_long "Wrap signed long type" OFF)
OPTION(EXPLICIT_signed_int "Wrap signed int type" ON)

OPTION(EXPLICIT_float "Wrap float type" ON)
OPTION(EXPLICIT_double "Wrap double type" ON)

OPTION(EXPLICIT_vector_float "Wrap vector float type" ON)
OPTION(EXPLICIT_vector_double "Wrap vector double type" OFF)

OPTION(EXPLICIT_covariant_vector_float "Wrap covariant vector float type" ON)
OPTION(EXPLICIT_covariant_vector_double "Wrap covariant vector double type" OFF)

OPTION(EXPLICIT_rgb_unsigned_char "Wrap RGB< unsigned char > type" ON)
OPTION(EXPLICIT_rgb_unsigned_short "Wrap RGB< unsigned short > type" OFF)

OPTION(EXPLICIT_complex_float "Wrap complex<float> type" OFF)
OPTION(EXPLICIT_complex_double "Wrap complex<double> type" OFF)

SET(EXPLICIT_ITK_DIMS "2;3" CACHE STRING "dimensions available separated by semicolons (;)")

# Output directories.
SET(EXPLICIT_ITK_CONFIG_DIR "${ExplicitITK_SOURCE_DIR}/ConfigurationInputs")
SET(EXPLICIT_ITK_CMAKE_DIR "${ExplicitITK_SOURCE_DIR}")

MACRO(BEGIN_WRAPPER_LIBRARY library_name wrap_directory)
  SET(WRAPPER_LIBRARY_NAME "${library_name}")

  # Mark the current source dir for inclusion because it may contain header files.
  INCLUDE_DIRECTORIES("${wrap_directory}")
  
  # WRAPPER_LIBRARY_SOURCE_DIR. Directory to be scanned for wrap_*.cmake files. 
  SET(WRAPPER_LIBRARY_SOURCE_DIR "${wrap_directory}")
  
  # WRAPPER_LIBRARY_OUTPUT_DIR. Directory in which generated cxx, xml, and idx
  # files will be placed. 
  SET(WRAPPER_LIBRARY_OUTPUT_DIR "${ITK_BINARY_DIR}/Code/${library_name}/Templates")

  # WRAPPER_LIBRARY_DEPENDS. List of names of other wrapper libraries that
  # define symbols used by this wrapper library.
  SET(WRAPPER_LIBRARY_DEPENDS )

  # WRAPPER_LIBRARY_LINK_LIBRARIES. List of other libraries that should
  # be linked to the wrapper library.
  SET(WRAPPER_LIBRARY_LINK_LIBRARIES )

  # WRAPPER_LIBRARY_GROUPS. List of wrap_*.cmake groups in the source dir
  # that should be included/wrapped before the rest. Just the group name is needed,
  # not the full path or file name. 
  SET(WRAPPER_LIBRARY_GROUPS )
ENDMACRO(BEGIN_WRAPPER_LIBRARY)



INCLUDE("${ExplicitITK_SOURCE_DIR}/CMakeUtilityFunctions.cmake")
INCLUDE("${ExplicitITK_SOURCE_DIR}/CreateCableSwigInputs.cmake")
INCLUDE("${ExplicitITK_SOURCE_DIR}/CreateExplicitInstantiations.cmake")
INCLUDE("${ExplicitITK_SOURCE_DIR}/WrapBasicTypes.cmake")
INCLUDE("${ExplicitITK_SOURCE_DIR}/WrapITKTypesExplicit.cmake")


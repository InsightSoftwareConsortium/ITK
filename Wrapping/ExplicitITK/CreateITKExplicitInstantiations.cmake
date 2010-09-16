set(ExplicitITK_SOURCE_DIR ${ITK_SOURCE_DIR}/Wrapping/ExplicitITK)

set(EXPLICIT_MANUAL 0)

if(EXPLICIT_MANUAL)
option(EXPLICIT_unsigned_char "Wrap unsigned char type" ON)
option(EXPLICIT_unsigned_short "Wrap unsigned short type" ON)
option(EXPLICIT_unsigned_long "Wrap unsigned long type" ON)
option(EXPLICIT_unsigned_int "Wrap unsigned int type" ON)

option(EXPLICIT_signed_char "Wrap signed char type" OFF)
option(EXPLICIT_signed_short "Wrap signed short type" OFF)
option(EXPLICIT_signed_long "Wrap signed long type" OFF)
option(EXPLICIT_signed_int "Wrap signed int type" ON)

option(EXPLICIT_float "Wrap float type" ON)
option(EXPLICIT_double "Wrap double type" ON)

option(EXPLICIT_vector_float "Wrap vector float type" ON)
option(EXPLICIT_vector_double "Wrap vector double type" OFF)

option(EXPLICIT_covariant_vector_float "Wrap covariant vector float type" ON)
option(EXPLICIT_covariant_vector_double "Wrap covariant vector double type" OFF)

option(EXPLICIT_rgb_unsigned_char "Wrap RGB< unsigned char > type" ON)
option(EXPLICIT_rgb_unsigned_short "Wrap RGB< unsigned short > type" OFF)

option(EXPLICIT_complex_float "Wrap complex<float> type" OFF)
option(EXPLICIT_complex_double "Wrap complex<double> type" OFF)

set(EXPLICIT_ITK_DIMS "2;3" CACHE STRING "dimensions available separated by semicolons (;)")

endif(EXPLICIT_MANUAL)

# Output directories.
set(EXPLICIT_ITK_CONFIG_DIR "${ExplicitITK_SOURCE_DIR}/ConfigurationInputs")
set(EXPLICIT_ITK_CMAKE_DIR "${ExplicitITK_SOURCE_DIR}")
set(EXPLICIT_ITK_NO_INCLUDES "")

macro(BEGIN_WRAPPER_LIBRARY library_name wrap_directory)
  set(WRAPPER_LIBRARY_NAME "${library_name}")

  # Mark the current source dir for inclusion because it may contain header files.
  include_directories("${wrap_directory}")

  # WRAPPER_LIBRARY_SOURCE_DIR. Directory to be scanned for wrap_*.cmake files.
  set(WRAPPER_LIBRARY_SOURCE_DIR "${wrap_directory}")

  # WRAPPER_LIBRARY_OUTPUT_DIR. Directory in which generated cxx, xml, and idx
  # files will be placed.
  set(WRAPPER_LIBRARY_OUTPUT_DIR "${ITK_BINARY_DIR}/Code/${library_name}/Templates")

  # WRAPPER_LIBRARY_DEPENDS. List of names of other wrapper libraries that
  # define symbols used by this wrapper library.
  set(WRAPPER_LIBRARY_DEPENDS )

  # WRAPPER_LIBRARY_LINK_LIBRARIES. List of other libraries that should
  # be linked to the wrapper library.
  set(WRAPPER_LIBRARY_LINK_LIBRARIES )

  # WRAPPER_LIBRARY_GROUPS. List of wrap_*.cmake groups in the source dir
  # that should be included/wrapped before the rest. Just the group name is needed,
  # not the full path or file name.
  set(WRAPPER_LIBRARY_GROUPS )
endmacro(BEGIN_WRAPPER_LIBRARY)



include("${ExplicitITK_SOURCE_DIR}/CMakeUtilityFunctions.cmake")
include("${ExplicitITK_SOURCE_DIR}/CreateCableSwigInputs.cmake")
include("${ExplicitITK_SOURCE_DIR}/CreateExplicitInstantiations.cmake")
include("${ExplicitITK_SOURCE_DIR}/WrapBasicTypes.cmake")
include("${ExplicitITK_SOURCE_DIR}/WrapITKTypesExplicit.cmake")


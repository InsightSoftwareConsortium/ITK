## FFTW can be compiled and subsequently linked against
## various data types.
## There is a single set of include files, and then multiple libraries,
## One for each type. I.e. libfftw3.a-->double, libfftw3f.a-->float

## This FindFFTW3 module locates system-installed FFTW libraries.
##
## This module creates the following IMPORTED targets in the FFTW3:: namespace:
##   FFTW3::fftw3            - Double precision FFTW library
##   FFTW3::fftw3_threads    - Double precision FFTW threads library
##   FFTW3::fftw3f           - Single precision FFTW library
##   FFTW3::fftw3f_threads   - Single precision FFTW threads library
##   FFTW3::fftw3l           - Long double precision FFTW library
##   FFTW3::fftw3l_threads   - Long double precision FFTW threads library
##   FFTW3::fftw3q           - Quad precision FFTW library
##   FFTW3::fftw3q_threads   - Quad precision FFTW threads library
##
## This module sets the following variables:
##   FFTW3_FOUND              - True if FFTW3 is found
##   FFTW3_VERSION            - Version of FFTW3 (e.g., "3.3.10")
##   FFTW3_INCLUDE_DIRS       - Include directories for FFTW3
##   FFTW3_LIBRARIES          - Libraries to link for FFTW3
##   FFTW3_fftw3_FOUND        - True if double precision library found
##   FFTW3_fftw3f_FOUND       - True if single precision library found
##   FFTW3_fftw3l_FOUND       - True if long double precision library found
##   FFTW3_fftw3q_FOUND       - True if quad precision library found

include(FindPackageHandleStandardArgs)

# Try to get version from pkg-config
find_package(PkgConfig QUIET)
if(PKG_CONFIG_FOUND)
  pkg_check_modules(PC_FFTW3 QUIET fftw3)
  set(FFTW3_VERSION ${PC_FFTW3_VERSION})
endif()

find_path(
  FFTW3_INCLUDE_DIR
  fftw3.h
  PATH_SUFFIXES
    fftw3
    fftw
)
mark_as_advanced(FFTW3_INCLUDE_DIR)

# Determine which components to search for
# If FFTW3_FIND_COMPONENTS is set, only search for those components
# Otherwise, search for all precision types
if(FFTW3_FIND_COMPONENTS)
  set(_FFTW3_components_to_search ${FFTW3_FIND_COMPONENTS})
  message(
    STATUS
    "FFTW3_FIND_COMPONENTS is set, searching for components: ${_FFTW3_components_to_search}"
  )
else()
  set(
    _FFTW3_components_to_search
    fftw3
    fftw3f
    fftw3l
    fftw3q
  )

  set(_varients "")
  foreach(_comp IN LISTS _FFTW3_components_to_search)
    list(
      APPEND
      _varients
      "${_comp}"
      "${_comp}_threads"
    )
  endforeach()

  list(APPEND _FFTW3_components_to_search ${_varients})
endif()

# Search for each component library
foreach(_comp IN LISTS _FFTW3_components_to_search)
  find_library(FFTW3_${_comp}_LIBRARY NAMES ${_comp} NAMES_PER_DIR)
  if(FFTW3_${_comp}_LIBRARY)
    set(FFTW3_${_comp}_FOUND TRUE)
  else()
    set(FFTW3_${_comp}_FOUND FALSE)
  endif()
endforeach()

# Use find_package_handle_standard_args to set FFTW3_FOUND
# With HANDLE_COMPONENTS, this will:
# - Check that all required components (from COMPONENTS) have <Package>_<Component>_FOUND=TRUE
# - Set FFTW3_FOUND=FALSE if any required component is missing
# - Report which components were found/missing
find_package_handle_standard_args(
  FFTW3
  REQUIRED_VARS
    FFTW3_INCLUDE_DIR
  VERSION_VAR FFTW3_VERSION
  HANDLE_COMPONENTS
)

if(FFTW3_FOUND)
  # Set standard output variables
  set(FFTW3_INCLUDE_DIRS ${FFTW3_INCLUDE_DIR})
  set(FFTW3_LIBRARIES "")

  # Create IMPORTED targets for requested/found components
  foreach(_comp IN LISTS _FFTW3_components_to_search)
    if(FFTW3_${_comp}_LIBRARY)
      if(NOT TARGET FFTW3::${_comp})
        add_library(FFTW3::${_comp} UNKNOWN IMPORTED)
        set_target_properties(
          FFTW3::${_comp}
          PROPERTIES
            IMPORTED_LOCATION
              "${FFTW3_${_comp}_LIBRARY}"
            INTERFACE_INCLUDE_DIRECTORIES
              "${FFTW3_INCLUDE_DIR}"
        )
        if(
          (
            CMAKE_SYSTEM_NAME
              STREQUAL
              "Linux"
          )
          AND
            (
              "${FFTW3_${_comp}_LIBRARY}"
                MATCHES
                "\\${CMAKE_STATIC_LIBRARY_SUFFIX}$"
            )
        )
          set_property(
            TARGET
              FFTW3::${_comp}
            APPEND
            PROPERTY
              INTERFACE_LINK_LIBRARIES
                m
          )
        endif()
      endif()
    endif()
    list(APPEND FFTW3_LIBRARIES FFTW3::${_comp})
  endforeach()
endif()

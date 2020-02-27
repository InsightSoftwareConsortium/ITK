# Set up wrapping options

option(ITK_WRAP_PYTHON "Build Python support" OFF)
option(ITK_WRAP_JAVA "Build Java support (Currently not supported)" OFF)
option(ITK_WRAP_RUBY "Build Ruby support (Currently not supported)" OFF)
option(ITK_WRAP_PERL "Build Perl support (Currently not supported)" OFF)
option(ITK_WRAP_TCL "Build Tcl support (Currently not supported)" OFF)
mark_as_advanced(ITK_WRAP_JAVA)
mark_as_advanced(ITK_WRAP_RUBY)
mark_as_advanced(ITK_WRAP_PERL)
mark_as_advanced(ITK_WRAP_TCL)

if(ITK_WRAP_PYTHON OR ITK_WRAP_JAVA OR ITK_WRAP_RUBY OR ITK_WRAP_PERL OR ITK_WRAP_TCL)
  if(NOT ITK_DYNAMIC_LOADING)
    message(FATAL_ERROR "Wrapping requires ITK_DYNAMIC_LOADING to be ON")
  endif()
  # ITK_WRAPPING is an internal variable
  set(ITK_WRAPPING ON CACHE INTERNAL "Build external languages support" FORCE)
else()
  set(ITK_WRAPPING OFF CACHE INTERNAL "Build external languages support" FORCE)
endif()

cmake_dependent_option(ITK_WRAP_unsigned_char "Wrap unsigned char type" ON "ITK_WRAPPING" OFF)
cmake_dependent_option(ITK_WRAP_unsigned_short "Wrap unsigned short type" OFF "ITK_WRAPPING" OFF)
cmake_dependent_option(ITK_WRAP_unsigned_long "Wrap unsigned long type" OFF "ITK_WRAPPING" OFF)
mark_as_advanced(ITK_WRAP_unsigned_long)
if(ITK_WRAP_unsigned_long)
  message(WARNING "ITK_WRAP_unsigned_long is deprecated. Please use ITK_WRAP_unsigned_long_long instead.")
endif()
cmake_dependent_option(ITK_WRAP_unsigned_long_long "Wrap unsigned long long type" OFF "ITK_WRAPPING" OFF)

cmake_dependent_option(ITK_WRAP_signed_char "Wrap signed char type" OFF "ITK_WRAPPING" OFF)
cmake_dependent_option(ITK_WRAP_signed_short "Wrap signed short type" ON "ITK_WRAPPING" OFF)
cmake_dependent_option(ITK_WRAP_signed_long "Wrap signed long type" OFF "ITK_WRAPPING" OFF)
mark_as_advanced(ITK_WRAP_signed_long)
if(ITK_WRAP_signed_long)
  message(WARNING "ITK_WRAP_signed_long is deprecated. Please use ITK_WRAP_signed_long_long instead.")
endif()
cmake_dependent_option(ITK_WRAP_signed_long_long "Wrap signed long long type" OFF "ITK_WRAPPING" OFF)

cmake_dependent_option(ITK_WRAP_float "Wrap float type" ON "ITK_WRAPPING" OFF)
cmake_dependent_option(ITK_WRAP_double "Wrap double type" OFF "ITK_WRAPPING" OFF)

cmake_dependent_option(ITK_WRAP_vector_float "Wrap vector float type" ON "ITK_WRAPPING" OFF)
cmake_dependent_option(ITK_WRAP_vector_double "Wrap vector double type" OFF "ITK_WRAPPING" OFF)

cmake_dependent_option(ITK_WRAP_covariant_vector_float "Wrap covariant vector float type" ON "ITK_WRAPPING" OFF)
cmake_dependent_option(ITK_WRAP_covariant_vector_double "Wrap covariant vector double type" OFF "ITK_WRAPPING" OFF)

cmake_dependent_option(ITK_WRAP_rgb_unsigned_char "Wrap RGB< unsigned char > type" ON "ITK_WRAPPING" OFF)
cmake_dependent_option(ITK_WRAP_rgb_unsigned_short "Wrap RGB< unsigned short > type" OFF "ITK_WRAPPING" OFF)

cmake_dependent_option(ITK_WRAP_rgba_unsigned_char "Wrap RGBA< unsigned char > type" ON "ITK_WRAPPING" OFF)
cmake_dependent_option(ITK_WRAP_rgba_unsigned_short "Wrap RGBA< unsigned short > type" OFF "ITK_WRAPPING" OFF)

cmake_dependent_option(ITK_WRAP_complex_float "Wrap complex< float > type" ON "ITK_WRAPPING" OFF)
cmake_dependent_option(ITK_WRAP_complex_double "Wrap complex< double > type" OFF "ITK_WRAPPING" OFF)

if(ITK_WRAPPING)
  # Check for type conditions that need to be fullfilled.
  foreach(t float double)
    # Vectors
    if(ITK_WRAP_vector_${t} AND NOT ITK_WRAP_${t})
      message(SEND_ERROR "To use 'ITK_WRAP_vector_${t}', please set 'ITK_WRAP_${t}' to ON")
    endif()

    # Covariant vectors
    if(ITK_WRAP_covariant_vector_${t} AND NOT ITK_WRAP_${t})
      message(SEND_ERROR "To use 'ITK_WRAP_covariant_vector_${t}', please set 'ITK_WRAP_${t}' to ON")
    endif()

    # Complex
    if(ITK_WRAP_complex_${t} AND NOT ITK_WRAP_${t})
      message(SEND_ERROR "To use 'ITK_WRAP_complex_${t}', please set 'ITK_WRAP_${t}' to ON")
    endif()
  endforeach()

  foreach(t unsigned_char unsigned_short)
    if(ITK_WRAP_rgb_${t} AND NOT ITK_WRAP_${t})
      message(SEND_ERROR "To use 'ITK_WRAP_rgb_${t}', please set 'ITK_WRAP_${t}' to ON")
    endif()

    if(ITK_WRAP_rgba_${t} AND NOT ITK_WRAP_${t})
      message(SEND_ERROR "To use 'ITK_WRAP_rgba_${t}', please set 'ITK_WRAP_${t}' to ON")
    endif()
  endforeach()

  if(DEFINED ITK_WRAP_DIMS)
    # Keep ITK_WRAP_DIMS for backward compatibility.
    set(ITK_WRAP_IMAGE_DIMS "${ITK_WRAP_DIMS}" CACHE STRING "Dimensions available separated by semicolons (;)")
  else()
    set(ITK_WRAP_IMAGE_DIMS "2;3" CACHE STRING "Dimensions available separated by semicolons (;)")
    # Keep ITK_WRAP_DIMS for extra modules developed outside of ITK.
    set(ITK_WRAP_DIMS "${ITK_WRAP_IMAGE_DIMS}")
  endif()
  UNIQUE(vector_components "${ITK_WRAP_IMAGE_DIMS};4")
  set(ITK_WRAP_VECTOR_COMPONENTS "${vector_components}" CACHE STRING "Number of vector components available separated by semicolons (;)")
else()
  # Hide options that are not relevant when no wrapping is requested
  if(ITK_WRAP_IMAGE_DIMS)
    set_property(CACHE ITK_WRAP_IMAGE_DIMS PROPERTY TYPE INTERNAL)
  endif()
  if(ITK_WRAP_DIMS)
    set_property(CACHE ITK_WRAP_DIMS PROPERTY TYPE INTERNAL)
  endif()
  if(ITK_WRAP_VECTOR_COMPONENTS)
    set_property(CACHE ITK_WRAP_VECTOR_COMPONENTS PROPERTY TYPE INTERNAL)
  endif()
endif()

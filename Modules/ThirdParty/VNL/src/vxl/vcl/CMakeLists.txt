# vcl/CMakeLists.txt

if(WIN32)
  # MSVC.NET produces lots of warnings like
  # "warning LNK4221: no public symbols found; archive member will be inaccessible"
  # that depend on subtle logic at compile time.
  # This is a bit indescriminate, but it may be the only feasible way to suppress them.
  if(CMAKE_CXX_COMPILER MATCHES "^cl$")
    set( CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} /WARN:0")
    set( CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /WARN:0")
  endif()
endif()


project( vcl )

## Dynamically create compiler detection header
set(VCL_COMPILER_DETECTION_HEADER "${CMAKE_CURRENT_BINARY_DIR}/vcl_compiler_detection.h")
include(WriteCompilerDetectionHeader)
write_compiler_detection_header(
    FILE "${VCL_COMPILER_DETECTION_HEADER}"
    PREFIX VXL
    OUTPUT_FILES_VAR compiler_stub_headers
    OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/compilers
    COMPILERS AppleClang Clang GNU MSVC SunPro Intel #Supported compilers as of 3.10.3
    VERSION ${CMAKE_VERSION}
    FEATURES
      cxx_aggregate_default_initializers      # Aggregate default initializers, as defined in N3605.
      cxx_alias_templates                     # Template aliases, as defined in N2258.
      cxx_alignas                             # Alignment control alignas, as defined in N2341.
      cxx_alignof                             # Alignment control alignof, as defined in N2341.
      cxx_attributes                          # Generic attributes, as defined in N2761.
      cxx_attribute_deprecated                # [[deprecated]] attribute, as defined in N3760.
      cxx_auto_type                           # Automatic type deduction, as defined in N1984.
      cxx_binary_literals                     # Binary literals, as defined in N3472.
      cxx_constexpr                           # Constant expressions, as defined in N2235.
      cxx_contextual_conversions              # Contextual conversions, as defined in N3323.
      cxx_decltype_incomplete_return_types    # Decltype on incomplete return types, as defined in N3276.
      cxx_decltype                            # Decltype, as defined in N2343.
      cxx_decltype_auto                       # decltype(auto) semantics, as defined in N3638.
      cxx_default_function_template_args      # Default template arguments for function templates, as defined in DR226
      cxx_defaulted_functions                 # Defaulted functions, as defined in N2346.
      cxx_defaulted_move_initializers         # Defaulted move initializers, as defined in N3053.
      cxx_delegating_constructors             # Delegating constructors, as defined in N1986.
      cxx_deleted_functions                   # Deleted functions, as defined in N2346.
      cxx_digit_separators                    # Digit separators, as defined in N3781.
      cxx_enum_forward_declarations           # Enum forward declarations, as defined in N2764.
      cxx_explicit_conversions                # Explicit conversion operators, as defined in N2437.
      cxx_extended_friend_declarations        # Extended friend declarations, as defined in N1791.
      cxx_extern_templates                    # Extern templates, as defined in N1987.
      cxx_final                               # Override control final keyword, as defined in N2928, N3206 and N3272.
      cxx_func_identifier                     # Predefined __func__ identifier, as defined in N2340.
      cxx_generalized_initializers            # Initializer lists, as defined in N2672.
      cxx_generic_lambdas                     # Generic lambdas, as defined in N3649.
      cxx_inheriting_constructors             # Inheriting constructors, as defined in N2540.
      cxx_inline_namespaces                   # Inline namespaces, as defined in N2535.
      cxx_lambdas                             #Lambda functions, as defined in N2927.
      cxx_lambda_init_captures                # Initialized lambda captures, as defined in N3648.
      cxx_local_type_template_args            # Local and unnamed types as template arguments, as defined in N2657.
      cxx_long_long_type                      # long long type, as defined in N1811.
      cxx_noexcept                            # Exception specifications, as defined in N3050.
      cxx_nonstatic_member_init               # Non-static data member initialization, as defined in N2756.
      cxx_nullptr                             # Null pointer, as defined in N2431.
      cxx_override                            # Override control override keyword, as defined in N2928, N3206 and N3272.
      cxx_range_for                           # Range-based for, as defined in N2930.
      cxx_raw_string_literals                 # Raw string literals, as defined in N2442.
      cxx_reference_qualified_functions       # Reference qualified functions, as defined in N2439.
      cxx_relaxed_constexpr                   #Relaxed constexpr, as defined in N3652.
      cxx_return_type_deduction               # Return type deduction on normal functions, as defined in N3386.
      cxx_right_angle_brackets                # Right angle bracket parsing, as defined in N1757.
      cxx_rvalue_references                   # R-value references, as defined in N2118.
      cxx_sizeof_member                       # Size of non-static data members, as defined in N2253.
      cxx_static_assert                       # Static assert, as defined in N1720.
      cxx_strong_enums                        # Strongly typed enums, as defined in N2347.
      cxx_thread_local                        #Thread-local variables, as defined in N2659.
      cxx_trailing_return_types               # Automatic function return type, as defined in N2541.
      cxx_unicode_literals                    # Unicode string literals, as defined in N2442.
      cxx_uniform_initialization              # Uniform intialization, as defined in N2640.
      cxx_unrestricted_unions                 # Unrestricted unions, as defined in N2544.
      cxx_user_literals                       # User-defined literals, as defined in N2765.
      cxx_variable_templates                  # Variable templates, as defined in N3651.
      cxx_variadic_macros                     # Variadic macros, as defined in N1653.
      cxx_variadic_templates                  # Variadic templates, as defined in N2242.
      cxx_template_template_parameters        # Template template parameters, as defined in ISO/IEC 14882:1998.
)

#
# Install platform-specific compiler detection files
#
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  set(_compilers_destination ${VXL_INSTALL_INCLUDE_DIR}/vcl/compilers)
  set(_complier_detection_destination ${VXL_INSTALL_INCLUDE_DIR}/vcl)
else()
  set(_compilers_destination ${VXL_INSTALL_INCLUDE_DIR}/compilers)
  set(_complier_detection_destination ${VXL_INSTALL_INCLUDE_DIR})
endif()
file(GLOB VXL_COMPILER_DETECTION_FILES ${CMAKE_CURRENT_BINARY_DIR}/compilers/*)
install(FILES ${VXL_COMPILER_DETECTION_FILES}
      DESTINATION ${_compilers_destination}
      PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ
      COMPONENT Development )
install(FILES ${VCL_COMPILER_DETECTION_HEADER}
      DESTINATION ${_complier_detection_destination}
      PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ
      COMPONENT Development )

# If VXL_INSTALL_INCLUDE_DIR is the default value
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  set(_config_destination ${VXL_INSTALL_INCLUDE_DIR}/vcl)
else()
  set(_config_destination ${VXL_INSTALL_INCLUDE_DIR})
endif()
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vcl_compiler.h.in ${PROJECT_BINARY_DIR}/vcl_compiler.h ${_config_destination})
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vcl_where_root_dir.h.in ${PROJECT_BINARY_DIR}/vcl_where_root_dir.h  ${_config_destination})

include_directories(${PROJECT_BINARY_DIR})
set( vcl_sources
  vcl_legacy_aliases.h ${VCL_COMPILER_DETECTION_HEADER}
  vcl_deprecated.cxx  vcl_deprecated.h

  vcl_cmath.h
  vcl_cassert.h
  vcl_cstdlib.h

  vcl_algorithm.h
  vcl_bitset.h
  vcl_cctype.h
  vcl_cerrno.h
  vcl_cfloat.h
  vcl_ciso646.h
  vcl_climits.h
  vcl_clocale.h
  vcl_complex.h
  vcl_csetjmp.h
  vcl_csignal.h
  vcl_cstdarg.h
  vcl_cstddef.h
  vcl_cstdio.h
  vcl_cstring.h
  vcl_ctime.h
  vcl_cwchar.h
  vcl_cwctype.h
  vcl_deprecated_header.h
  vcl_deque.h
  vcl_exception.h
  vcl_fstream.h
  vcl_functional.h
  vcl_iomanip.h
  vcl_ios.h
  vcl_iosfwd.h
  vcl_iostream.h
  vcl_istream.h
  vcl_iterator.h
  vcl_limits.h
  vcl_list.h
  vcl_locale.h
  vcl_map.h
  vcl_memory.h
  vcl_numeric.h
  vcl_ostream.h
  vcl_queue.h
  vcl_set.h
  vcl_sstream.h
  vcl_stack.h
  vcl_stdexcept.h
  vcl_stlfwd.h
  vcl_streambuf.h
  vcl_string.h
  vcl_typeinfo.h
  vcl_utility.h
  vcl_valarray.h
  vcl_vector.h
  vcl_sys/time.h
  vcl_sys/types.h

  # The following shall not be used other than as reference count for smart pointers
  vcl_atomic_count.h
  internal/vcl_atomic_count_gcc.h
  internal/vcl_atomic_count_gcc_x86.h
  internal/vcl_atomic_count_pthreads.h
  internal/vcl_atomic_count_solaris.h
  internal/vcl_atomic_count_sync.h
  internal/vcl_atomic_count_win32.h
  internal/vcl_interlocked.h
)


# We use implicit instantiation of the standard library now,
# so we shouldn't be explicitly instantiating anything.
# This also avoid trouble where the instantiation hack conflict
# with modern, more compliant compilers.
#aux_source_directory(Templates vcl_sources)
vxl_add_library(LIBRARY_NAME ${VXL_LIB_PREFIX}vcl
  LIBRARY_SOURCES ${vcl_sources}
  )
if(UNIX)
  target_link_libraries(${VXL_LIB_PREFIX}vcl m)
endif()

if( BUILD_TESTING)
  add_subdirectory(tests)
endif()

if (VXL_BUILD_OBJECT_LIBRARIES)
  add_library(vcl-obj OBJECT ${vcl_sources})
endif()

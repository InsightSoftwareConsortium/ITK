#ifndef VXL_COMPILER_DETECTION_H
#  error This file may only be included from vcl_compiler_detection.h
#endif

#    if !(((__clang_major__ * 100) + __clang_minor__) >= 301)
#      error Unsupported compiler version
#    endif

# define VXL_COMPILER_VERSION_MAJOR VXL_DEC(__clang_major__)
# define VXL_COMPILER_VERSION_MINOR VXL_DEC(__clang_minor__)
# define VXL_COMPILER_VERSION_PATCH VXL_DEC(__clang_patchlevel__)
# if defined(_MSC_VER)
   /* _MSC_VER = VVRR */
#  define VXL_SIMULATE_VERSION_MAJOR VXL_DEC(_MSC_VER / 100)
#  define VXL_SIMULATE_VERSION_MINOR VXL_DEC(_MSC_VER % 100)
# endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_aggregate_nsdmi)
#      define VXL_COMPILER_CXX_AGGREGATE_DEFAULT_INITIALIZERS 1
#    else
#      define VXL_COMPILER_CXX_AGGREGATE_DEFAULT_INITIALIZERS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_alias_templates)
#      define VXL_COMPILER_CXX_ALIAS_TEMPLATES 1
#    else
#      define VXL_COMPILER_CXX_ALIAS_TEMPLATES 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_alignas)
#      define VXL_COMPILER_CXX_ALIGNAS 1
#    else
#      define VXL_COMPILER_CXX_ALIGNAS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_alignas)
#      define VXL_COMPILER_CXX_ALIGNOF 1
#    else
#      define VXL_COMPILER_CXX_ALIGNOF 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_attributes)
#      define VXL_COMPILER_CXX_ATTRIBUTES 1
#    else
#      define VXL_COMPILER_CXX_ATTRIBUTES 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_ATTRIBUTE_DEPRECATED 1
#    else
#      define VXL_COMPILER_CXX_ATTRIBUTE_DEPRECATED 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_auto_type)
#      define VXL_COMPILER_CXX_AUTO_TYPE 1
#    else
#      define VXL_COMPILER_CXX_AUTO_TYPE 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_binary_literals)
#      define VXL_COMPILER_CXX_BINARY_LITERALS 1
#    else
#      define VXL_COMPILER_CXX_BINARY_LITERALS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_constexpr)
#      define VXL_COMPILER_CXX_CONSTEXPR 1
#    else
#      define VXL_COMPILER_CXX_CONSTEXPR 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_contextual_conversions)
#      define VXL_COMPILER_CXX_CONTEXTUAL_CONVERSIONS 1
#    else
#      define VXL_COMPILER_CXX_CONTEXTUAL_CONVERSIONS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_decltype_incomplete_return_types)
#      define VXL_COMPILER_CXX_DECLTYPE_INCOMPLETE_RETURN_TYPES 1
#    else
#      define VXL_COMPILER_CXX_DECLTYPE_INCOMPLETE_RETURN_TYPES 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_decltype)
#      define VXL_COMPILER_CXX_DECLTYPE 1
#    else
#      define VXL_COMPILER_CXX_DECLTYPE 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_DECLTYPE_AUTO 1
#    else
#      define VXL_COMPILER_CXX_DECLTYPE_AUTO 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_default_function_template_args)
#      define VXL_COMPILER_CXX_DEFAULT_FUNCTION_TEMPLATE_ARGS 1
#    else
#      define VXL_COMPILER_CXX_DEFAULT_FUNCTION_TEMPLATE_ARGS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_defaulted_functions)
#      define VXL_COMPILER_CXX_DEFAULTED_FUNCTIONS 1
#    else
#      define VXL_COMPILER_CXX_DEFAULTED_FUNCTIONS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_defaulted_functions)
#      define VXL_COMPILER_CXX_DEFAULTED_MOVE_INITIALIZERS 1
#    else
#      define VXL_COMPILER_CXX_DEFAULTED_MOVE_INITIALIZERS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_delegating_constructors)
#      define VXL_COMPILER_CXX_DELEGATING_CONSTRUCTORS 1
#    else
#      define VXL_COMPILER_CXX_DELEGATING_CONSTRUCTORS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_deleted_functions)
#      define VXL_COMPILER_CXX_DELETED_FUNCTIONS 1
#    else
#      define VXL_COMPILER_CXX_DELETED_FUNCTIONS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_DIGIT_SEPARATORS 1
#    else
#      define VXL_COMPILER_CXX_DIGIT_SEPARATORS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_ENUM_FORWARD_DECLARATIONS 1
#    else
#      define VXL_COMPILER_CXX_ENUM_FORWARD_DECLARATIONS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_explicit_conversions)
#      define VXL_COMPILER_CXX_EXPLICIT_CONVERSIONS 1
#    else
#      define VXL_COMPILER_CXX_EXPLICIT_CONVERSIONS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_EXTENDED_FRIEND_DECLARATIONS 1
#    else
#      define VXL_COMPILER_CXX_EXTENDED_FRIEND_DECLARATIONS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_EXTERN_TEMPLATES 1
#    else
#      define VXL_COMPILER_CXX_EXTERN_TEMPLATES 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_override_control)
#      define VXL_COMPILER_CXX_FINAL 1
#    else
#      define VXL_COMPILER_CXX_FINAL 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_FUNC_IDENTIFIER 1
#    else
#      define VXL_COMPILER_CXX_FUNC_IDENTIFIER 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_generalized_initializers)
#      define VXL_COMPILER_CXX_GENERALIZED_INITIALIZERS 1
#    else
#      define VXL_COMPILER_CXX_GENERALIZED_INITIALIZERS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_GENERIC_LAMBDAS 1
#    else
#      define VXL_COMPILER_CXX_GENERIC_LAMBDAS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_inheriting_constructors)
#      define VXL_COMPILER_CXX_INHERITING_CONSTRUCTORS 1
#    else
#      define VXL_COMPILER_CXX_INHERITING_CONSTRUCTORS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_INLINE_NAMESPACES 1
#    else
#      define VXL_COMPILER_CXX_INLINE_NAMESPACES 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_lambdas)
#      define VXL_COMPILER_CXX_LAMBDAS 1
#    else
#      define VXL_COMPILER_CXX_LAMBDAS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_init_captures)
#      define VXL_COMPILER_CXX_LAMBDA_INIT_CAPTURES 1
#    else
#      define VXL_COMPILER_CXX_LAMBDA_INIT_CAPTURES 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_local_type_template_args)
#      define VXL_COMPILER_CXX_LOCAL_TYPE_TEMPLATE_ARGS 1
#    else
#      define VXL_COMPILER_CXX_LOCAL_TYPE_TEMPLATE_ARGS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_LONG_LONG_TYPE 1
#    else
#      define VXL_COMPILER_CXX_LONG_LONG_TYPE 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_noexcept)
#      define VXL_COMPILER_CXX_NOEXCEPT 1
#    else
#      define VXL_COMPILER_CXX_NOEXCEPT 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_nonstatic_member_init)
#      define VXL_COMPILER_CXX_NONSTATIC_MEMBER_INIT 1
#    else
#      define VXL_COMPILER_CXX_NONSTATIC_MEMBER_INIT 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_nullptr)
#      define VXL_COMPILER_CXX_NULLPTR 1
#    else
#      define VXL_COMPILER_CXX_NULLPTR 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_override_control)
#      define VXL_COMPILER_CXX_OVERRIDE 1
#    else
#      define VXL_COMPILER_CXX_OVERRIDE 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_range_for)
#      define VXL_COMPILER_CXX_RANGE_FOR 1
#    else
#      define VXL_COMPILER_CXX_RANGE_FOR 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_raw_string_literals)
#      define VXL_COMPILER_CXX_RAW_STRING_LITERALS 1
#    else
#      define VXL_COMPILER_CXX_RAW_STRING_LITERALS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_reference_qualified_functions)
#      define VXL_COMPILER_CXX_REFERENCE_QUALIFIED_FUNCTIONS 1
#    else
#      define VXL_COMPILER_CXX_REFERENCE_QUALIFIED_FUNCTIONS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_relaxed_constexpr)
#      define VXL_COMPILER_CXX_RELAXED_CONSTEXPR 1
#    else
#      define VXL_COMPILER_CXX_RELAXED_CONSTEXPR 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_return_type_deduction)
#      define VXL_COMPILER_CXX_RETURN_TYPE_DEDUCTION 1
#    else
#      define VXL_COMPILER_CXX_RETURN_TYPE_DEDUCTION 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_RIGHT_ANGLE_BRACKETS 1
#    else
#      define VXL_COMPILER_CXX_RIGHT_ANGLE_BRACKETS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_rvalue_references)
#      define VXL_COMPILER_CXX_RVALUE_REFERENCES 1
#    else
#      define VXL_COMPILER_CXX_RVALUE_REFERENCES 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_SIZEOF_MEMBER 1
#    else
#      define VXL_COMPILER_CXX_SIZEOF_MEMBER 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_static_assert)
#      define VXL_COMPILER_CXX_STATIC_ASSERT 1
#    else
#      define VXL_COMPILER_CXX_STATIC_ASSERT 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_strong_enums)
#      define VXL_COMPILER_CXX_STRONG_ENUMS 1
#    else
#      define VXL_COMPILER_CXX_STRONG_ENUMS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_thread_local)
#      define VXL_COMPILER_CXX_THREAD_LOCAL 1
#    else
#      define VXL_COMPILER_CXX_THREAD_LOCAL 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_trailing_return)
#      define VXL_COMPILER_CXX_TRAILING_RETURN_TYPES 1
#    else
#      define VXL_COMPILER_CXX_TRAILING_RETURN_TYPES 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_unicode_literals)
#      define VXL_COMPILER_CXX_UNICODE_LITERALS 1
#    else
#      define VXL_COMPILER_CXX_UNICODE_LITERALS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_generalized_initializers)
#      define VXL_COMPILER_CXX_UNIFORM_INITIALIZATION 1
#    else
#      define VXL_COMPILER_CXX_UNIFORM_INITIALIZATION 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_unrestricted_unions)
#      define VXL_COMPILER_CXX_UNRESTRICTED_UNIONS 1
#    else
#      define VXL_COMPILER_CXX_UNRESTRICTED_UNIONS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_user_literals)
#      define VXL_COMPILER_CXX_USER_LITERALS 1
#    else
#      define VXL_COMPILER_CXX_USER_LITERALS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_variable_templates)
#      define VXL_COMPILER_CXX_VARIABLE_TEMPLATES 1
#    else
#      define VXL_COMPILER_CXX_VARIABLE_TEMPLATES 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_VARIADIC_MACROS 1
#    else
#      define VXL_COMPILER_CXX_VARIADIC_MACROS 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __has_feature(cxx_variadic_templates)
#      define VXL_COMPILER_CXX_VARIADIC_TEMPLATES 1
#    else
#      define VXL_COMPILER_CXX_VARIADIC_TEMPLATES 0
#    endif

#    if ((__clang_major__ * 100) + __clang_minor__) >= 301 && __cplusplus >= 199711L
#      define VXL_COMPILER_CXX_TEMPLATE_TEMPLATE_PARAMETERS 1
#    else
#      define VXL_COMPILER_CXX_TEMPLATE_TEMPLATE_PARAMETERS 0
#    endif

#ifndef VXL_COMPILER_DETECTION_H
#  error This file may only be included from vcl_compiler_detection.h
#endif

#    if !((__GNUC__ * 100 + __GNUC_MINOR__) >= 404)
#      error Unsupported compiler version
#    endif

# if defined(__GNUC__)
#  define VXL_COMPILER_VERSION_MAJOR VXL_DEC(__GNUC__)
# else
#  define VXL_COMPILER_VERSION_MAJOR VXL_DEC(__GNUG__)
# endif
# if defined(__GNUC_MINOR__)
#  define VXL_COMPILER_VERSION_MINOR VXL_DEC(__GNUC_MINOR__)
# endif
# if defined(__GNUC_PATCHLEVEL__)
#  define VXL_COMPILER_VERSION_PATCH VXL_DEC(__GNUC_PATCHLEVEL__)
# endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 500 && __cplusplus >= 201402L
#      define VXL_COMPILER_CXX_AGGREGATE_DEFAULT_INITIALIZERS 1
#    else
#      define VXL_COMPILER_CXX_AGGREGATE_DEFAULT_INITIALIZERS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 407 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_ALIAS_TEMPLATES 1
#    else
#      define VXL_COMPILER_CXX_ALIAS_TEMPLATES 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 408 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_ALIGNAS 1
#    else
#      define VXL_COMPILER_CXX_ALIGNAS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 408 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_ALIGNOF 1
#    else
#      define VXL_COMPILER_CXX_ALIGNOF 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 408 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_ATTRIBUTES 1
#    else
#      define VXL_COMPILER_CXX_ATTRIBUTES 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 409 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_ATTRIBUTE_DEPRECATED 1
#    else
#      define VXL_COMPILER_CXX_ATTRIBUTE_DEPRECATED 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_AUTO_TYPE 1
#    else
#      define VXL_COMPILER_CXX_AUTO_TYPE 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 409 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_BINARY_LITERALS 1
#    else
#      define VXL_COMPILER_CXX_BINARY_LITERALS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 406 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_CONSTEXPR 1
#    else
#      define VXL_COMPILER_CXX_CONSTEXPR 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 409 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_CONTEXTUAL_CONVERSIONS 1
#    else
#      define VXL_COMPILER_CXX_CONTEXTUAL_CONVERSIONS 0
#    endif

#    if ((__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__) >= 40801) && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_DECLTYPE_INCOMPLETE_RETURN_TYPES 1
#    else
#      define VXL_COMPILER_CXX_DECLTYPE_INCOMPLETE_RETURN_TYPES 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_DECLTYPE 1
#    else
#      define VXL_COMPILER_CXX_DECLTYPE 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 409 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_DECLTYPE_AUTO 1
#    else
#      define VXL_COMPILER_CXX_DECLTYPE_AUTO 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_DEFAULT_FUNCTION_TEMPLATE_ARGS 1
#    else
#      define VXL_COMPILER_CXX_DEFAULT_FUNCTION_TEMPLATE_ARGS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_DEFAULTED_FUNCTIONS 1
#    else
#      define VXL_COMPILER_CXX_DEFAULTED_FUNCTIONS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 406 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_DEFAULTED_MOVE_INITIALIZERS 1
#    else
#      define VXL_COMPILER_CXX_DEFAULTED_MOVE_INITIALIZERS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 407 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_DELEGATING_CONSTRUCTORS 1
#    else
#      define VXL_COMPILER_CXX_DELEGATING_CONSTRUCTORS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_DELETED_FUNCTIONS 1
#    else
#      define VXL_COMPILER_CXX_DELETED_FUNCTIONS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 409 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_DIGIT_SEPARATORS 1
#    else
#      define VXL_COMPILER_CXX_DIGIT_SEPARATORS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 406 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_ENUM_FORWARD_DECLARATIONS 1
#    else
#      define VXL_COMPILER_CXX_ENUM_FORWARD_DECLARATIONS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 405 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_EXPLICIT_CONVERSIONS 1
#    else
#      define VXL_COMPILER_CXX_EXPLICIT_CONVERSIONS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 407 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_EXTENDED_FRIEND_DECLARATIONS 1
#    else
#      define VXL_COMPILER_CXX_EXTENDED_FRIEND_DECLARATIONS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_EXTERN_TEMPLATES 1
#    else
#      define VXL_COMPILER_CXX_EXTERN_TEMPLATES 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 407 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_FINAL 1
#    else
#      define VXL_COMPILER_CXX_FINAL 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_FUNC_IDENTIFIER 1
#    else
#      define VXL_COMPILER_CXX_FUNC_IDENTIFIER 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_GENERALIZED_INITIALIZERS 1
#    else
#      define VXL_COMPILER_CXX_GENERALIZED_INITIALIZERS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 409 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_GENERIC_LAMBDAS 1
#    else
#      define VXL_COMPILER_CXX_GENERIC_LAMBDAS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 408 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_INHERITING_CONSTRUCTORS 1
#    else
#      define VXL_COMPILER_CXX_INHERITING_CONSTRUCTORS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_INLINE_NAMESPACES 1
#    else
#      define VXL_COMPILER_CXX_INLINE_NAMESPACES 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 405 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_LAMBDAS 1
#    else
#      define VXL_COMPILER_CXX_LAMBDAS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 409 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_LAMBDA_INIT_CAPTURES 1
#    else
#      define VXL_COMPILER_CXX_LAMBDA_INIT_CAPTURES 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 405 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_LOCAL_TYPE_TEMPLATE_ARGS 1
#    else
#      define VXL_COMPILER_CXX_LOCAL_TYPE_TEMPLATE_ARGS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_LONG_LONG_TYPE 1
#    else
#      define VXL_COMPILER_CXX_LONG_LONG_TYPE 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 406 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_NOEXCEPT 1
#    else
#      define VXL_COMPILER_CXX_NOEXCEPT 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 407 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_NONSTATIC_MEMBER_INIT 1
#    else
#      define VXL_COMPILER_CXX_NONSTATIC_MEMBER_INIT 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 406 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_NULLPTR 1
#    else
#      define VXL_COMPILER_CXX_NULLPTR 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 407 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_OVERRIDE 1
#    else
#      define VXL_COMPILER_CXX_OVERRIDE 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 406 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_RANGE_FOR 1
#    else
#      define VXL_COMPILER_CXX_RANGE_FOR 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 405 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_RAW_STRING_LITERALS 1
#    else
#      define VXL_COMPILER_CXX_RAW_STRING_LITERALS 0
#    endif

#    if ((__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__) >= 40801) && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_REFERENCE_QUALIFIED_FUNCTIONS 1
#    else
#      define VXL_COMPILER_CXX_REFERENCE_QUALIFIED_FUNCTIONS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 500 && __cplusplus >= 201402L
#      define VXL_COMPILER_CXX_RELAXED_CONSTEXPR 1
#    else
#      define VXL_COMPILER_CXX_RELAXED_CONSTEXPR 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 409 && __cplusplus > 201103L
#      define VXL_COMPILER_CXX_RETURN_TYPE_DEDUCTION 1
#    else
#      define VXL_COMPILER_CXX_RETURN_TYPE_DEDUCTION 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_RIGHT_ANGLE_BRACKETS 1
#    else
#      define VXL_COMPILER_CXX_RIGHT_ANGLE_BRACKETS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_RVALUE_REFERENCES 1
#    else
#      define VXL_COMPILER_CXX_RVALUE_REFERENCES 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_SIZEOF_MEMBER 1
#    else
#      define VXL_COMPILER_CXX_SIZEOF_MEMBER 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_STATIC_ASSERT 1
#    else
#      define VXL_COMPILER_CXX_STATIC_ASSERT 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_STRONG_ENUMS 1
#    else
#      define VXL_COMPILER_CXX_STRONG_ENUMS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 408 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_THREAD_LOCAL 1
#    else
#      define VXL_COMPILER_CXX_THREAD_LOCAL 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_TRAILING_RETURN_TYPES 1
#    else
#      define VXL_COMPILER_CXX_TRAILING_RETURN_TYPES 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_UNICODE_LITERALS 1
#    else
#      define VXL_COMPILER_CXX_UNICODE_LITERALS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_UNIFORM_INITIALIZATION 1
#    else
#      define VXL_COMPILER_CXX_UNIFORM_INITIALIZATION 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 406 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_UNRESTRICTED_UNIONS 1
#    else
#      define VXL_COMPILER_CXX_UNRESTRICTED_UNIONS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 407 && __cplusplus >= 201103L
#      define VXL_COMPILER_CXX_USER_LITERALS 1
#    else
#      define VXL_COMPILER_CXX_USER_LITERALS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 500 && __cplusplus >= 201402L
#      define VXL_COMPILER_CXX_VARIABLE_TEMPLATES 1
#    else
#      define VXL_COMPILER_CXX_VARIABLE_TEMPLATES 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_VARIADIC_MACROS 1
#    else
#      define VXL_COMPILER_CXX_VARIADIC_MACROS 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && (__cplusplus >= 201103L || (defined(__GXX_EXPERIMENTAL_CXX0X__) && __GXX_EXPERIMENTAL_CXX0X__))
#      define VXL_COMPILER_CXX_VARIADIC_TEMPLATES 1
#    else
#      define VXL_COMPILER_CXX_VARIADIC_TEMPLATES 0
#    endif

#    if (__GNUC__ * 100 + __GNUC_MINOR__) >= 404 && __cplusplus
#      define VXL_COMPILER_CXX_TEMPLATE_TEMPLATE_PARAMETERS 1
#    else
#      define VXL_COMPILER_CXX_TEMPLATE_TEMPLATE_PARAMETERS 0
#    endif

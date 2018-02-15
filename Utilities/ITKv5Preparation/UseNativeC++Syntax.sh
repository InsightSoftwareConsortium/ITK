#!/bin/bash
# \author Hans J. Johnson
#
# Script to process a directory to remove unnecessary
# backwards compatibility layers for C++11
# options that are now required.
#
#  Step 1 for migration to ITKv5:  Continue building your toolkit with ITKv4, but use  -DITK_FUTURE_LEGACY_REMOVE:BOOL=ON


function ReplaceCXXString()
{
  oldstring="$1"
  newstring="$2"

  git grep -l "${oldstring}" | \
    fgrep -v itk_compiler_detection.h | fgrep -v itkMacro.h | fgrep -v CMakeLists.txt |fgrep -v .cmake | \
    fgrep -v ITKv5Preparation | \
    xargs sed -i '' -e "s/ ${oldstring}/ ${newstring}/g"

  file_changed=$(expr $(git status --porcelain 2>/dev/null| grep "^ M" | wc -l))
  if [[ file_changed -gt 0 ]];then

    cat > /tmp/COMMIT_MSG << EOF
COMP:  Use C++11 ${newstring} directly

git grep -l \"${oldstring}\" | \
  fgrep -v itk_compiler_detection.h | fgrep -v itkMacro.h | fgrep -v CMakeLists.txt |fgrep -v .cmake | \
  xargs sed -i '' -e \"s/ ${oldstring}/ ${newstring}/g\"
EOF
    git add -A
    git commit -F /tmp/COMMIT_MSG

    if [[ $? -ne 0 ]]; then
      echo "ERROR:  COMMIT DID NOT SUCCEED"
      echo "        Fix, then use: git commit -F /tmp/COMMIT_MSG"
      exit -1
    fi
  fi
}

ReplaceCXXString ITK_NOEXCEPT_OR_THROW ITK_NOEXCEPT
ReplaceCXXString ITK_HAS_CXX11_STATIC_ASSERT ITK_COMPILER_CXX_STATIC_ASSERT
ReplaceCXXString ITK_DELETE_FUNCTION ITK_DELETED_FUNCTION
ReplaceCXXString ITK_HAS_CPP11_ALIGNAS ITK_COMPILER_CXX_ALIGNAS


# cxx_nullptr
#    define ITK_NULLPTR nullptr
ReplaceCXXString ITK_NULLPTR nullptr

# cxx_deleted_functions
#    define ITK_DELETED_FUNCTION = delete
ReplaceCXXString ITK_DELETED_FUNCTION "= delete"

# cxx_constexpr
#    define ITK_CONSTEXPR constexpr
#COMP:  Use C++11 constexpr directly
ReplaceCXXString ITK_CONSTEXPR_VAR constexpr
ReplaceCXXString ITK_CONSTEXPR_FUNC constexpr


# cxx_noexcept
ReplaceCXXString ITK_NOEXCEPT noexcept

### --- Other considerations for replacement
# cxx_std_98
# cxx_template_template_parameters
# cxx_std_11
# cxx_alias_templates
# cxx_alignas
# cxx_alignof
# cxx_attributes
# cxx_auto_type
# cxx_decltype
# cxx_decltype_incomplete_return_types
# cxx_default_function_template_args
# cxx_defaulted_functions
# cxx_defaulted_move_initializers
# cxx_delegating_constructors
# cxx_enum_forward_declarations
# cxx_explicit_conversions
# cxx_extended_friend_declarations
# cxx_extern_templates
# cxx_final
# cxx_func_identifier
# cxx_generalized_initializers
# cxx_inheriting_constructors
# cxx_inline_namespaces
# cxx_lambdas
# cxx_local_type_template_args
# cxx_long_long_type
# cxx_nonstatic_member_init
# cxx_override
# cxx_range_for
# cxx_raw_string_literals
# cxx_reference_qualified_functions
# cxx_right_angle_brackets
# cxx_rvalue_references
# cxx_sizeof_member
# cxx_static_assert
# cxx_strong_enums
# cxx_thread_local
# cxx_trailing_return_types
# cxx_unicode_literals
# cxx_uniform_initialization
# cxx_unrestricted_unions
# cxx_user_literals
# cxx_variadic_macros
# cxx_variadic_templates
# cxx_std_14
# cxx_aggregate_default_initializers
# cxx_attribute_deprecated
# cxx_binary_literals
# cxx_contextual_conversions
# cxx_decltype_auto
# cxx_digit_separators
# cxx_generic_lambdas
# cxx_lambda_init_captures
# cxx_relaxed_constexpr
# cxx_return_type_deduction
# cxx_variable_templates
# cxx_std_17

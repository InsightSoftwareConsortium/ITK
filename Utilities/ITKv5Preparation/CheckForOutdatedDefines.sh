#!/bin/bash
# \author Hans J. Johnson
#
# This script simply checks a codebase for pre-processor
# macros that have been deprecated since ITKv5.
#
# This is intended to assist developers with
# cleaning up and removing conditional code
# that now consistently set.


for oudatedvalue in \
ITK_HAS_CXX11_STATIC_ASSERT \
ITK_HAS_CPP11_ALIGNAS \
ITK_HAS_CPP11_TYPETRAITS \
ITK_HAS_CXX11_RVREF \
ITK_HAS_STD_COPY_N \
ITK_SUPPORTS_TEMPLATED_FRIEND_FUNCTION_WITH_TEMPLATE_ARGUMENTS \
ITK_SUPPORTS_TEMPLATED_FRIEND_FUNCTION_WITH_EMPTY_BRACKETS \
ITK_SUPPORTS_TEMPLATED_FRIEND_FUNCTION_WITH_NULL_STRING \
ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENT \
ITK_HAS_STLTR1_TR1_TYPE_TRAITS \
ITK_HAS_STLTR1_TYPE_TRAITS \
; do
  echo "FOUND OUTDATED CONDITIONAL ${oudatedvalue} in:======================================="
  git grep -l ${oudatedvalue} |fgrep -v  ITKv5Preparation
done

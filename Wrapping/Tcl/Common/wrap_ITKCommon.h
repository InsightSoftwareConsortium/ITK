#ifndef _wrap_ITKCommon_h
#define _wrap_ITKCommon_h

#define ITK_WRAP_PACKAGE "ITKCommonTcl"
#define ITK_WRAP_PACKAGE_VERSION "0.7"
#define ITK_WRAP_GROUP(x) ITK_WRAP_PACKAGE #x

#define ITK_WRAP_BASE_TYPEDEF(x) \
  typedef ::itk::x x; \
  typedef ::itk::x::Pointer x##_Pointer

#define ITK_WRAP_BASE_SIZEOF(x) \
  sizeof(::_cable_::wrappers::itk::x); \
  sizeof(::_cable_::wrappers::itk::x##_Pointer)

#endif

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKMacro.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _wrap_ITKMacro_h
#define _wrap_ITKMacro_h

// The ITK Tcl package version number.
#define ITK_WRAP_PACKAGE_VERSION "0.7"

// Useful group reference macro.
#define ITK_WRAP_GROUP(x) ITK_WRAP_PACKAGE "_" #x

// Helper to wrap a non-templated class.
#define ITK_WRAP_OBJECT_TYPEDEF(x) \
  typedef ::itk::x x; \
  typedef ::itk::x::Pointer x##_Pointer

// Helper to wrap a non-templated class.
#define ITK_WRAP_OBJECT_SIZEOF(x) \
  sizeof(x); \
  sizeof(x##_Pointer)

// Wrap a class that takes one image argument.
#define ITK_WRAP_IMAGE_SINK(s) ITK_WRAP_IMAGE_SOURCE(s)
#define ITK_WRAP_IMAGE_SOURCE(s) \
  namespace _cable_ \
  { \
    const char* const group = ITK_WRAP_GROUP(itk##s); \
    typedef ::itk::Image<float, 2> ImageF2; \
    typedef ::itk::Image<float, 3> ImageF3; \
    typedef ::itk::Image<unsigned short, 2> ImageUS2; \
    typedef ::itk::Image<unsigned short, 3> ImageUS3; \
    namespace wrappers \
    { \
      namespace itk \
      { \
        ITK_WRAP_IMAGE_SOURCE_TYPEDEF(s, F2); \
        ITK_WRAP_IMAGE_SOURCE_TYPEDEF(s, F3); \
        ITK_WRAP_IMAGE_SOURCE_TYPEDEF(s, US2); \
        ITK_WRAP_IMAGE_SOURCE_TYPEDEF(s, US3); \
      } \
    } \
  } \
  void force_instantiate() \
  { \
    using namespace _cable_::wrappers::itk; \
    ITK_WRAP_IMAGE_SOURCE_SIZEOF(s, F2); \
    ITK_WRAP_IMAGE_SOURCE_SIZEOF(s, F3); \
    ITK_WRAP_IMAGE_SOURCE_SIZEOF(s, US2); \
    ITK_WRAP_IMAGE_SOURCE_SIZEOF(s, US3); \
  }

#define ITK_WRAP_IMAGE_SOURCE_TYPEDEF(s, x) \
  typedef ::itk::s< Image##x > \
          s##x; \
  typedef s##x::Pointer s##x##_Pointer

#define ITK_WRAP_IMAGE_SOURCE_SIZEOF(s, x) \
  sizeof(s##x); \
  sizeof(s##x##_Pointer)

// Wrap a class that takes two image arguments.
#define ITK_WRAP_IMAGE_TO_IMAGE(f) \
  namespace _cable_ \
  { \
    const char* const group = ITK_WRAP_GROUP(itk##f); \
    typedef ::itk::Image<float, 2> ImageF2; \
    typedef ::itk::Image<float, 3> ImageF3; \
    typedef ::itk::Image<unsigned short, 2> ImageUS2; \
    typedef ::itk::Image<unsigned short, 3> ImageUS3; \
    namespace wrappers \
    { \
      namespace itk \
      { \
        ITK_WRAP_IMAGE_TO_IMAGE_TYPEDEF(f, F2); \
        ITK_WRAP_IMAGE_TO_IMAGE_TYPEDEF(f, F3); \
        ITK_WRAP_IMAGE_TO_IMAGE_TYPEDEF(f, US2); \
        ITK_WRAP_IMAGE_TO_IMAGE_TYPEDEF(f, US3); \
      } \
    } \
  } \
  void force_instantiate() \
  { \
    using namespace _cable_::wrappers::itk; \
    ITK_WRAP_IMAGE_TO_IMAGE_SIZEOF(f, F2); \
    ITK_WRAP_IMAGE_TO_IMAGE_SIZEOF(f, F3); \
    ITK_WRAP_IMAGE_TO_IMAGE_SIZEOF(f, US2); \
    ITK_WRAP_IMAGE_TO_IMAGE_SIZEOF(f, US3); \
  }

#define ITK_WRAP_IMAGE_TO_IMAGE_TYPEDEF(f, x) \
  typedef ::itk::f< Image##x , Image##x > \
          f##x; \
  typedef f##x::Pointer f##x##_Pointer

#define ITK_WRAP_IMAGE_TO_IMAGE_SIZEOF(f, x) \
  sizeof(f##x); \
  sizeof(f##x##_Pointer)

#endif

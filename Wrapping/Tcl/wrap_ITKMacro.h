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

#include "itkConfigure.h"

// The ITK Tcl package version number.
#define ITK_WRAP_PACKAGE_VERSION ITK_VERSION_STRING

// Useful group reference macro.
#define ITK_WRAP_GROUP(x) ITK_WRAP_PACKAGE "_" #x

// CSWIG does not support wrapper namespaces
// Wrapper configuration namespace helpers.
#ifdef CSWIG
#  define ITK_WRAP_NAMESPACE_OPEN \
  namespace _cable_ { namespace wrappers {
#  define ITK_WRAP_NAMESPACE_CLOSE } } 
#  define ITK_WRAP_NAMESPACE_USING using namespace _cable_::wrappers;
# 
// Cable supports internal namespaces
#else
#  define ITK_WRAP_NAMESPACE_OPEN \
namespace _cable_ { namespace wrappers { namespace itk {
#  define ITK_WRAP_NAMESPACE_CLOSE } } }
#  define ITK_WRAP_NAMESPACE_USING using namespace _cable_::wrappers::itk;
#endif

// Configure the group setting for a group of wrappers.
#define ITK_WRAP_CONFIG_GROUP(g) \
  namespace _cable_ \
  { \
    const char* const group = ITK_WRAP_GROUP(g); \
  }

// Define the image typedefs needed for other image wrappers.
#define ITK_WRAP_DEFINE_IMAGE_TYPES() \
  namespace _cable_ \
  { \
    typedef ::itk::Image<float, 2> ImageF2; \
    typedef ::itk::Image<float, 3> ImageF3; \
    typedef ::itk::Image<unsigned short, 2> ImageUS2; \
    typedef ::itk::Image<unsigned short, 3> ImageUS3; \
    typedef ::itk::Image<unsigned char, 2> ImageUC2; \
  }

// Wrap a non-templated class that is not part of main hierarchy.
#define ITK_WRAP_CLASS(x) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::x x; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##x() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(x); \
  }

// Wrap a class with one template argument that is not part of main hierarchy.
#define ITK_WRAP_CLASS_TEMPLATE_1(x, c1) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::c1 x; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##x() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(x); \
  }

// Wrap a class with two template arguments that is not part of main hierarchy.
#define ITK_WRAP_CLASS_TEMPLATE_2(x, c1,c2) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::c1,c2 x; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##x() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(x); \
  }

// Wrap a non-templated subclass of itk::LightObject.
#define ITK_WRAP_OBJECT(x) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::x x; \
    typedef ::itk::x::Pointer x##_Pointer; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##x() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(x); \
    sizeof(x##_Pointer); \
  }

// Wrap a subclass of itk::LightObject with one template argument.
#define ITK_WRAP_OBJECT_TEMPLATE_1(x, c1) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::c1 x; \
    typedef x::Pointer x##_Pointer; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##x() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(x); \
    sizeof(x##_Pointer); \
  }

// Wrap a subclass of itk::LightObject with two template arguments.
#define ITK_WRAP_OBJECT_TEMPLATE_2(x, c1,c2) \
  ITK_WRAP_NAMESPACE_OPEN \
      typedef ::itk::c1,c2 x; \
      typedef x::Pointer x##_Pointer; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##x() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(x); \
    sizeof(x##_Pointer); \
  }

// Wrap a subclass of itk::LightObject with two template arguments.
#define ITK_WRAP_OBJECT_TEMPLATE_3(x, c1,c2,c3) \
  ITK_WRAP_NAMESPACE_OPEN \
      typedef ::itk::c1,c2,c3 x; \
      typedef x::Pointer x##_Pointer; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##x() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(x); \
    sizeof(x##_Pointer); \
  }

// Wrap a class taking two image type arguments.
#define ITK_WRAP_IMAGE_TO_IMAGE(f, x, y) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::f< Image##x , Image##y > f##x##y; \
    typedef f##x##y::Pointer f##x##y##_Pointer; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##f##x##y() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(f##x##y); \
    sizeof(f##x##y##_Pointer); \
  }

// Wrap the superclass of a class taking two image type arguments.
#define ITK_WRAP_IMAGE_TO_IMAGE_SUPERCLASS(f, x, y) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::f< Image##x , Image##y >::Superclass \
              f##x##y##_Superclass; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##f##x##y##_Superclass() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(f##x##y##_Superclass); \
  }

// Wrap a class one image type argument.
#define ITK_WRAP_IMAGE_TO_DEFAULT_IMAGE(f, x) ITK_WRAP_IMAGE_SOURCE(f, x)
#define ITK_WRAP_IMAGE_SINK(f, x) ITK_WRAP_IMAGE_SOURCE(f, x)
#define ITK_WRAP_IMAGE_SOURCE(f, x) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::f< Image##x > f##x; \
    typedef f##x::Pointer f##x##_Pointer; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##f##x() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(f##x); \
    sizeof(f##x##_Pointer); \
  }

// Wrap a class taking two of the same image type arguments.
#define ITK_WRAP_IMAGE_TO_SAME_IMAGE(f, x) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::f< Image##x , Image##x > f##x; \
    typedef f##x::Pointer f##x##_Pointer; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##f##x() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(f##x); \
    sizeof(f##x##_Pointer); \
  }

// Wrap the superclass of a class taking two of the same image type
// arguments.
#define ITK_WRAP_IMAGE_TO_SAME_IMAGE_SUPERCLASS(f, x) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::f< Image##x , Image##x >::Superclass f##x##_Superclass; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##f##x##_Superclass() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(f##x##_Superclass); \
  }

// Wrap a class taking three of the same image type arguments and one
// other image type argument.
#define ITK_WRAP_TERNARY_IMAGE_TO_IMAGE(f, x, y) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::f< Image##x , Image##x , Image##x , Image##y> f##x##y; \
    typedef f##x##y::Pointer f##x##y##_Pointer; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##f##x##y() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(f##x##y); \
    sizeof(f##x##y##_Pointer); \
  }

// Wrap the superclass of a class taking three of the same image type
// arguments and one other image type argument.
#define ITK_WRAP_TERNARY_IMAGE_TO_IMAGE_SUPERCLASS(f, x, y) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::f< Image##x , Image##x , Image##x , Image##y>::Superclass \
              f##x##y##_Superclass; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##f##x##y##_Superclass() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(f##x##y##_Superclass); \
  }

// Wrap a calculator class.  This is a sink that is not a process object.
#define ITK_WRAP_IMAGE_CALCULATOR(f, x) \
  ITK_WRAP_NAMESPACE_OPEN \
    typedef ::itk::f< Image##x > f##x; \
    typedef f##x::Pointer f##x##_Pointer; \
  ITK_WRAP_NAMESPACE_CLOSE \
  void _cable_force_instantiate_##f##x() { \
    ITK_WRAP_NAMESPACE_USING \
    sizeof(f##x); \
    sizeof(f##x##_Pointer); \
  }

#endif

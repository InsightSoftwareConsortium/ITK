/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_VXLNumerics.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _wrap_VXLNumerics_h
#define _wrap_VXLNumerics_h

#include "itkCSwigMacros.h"


#define ITK_WRAP_VNL_TYPEDEF(type) \
  typedef ::type<double>                    type##_double; \
  typedef ::type<vcl_complex<double> >      type##_double_complex; \
  typedef ::type<float>                     type##_float; \
  typedef ::type<vcl_complex<float> >       type##_float_complex; \
  typedef ::type<int>                       type##_int; \
  typedef ::type<long>                      type##_long; \
  typedef ::type<long double>               type##_long_double; \
  typedef ::type<vcl_complex<long double> > type##_long_double_complex; \
  typedef ::type<signed char>               type##_schar; \
  typedef ::type<unsigned char>             type##_uchar; \
  typedef ::type<unsigned int>              type##_uint; \
  typedef ::type<unsigned long>             type##_ulong

#define ITK_WRAP_VNL_SIZEOF(type) \
  sizeof(type##_double); \
  sizeof(type##_double_complex); \
  sizeof(type##_float); \
  sizeof(type##_float_complex); \
  sizeof(type##_int); \
  sizeof(type##_long); \
  sizeof(type##_long_double); \
  sizeof(type##_long_double_complex); \
  sizeof(type##_schar); \
  sizeof(type##_uchar); \
  sizeof(type##_uint); \
  sizeof(type##_ulong)

#define ITK_WRAP_VNL(type) \
  namespace _cable_ \
  { \
    const char* const group = ITK_WRAP_GROUP(type); \
    namespace wrappers \
    { \
      ITK_WRAP_VNL_TYPEDEF(type); \
    } \
  } \
  void force_instantiate() \
  { \
    using namespace _cable_::wrappers; \
    ITK_WRAP_VNL_SIZEOF(type); \
  }

#endif

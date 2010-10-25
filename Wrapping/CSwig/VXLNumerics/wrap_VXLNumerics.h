/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
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

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkRandomImageSource.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRandomImageSource.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_RIS_TYPEDEF(x) \
  typedef ::itk::RandomImageSource< Image##x > RandomImageSource##x; \
  typedef RandomImageSource##x::Pointer RandomImageSource##x##_Pointer
#define ITK_WRAP_RIS_SIZEOF(x) \
  sizeof(RandomImageSource##x); \
  sizeof(RandomImageSource##x##_Pointer)

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkRandomImageSource);
  typedef ::itk::Image<float, 2> ImageF2;
  typedef ::itk::Image<float, 3> ImageF3;
  typedef ::itk::Image<unsigned short, 2> ImageUS2;
  typedef ::itk::Image<unsigned short, 3> ImageUS3;
  namespace wrappers
  {
    namespace itk
    {
      ITK_WRAP_RIS_TYPEDEF(F2);
      ITK_WRAP_RIS_TYPEDEF(F3);
      ITK_WRAP_RIS_TYPEDEF(US2);
      ITK_WRAP_RIS_TYPEDEF(US3);
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  ITK_WRAP_RIS_SIZEOF(F2);
  ITK_WRAP_RIS_SIZEOF(F3);
  ITK_WRAP_RIS_SIZEOF(US2);
  ITK_WRAP_RIS_SIZEOF(US3);
}

#endif

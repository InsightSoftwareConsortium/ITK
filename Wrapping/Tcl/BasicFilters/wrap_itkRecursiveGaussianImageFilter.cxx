/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkRecursiveGaussianImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRecursiveGaussianImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_RGIF_TYPEDEF(x) \
  typedef ::itk::RecursiveGaussianImageFilter< Image##x > \
          RecursiveGaussianImageFilter##x; \
  typedef RecursiveGaussianImageFilter##x::Pointer \
          RecursiveGaussianImageFilter##x##_Pointer
#define ITK_WRAP_RGIF_SIZEOF(x) \
  sizeof(RecursiveGaussianImageFilter##x); \
  sizeof(RecursiveGaussianImageFilter##x##_Pointer)

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkRecursiveGaussianImageFilter);
  typedef ::itk::Image<float, 2> ImageF2;
  typedef ::itk::Image<float, 3> ImageF3;
  typedef ::itk::Image<unsigned short, 2> ImageUS2;
  typedef ::itk::Image<unsigned short, 3> ImageUS3;
  namespace wrappers
  {
    namespace itk
    {
      ITK_WRAP_RGIF_TYPEDEF(F2);
      ITK_WRAP_RGIF_TYPEDEF(F3);
      ITK_WRAP_RGIF_TYPEDEF(US2);
      ITK_WRAP_RGIF_TYPEDEF(US3);
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  ITK_WRAP_RGIF_SIZEOF(F2);
  ITK_WRAP_RGIF_SIZEOF(F3);
  ITK_WRAP_RGIF_SIZEOF(US2);
  ITK_WRAP_RGIF_SIZEOF(US3);
}

#endif

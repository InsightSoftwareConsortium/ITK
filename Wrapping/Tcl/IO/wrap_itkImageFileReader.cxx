/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageFileReader.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileReader.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKIO.h"

#define ITK_WRAP_IFR_TYPEDEF(x) \
  typedef ::itk::ImageFileReader< Image##x > ImageFileReader##x; \
  typedef ImageFileReader##x::Pointer ImageFileReader##x##_Pointer
#define ITK_WRAP_IFR_SIZEOF(x) \
  sizeof(ImageFileReader##x); \
  sizeof(ImageFileReader##x##_Pointer)

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageFileReader);
  typedef ::itk::Image<float, 2> ImageF2;
  typedef ::itk::Image<float, 3> ImageF3;
  typedef ::itk::Image<unsigned short, 2> ImageUS2;
  typedef ::itk::Image<unsigned short, 3> ImageUS3;
  namespace wrappers
  {
    namespace itk
    {
      ITK_WRAP_IFR_TYPEDEF(F2);
      ITK_WRAP_IFR_TYPEDEF(F3);
      ITK_WRAP_IFR_TYPEDEF(US2);
      ITK_WRAP_IFR_TYPEDEF(US3);
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  ITK_WRAP_IFR_SIZEOF(F2);
  ITK_WRAP_IFR_SIZEOF(F3);
  ITK_WRAP_IFR_SIZEOF(US2);
  ITK_WRAP_IFR_SIZEOF(US3);
}

#endif

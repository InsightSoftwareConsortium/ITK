/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageFileWriter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileWriter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKIO.h"

#define ITK_WRAP_IFW_TYPEDEF(x) \
  typedef ::itk::ImageFileWriter< Image##x > ImageFileWriter##x; \
  typedef ImageFileWriter##x::Pointer ImageFileWriter##x##_Pointer
#define ITK_WRAP_IFW_SIZEOF(x) \
  sizeof(ImageFileWriter##x); \
  sizeof(ImageFileWriter##x##_Pointer)

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageFileWriter);
  typedef ::itk::Image<float, 2> ImageF2;
  typedef ::itk::Image<float, 3> ImageF3;
  typedef ::itk::Image<unsigned short, 2> ImageUS2;
  typedef ::itk::Image<unsigned short, 3> ImageUS3;
  namespace wrappers
  {
    namespace itk
    {
      ITK_WRAP_IFW_TYPEDEF(F2);
      ITK_WRAP_IFW_TYPEDEF(F3);
      ITK_WRAP_IFW_TYPEDEF(US2);
      ITK_WRAP_IFW_TYPEDEF(US3);
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  ITK_WRAP_IFW_SIZEOF(F2);
  ITK_WRAP_IFW_SIZEOF(F3);
  ITK_WRAP_IFW_SIZEOF(US2);
  ITK_WRAP_IFW_SIZEOF(US3);
}

#endif

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_IOBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMetaImageIOFactory.h"
#include "itkPNGImageIOFactory.h"
#include "itkPNGImageIO.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKIO.h"

#define ITK_WRAP_BASE_TYPEDEF(x) \
  typedef ::itk::x x; \
  typedef ::itk::x::Pointer x##_Pointer

#define ITK_WRAP_BASE_SIZEOF(x) \
  sizeof(x); \
  sizeof(x##_Pointer)

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(IOBase);
  namespace wrappers
  {
    namespace itk
    {
      ITK_WRAP_BASE_TYPEDEF(PNGImageIO);
      ITK_WRAP_BASE_TYPEDEF(MetaImageIOFactory);
      ITK_WRAP_BASE_TYPEDEF(PNGImageIOFactory);
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  ITK_WRAP_BASE_SIZEOF(PNGImageIO);
  ITK_WRAP_BASE_SIZEOF(MetaImageIOFactory);
  ITK_WRAP_BASE_SIZEOF(PNGImageIOFactory);
}

#endif

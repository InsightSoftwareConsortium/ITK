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

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(IOBase);
  namespace wrappers
  {
    namespace itk
    {
      ITK_WRAP_OBJECT_TYPEDEF(PNGImageIO);
      ITK_WRAP_OBJECT_TYPEDEF(MetaImageIOFactory);
      ITK_WRAP_OBJECT_TYPEDEF(PNGImageIOFactory);
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  ITK_WRAP_OBJECT_SIZEOF(PNGImageIO);
  ITK_WRAP_OBJECT_SIZEOF(MetaImageIOFactory);
  ITK_WRAP_OBJECT_SIZEOF(PNGImageIOFactory);
}

#endif

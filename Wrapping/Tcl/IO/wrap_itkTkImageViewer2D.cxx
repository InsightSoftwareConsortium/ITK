/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkTkImageViewer2D.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTkImageViewer2D.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKIO.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkTkImageViewer2D);
  namespace wrappers
  {
    namespace itk
    {
      typedef ::itk::TkImageViewer2D TkImageViewer2D;
      typedef TkImageViewer2D::Pointer TkImageViewer2D_Pointer;
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  sizeof(TkImageViewer2D);
  sizeof(TkImageViewer2D_Pointer);
}

#endif

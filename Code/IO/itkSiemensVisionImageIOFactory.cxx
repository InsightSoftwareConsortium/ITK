/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSiemensVisionImageIOFactory.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkSiemensVisionImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkSiemensVisionImageIO.h"
#include "itkVersion.h"


namespace itk
{
  void SiemensVisionImageIOFactory::PrintSelf(std::ostream&, Indent) const
  {

  }


  SiemensVisionImageIOFactory::SiemensVisionImageIOFactory()
  {
    this->RegisterOverride("itkImageIOBase",
        "itkSiemensVisionImageIO",
        "SiemensVision Image IO",
        1,
        CreateObjectFunction<SiemensVisionImageIO>::New());
  }

  SiemensVisionImageIOFactory::~SiemensVisionImageIOFactory()
  {
  }

  const char*
    SiemensVisionImageIOFactory::GetITKSourceVersion(void) const
    {
      return ITK_SOURCE_VERSION;
    }

  const char*
    SiemensVisionImageIOFactory::GetDescription() const
    {
      return "SiemensVision ImageIO Factory, allows the loading of SiemensVision images into insight";
    }

} // end namespace itk


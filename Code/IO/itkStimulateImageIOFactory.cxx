/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStimulateImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkStimulateImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkStimulateImageIO.h"
#include "itkVersion.h"

namespace itk
{

StimulateImageIOFactory::StimulateImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkStimulateImageIO",
                         "Stimulate Image IO",
                         1,
                         CreateObjectFunction<StimulateImageIO>::New());
}
  
StimulateImageIOFactory::~StimulateImageIOFactory()
{
}

const char* 
StimulateImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char* 
StimulateImageIOFactory::GetDescription(void) const
{
  return "Stimulate ImageIO Factory, allows the loading of Stimulate images into ITK";
}

} // end namespace itk


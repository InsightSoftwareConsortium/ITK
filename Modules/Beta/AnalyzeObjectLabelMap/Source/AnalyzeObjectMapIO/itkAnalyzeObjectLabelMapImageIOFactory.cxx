/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    $RCSfile: itkAnalyzeObjectLabelMapImageIOFactory.cxx,v $
Language:  C++
Date:      $Date: 2007/03/22 14:28:51 $
Version:   $Revision: 1.2 $

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkAnalyzeObjectLabelMapImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkAnalyzeObjectLabelMapImageIO.h"
#include "itkVersion.h"


namespace itk
{
void AnalyzeObjectLabelMapImageIOFactory::PrintSelf(std::ostream&, Indent) const
{
}


AnalyzeObjectLabelMapImageIOFactory::AnalyzeObjectLabelMapImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkAnalyzeObjectLabelMapImageIO",
                         "Anaylze Object Label Map IO",
                         1,
                         CreateObjectFunction<AnalyzeObjectLabelMapImageIO>::New());
}

AnalyzeObjectLabelMapImageIOFactory::~AnalyzeObjectLabelMapImageIOFactory()
{
}

const char*
AnalyzeObjectLabelMapImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char*
AnalyzeObjectLabelMapImageIOFactory::GetDescription() const
{
  return "Anaylyze Object Map ImageIO Factory, allows the loading of Object Maps images into insight";
}

} // end namespace itk

/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkniftiImageIOFactory.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkniftiImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkniftiImageIO.h"
#include "itkVersion.h"


namespace itk
{
void niftiImageIOFactory::PrintSelf(std::ostream&, Indent) const
{

}


niftiImageIOFactory::niftiImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkniftiImageIO",
                         "nifti Image IO",
                         1,
                         CreateObjectFunction<niftiImageIO>::New());
}

niftiImageIOFactory::~niftiImageIOFactory()
{
}

const char*
niftiImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char*
niftiImageIOFactory::GetDescription() const
{
  return "nifti ImageIO Factory, allows the loading of nifti images into insight";
}

} // end namespace itk


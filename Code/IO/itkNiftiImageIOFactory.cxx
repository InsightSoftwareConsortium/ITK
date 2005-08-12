/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkNiftiImageIOFactory.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNiftiImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkNiftiImageIO.h"
#include "itkVersion.h"


namespace itk
{
void NiftiImageIOFactory::PrintSelf(std::ostream&, Indent) const
{

}


NiftiImageIOFactory::NiftiImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkNiftiImageIO",
                         "Nifti Image IO",
                         1,
                         CreateObjectFunction<NiftiImageIO>::New());
}

NiftiImageIOFactory::~NiftiImageIOFactory()
{
}

const char*
NiftiImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char*
NiftiImageIOFactory::GetDescription() const
{
  return "Nifti ImageIO Factory, allows the loading of Nifti images into insight";
}

} // end namespace itk


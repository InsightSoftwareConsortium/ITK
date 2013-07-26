/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkVTKImageIOFactory.h"
#include "itkVTKImageIO.h"
#include "itkVersion.h"

namespace itk
{
VTKImageIOFactory::VTKImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkVTKImageIO",
                          "VTK Image IO",
                          1,
                          CreateObjectFunction< VTKImageIO >::New() );
}

VTKImageIOFactory::~VTKImageIOFactory()
{}

const char *
VTKImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
VTKImageIOFactory::GetDescription(void) const
{
  return "VTK ImageIO Factory, allows the loading of VTK images into ITK";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool VTKImageIOFactoryHasBeenRegistered;

void ITKIOVTK_EXPORT VTKImageIOFactoryRegister__Private(void)
{
  if( ! VTKImageIOFactoryHasBeenRegistered )
    {
    VTKImageIOFactoryHasBeenRegistered = true;
    VTKImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk

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
#include "itkLSMImageIOFactory.h"
#include "itkLSMImageIO.h"
#include "itkVersion.h"

namespace itk
{
LSMImageIOFactory::LSMImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkLSMImageIO",
                          "LSM Image IO",
                          1,
                          CreateObjectFunction< LSMImageIO >::New() );
}

LSMImageIOFactory::~LSMImageIOFactory()
{}

const char *
LSMImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
LSMImageIOFactory::GetDescription() const
{
  return "LSM ImageIO Factory, allows the loading of LSM images into ITK";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool LSMImageIOFactoryHasBeenRegistered;

void ITKIOLSM_EXPORT LSMImageIOFactoryRegister__Private(void)
{
  if( ! LSMImageIOFactoryHasBeenRegistered )
    {
    LSMImageIOFactoryHasBeenRegistered = true;
    LSMImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk

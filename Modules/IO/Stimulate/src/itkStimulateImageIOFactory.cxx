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
#include "itkStimulateImageIOFactory.h"
#include "itkStimulateImageIO.h"
#include "itkVersion.h"

namespace itk
{
StimulateImageIOFactory::StimulateImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkStimulateImageIO",
                          "Stimulate Image IO",
                          1,
                          CreateObjectFunction< StimulateImageIO >::New() );
}

StimulateImageIOFactory::~StimulateImageIOFactory()
{}

const char *
StimulateImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
StimulateImageIOFactory::GetDescription(void) const
{
  return "Stimulate ImageIO Factory, allows the loading of Stimulate images into ITK";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool StimulateImageIOFactoryHasBeenRegistered;

void ITKIOStimulate_EXPORT StimulateImageIOFactoryRegister__Private(void)
{
  if( ! StimulateImageIOFactoryHasBeenRegistered )
    {
    StimulateImageIOFactoryHasBeenRegistered = true;
    StimulateImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk

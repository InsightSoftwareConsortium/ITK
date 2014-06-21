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
#include "itkDCMTKImageIOFactory.h"
#include "itkDCMTKImageIO.h"
#include "itkVersion.h"

namespace itk
{
DCMTKImageIOFactory::DCMTKImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkDCMTKImageIO",
                          "DICOM Image IO",
                          1,
                          CreateObjectFunction< DCMTKImageIO >::New() );
}

DCMTKImageIOFactory::~DCMTKImageIOFactory()
{}

const char *
DCMTKImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
DCMTKImageIOFactory::GetDescription() const
{
  return "DCMTK ImageIO Factory, allows the loading of DICOM images into Insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool DCMTKImageIOFactoryHasBeenRegistered;

void ITKIODCMTK_EXPORT DCMTKImageIOFactoryRegister__Private(void)
{
  if( ! DCMTKImageIOFactoryHasBeenRegistered )
    {
    DCMTKImageIOFactoryHasBeenRegistered = true;
    DCMTKImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk

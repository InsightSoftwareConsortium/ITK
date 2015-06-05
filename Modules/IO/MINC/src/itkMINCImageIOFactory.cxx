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
#include "itkMINCImageIOFactory.h"
#include "itkMINCImageIO.h"
#include "itkVersion.h"

namespace itk
{
MINCImageIOFactory::MINCImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkMINCImageIO",
                          "MINC Image IO",
                          1,
                          CreateObjectFunction< MINCImageIO >::New() );
}

MINCImageIOFactory::~MINCImageIOFactory()
{
}

const char *
MINCImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
MINCImageIOFactory::GetDescription() const
{
  return "MINC ImageIO Factory, allows the loading of MINC images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool MINCImageIOFactoryHasBeenRegistered;

void ITKIOMINC_EXPORT MINCImageIOFactoryRegister__Private(void)
{
  if( !MINCImageIOFactoryHasBeenRegistered )
    {
    MINCImageIOFactoryHasBeenRegistered = true;
    MINCImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk

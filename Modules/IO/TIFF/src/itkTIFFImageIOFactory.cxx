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
#include "itkTIFFImageIOFactory.h"
#include "itkTIFFImageIO.h"
#include "itkVersion.h"

namespace itk
{
TIFFImageIOFactory::TIFFImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkTIFFImageIO",
                          "TIFF Image IO",
                          1,
                          CreateObjectFunction< TIFFImageIO >::New() );
}

TIFFImageIOFactory::~TIFFImageIOFactory()
{}

const char *
TIFFImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
TIFFImageIOFactory::GetDescription(void) const
{
  return "TIFF ImageIO Factory, allows the loading of TIFF images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool TIFFImageIOFactoryHasBeenRegistered;

void ITKIOTIFF_EXPORT TIFFImageIOFactoryRegister__Private(void)
{
  if( ! TIFFImageIOFactoryHasBeenRegistered )
    {
    TIFFImageIOFactoryHasBeenRegistered = true;
    TIFFImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk

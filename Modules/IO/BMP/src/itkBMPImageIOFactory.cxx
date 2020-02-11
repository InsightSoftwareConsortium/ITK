/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkBMPImageIOFactory.h"
#include "itkBMPImageIO.h"
#include "itkVersion.h"

namespace itk
{
BMPImageIOFactory::BMPImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkBMPImageIO", "BMP Image IO", true, CreateObjectFunction<BMPImageIO>::New());
}

BMPImageIOFactory::~BMPImageIOFactory() = default;

const char *
BMPImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
BMPImageIOFactory::GetDescription() const
{
  return "BMP ImageIO Factory, allows the loading of BMP images into Insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool BMPImageIOFactoryHasBeenRegistered;

void ITKIOBMP_EXPORT
     BMPImageIOFactoryRegister__Private()
{
  if (!BMPImageIOFactoryHasBeenRegistered)
  {
    BMPImageIOFactoryHasBeenRegistered = true;
    BMPImageIOFactory::RegisterOneFactory();
  }
}

} // end namespace itk

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
#include "itkPNGImageIOFactory.h"
#include "itkPNGImageIO.h"
#include "itkVersion.h"

namespace itk
{
PNGImageIOFactory::PNGImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkPNGImageIO", "PNG Image IO", true, CreateObjectFunction<PNGImageIO>::New());
}

PNGImageIOFactory::~PNGImageIOFactory() = default;

const char *
PNGImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
PNGImageIOFactory::GetDescription() const
{
  return "PNG ImageIO Factory, allows the loading of PNG images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool PNGImageIOFactoryHasBeenRegistered;

void ITKIOPNG_EXPORT
     PNGImageIOFactoryRegister__Private()
{
  if (!PNGImageIOFactoryHasBeenRegistered)
  {
    PNGImageIOFactoryHasBeenRegistered = true;
    PNGImageIOFactory::RegisterOneFactory();
  }
}

} // end namespace itk

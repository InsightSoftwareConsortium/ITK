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
#include "itkJPEG2000ImageIOFactory.h"
#include "itkJPEG2000ImageIO.h"
#include "itkVersion.h"

namespace itk
{
JPEG2000ImageIOFactory::JPEG2000ImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkJPEG2000ImageIO", "JPEG2000 Image IO", true, CreateObjectFunction<JPEG2000ImageIO>::New());
}

JPEG2000ImageIOFactory::~JPEG2000ImageIOFactory() = default;

const char *
JPEG2000ImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
JPEG2000ImageIOFactory::GetDescription() const
{
  return "JPEG2000 ImageIO Factory, allows the loading of JPEG2000 images into insight";
}


// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool JPEG2000ImageIOFactoryHasBeenRegistered;

void ITKIOJPEG2000_EXPORT
     JPEG2000ImageIOFactoryRegister__Private()
{
  if (!JPEG2000ImageIOFactoryHasBeenRegistered)
  {
    JPEG2000ImageIOFactoryHasBeenRegistered = true;
    JPEG2000ImageIOFactory::RegisterOneFactory();
  }
}


} // end namespace itk

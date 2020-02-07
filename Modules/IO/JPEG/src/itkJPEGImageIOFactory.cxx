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
#include "itkJPEGImageIOFactory.h"
#include "itkJPEGImageIO.h"
#include "itkVersion.h"

namespace itk
{
JPEGImageIOFactory::JPEGImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkJPEGImageIO", "JPEG Image IO", true, CreateObjectFunction<JPEGImageIO>::New());
}

JPEGImageIOFactory::~JPEGImageIOFactory() = default;

const char *
JPEGImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
JPEGImageIOFactory::GetDescription() const
{
  return "JPEG ImageIO Factory, allows the loading of JPEG images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool JPEGImageIOFactoryHasBeenRegistered;

void ITKIOJPEG_EXPORT
     JPEGImageIOFactoryRegister__Private()
{
  if (!JPEGImageIOFactoryHasBeenRegistered)
  {
    JPEGImageIOFactoryHasBeenRegistered = true;
    JPEGImageIOFactory::RegisterOneFactory();
  }
}

} // end namespace itk

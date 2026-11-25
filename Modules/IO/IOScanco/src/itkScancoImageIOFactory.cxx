/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkScancoImageIOFactory.h"
#include "itkScancoImageIO.h"
#include "itkVersion.h"

namespace itk
{
ScancoImageIOFactory::ScancoImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkScancoImageIO", "Scanco Image IO", true, CreateObjectFunction<ScancoImageIO>::New());
}

const char *
ScancoImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
ScancoImageIOFactory::GetDescription() const
{
  return "Scanco ImageIO Factory, allows the loading of Scanco images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool ScancoImageIOFactoryHasBeenRegistered;

void IOScanco_EXPORT
ScancoImageIOFactoryRegister__Private()
{
  if (!ScancoImageIOFactoryHasBeenRegistered)
  {
    ScancoImageIOFactoryHasBeenRegistered = true;
    ScancoImageIOFactory::RegisterOneFactory();
  }
}

} // end namespace itk

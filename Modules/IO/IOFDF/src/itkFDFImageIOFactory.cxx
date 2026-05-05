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

#include "itkFDFImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkFDFImageIO.h"
#include "itkVersion.h"


namespace itk
{
FDFImageIOFactory::FDFImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkFDFImageIO", "FDF Image IO", true, CreateObjectFunction<FDFImageIO>::New());
}

FDFImageIOFactory::~FDFImageIOFactory() = default;

const char *
FDFImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
FDFImageIOFactory::GetDescription() const
{
  return "FDF ImageIO Factory, allows the loading of Varian FDF images into Insight";
}


// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool FDFImageIOFactoryHasBeenRegistered;

void IOFDF_EXPORT
FDFImageIOFactoryRegister__Private()
{
  if (!FDFImageIOFactoryHasBeenRegistered)
  {
    FDFImageIOFactoryHasBeenRegistered = true;
    FDFImageIOFactory::RegisterOneFactory();
  }
}


} // end namespace itk

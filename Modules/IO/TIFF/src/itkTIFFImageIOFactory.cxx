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
#include "itkTIFFImageIOFactory.h"
#include "itkTIFFImageIO.h"
#include "itkVersion.h"

namespace itk
{
TIFFImageIOFactory::TIFFImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkTIFFImageIO", "TIFF Image IO", true, CreateObjectFunction<TIFFImageIO>::New());
}

TIFFImageIOFactory::~TIFFImageIOFactory() = default;

const char *
TIFFImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
TIFFImageIOFactory::GetDescription() const
{
  return "TIFF ImageIO Factory, allows the loading of TIFF images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void ITKIOTIFF_EXPORT
     TIFFImageIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<TIFFImageIOFactory>();
}

} // end namespace itk

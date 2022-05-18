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
#include "itkGiplImageIOFactory.h"
#include "itkGiplImageIO.h"
#include "itkVersion.h"

namespace itk
{
GiplImageIOFactory::GiplImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkGiplImageIO", "Gipl Image IO", true, CreateObjectFunction<GiplImageIO>::New());
}

GiplImageIOFactory::~GiplImageIOFactory() = default;

const char *
GiplImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
GiplImageIOFactory::GetDescription() const
{
  return "Gipl ImageIO Factory, allows the loading of Gipl images into Insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void ITKIOGIPL_EXPORT
     GiplImageIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<GiplImageIOFactory>();
}

} // end namespace itk

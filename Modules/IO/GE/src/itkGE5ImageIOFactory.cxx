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
#include "itkGE5ImageIOFactory.h"
#include "itkGE5ImageIO.h"
#include "itkVersion.h"

namespace itk
{
void
GE5ImageIOFactory::PrintSelf(std::ostream &, Indent) const
{}

GE5ImageIOFactory::GE5ImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkGE5ImageIO", "GE5 Image IO", true, CreateObjectFunction<GE5ImageIO>::New());
}

GE5ImageIOFactory::~GE5ImageIOFactory() = default;

const char *
GE5ImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
GE5ImageIOFactory::GetDescription() const
{
  return "GE5 ImageIO Factory, allows the loading of GE5 images into ITK";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void ITKIOGE_EXPORT
     GE5ImageIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<GE5ImageIOFactory>();
}

} // end namespace itk

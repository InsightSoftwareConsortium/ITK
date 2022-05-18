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
#include "itkGEAdwImageIOFactory.h"
#include "itkGEAdwImageIO.h"
#include "itkVersion.h"

namespace itk
{
void
GEAdwImageIOFactory::PrintSelf(std::ostream &, Indent) const
{}

GEAdwImageIOFactory::GEAdwImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkGEAdwImageIO", "GEAdw Image IO", true, CreateObjectFunction<GEAdwImageIO>::New());
}

GEAdwImageIOFactory::~GEAdwImageIOFactory() = default;

const char *
GEAdwImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
GEAdwImageIOFactory::GetDescription() const
{
  return "GEAdw ImageIO Factory, allows the loading of GEAdw images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void ITKIOGE_EXPORT
     GEAdwImageIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<GEAdwImageIOFactory>();
}

} // end namespace itk

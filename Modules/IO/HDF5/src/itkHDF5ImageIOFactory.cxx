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
#include "itkHDF5ImageIOFactory.h"
#include "itkHDF5ImageIO.h"
#include "itkVersion.h"

namespace itk
{
void
HDF5ImageIOFactory::PrintSelf(std::ostream &, Indent) const
{}

HDF5ImageIOFactory::HDF5ImageIOFactory()
{
  this->RegisterOverride(
    "itkImageIOBase", "itkHDF5ImageIO", "HDF5 Image IO", true, CreateObjectFunction<HDF5ImageIO>::New());
}

HDF5ImageIOFactory::~HDF5ImageIOFactory() = default;

const char *
HDF5ImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
HDF5ImageIOFactory::GetDescription() const
{
  return "HDF5 ImageIO Factory, allows the loading of HDF5 images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void ITKIOHDF5_EXPORT
     HDF5ImageIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<HDF5ImageIOFactory>();
}

} // end namespace itk

/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkHDF5TransformIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkHDF5TransformIO.h"
#include "itkVersion.h"

namespace itk
{
void HDF5TransformIOFactory::PrintSelf(std::ostream &, Indent) const
{}

HDF5TransformIOFactory::HDF5TransformIOFactory()
{
  this->RegisterOverride( "itkTransformIOBaseTemplate",
                          "itkHDF5TransformIO",
                          "HD5 Transform float IO",
                          1,
                          CreateObjectFunction< HDF5TransformIOTemplate< float > >::New() );

  this->RegisterOverride( "itkTransformIOBaseTemplate",
                          "itkHDF5TransformIO",
                          "HD5 Transform double IO",
                          1,
                          CreateObjectFunction< HDF5TransformIOTemplate< double > >::New() );
}

HDF5TransformIOFactory::~HDF5TransformIOFactory()
{}

const char *
HDF5TransformIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
HDF5TransformIOFactory::GetDescription() const
{
  return "HD5 TransformIO Factory, allows the"
         " loading of HDF5 transforms into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
static bool HDF5TransformIOFactoryHasBeenRegistered;

void ITKIOTransformHDF5_EXPORT HDF5TransformIOFactoryRegister__Private(void)
{
  if( ! HDF5TransformIOFactoryHasBeenRegistered )
    {
    HDF5TransformIOFactoryHasBeenRegistered = true;
    HDF5TransformIOFactory::RegisterOneFactory();
    }
}
} // end namespace itk

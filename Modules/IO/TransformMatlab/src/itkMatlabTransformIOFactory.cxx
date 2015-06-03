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
#include "itkMatlabTransformIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkMatlabTransformIO.h"
#include "itkVersion.h"

namespace itk
{
void MatlabTransformIOFactory::PrintSelf(std::ostream &, Indent) const
{}

MatlabTransformIOFactory::MatlabTransformIOFactory()
{
  this->RegisterOverride( "itkTransformIOBaseTemplate",
                          "itkMatlabTransformIO",
                          "Matlab Transform float IO",
                          1,
                          CreateObjectFunction< MatlabTransformIOTemplate< float > >::New() );
  this->RegisterOverride( "itkTransformIOBaseTemplate",
                          "itkMatlabTransformIO",
                          "Matlab Transform double IO",
                          1,
                          CreateObjectFunction< MatlabTransformIOTemplate< double >  >::New() );
}

MatlabTransformIOFactory::~MatlabTransformIOFactory()
{}

const char *
MatlabTransformIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
MatlabTransformIOFactory::GetDescription() const
{
  return "Matlab TransformIO Factory, allows the "
         "loading of Nifti images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
static bool MatlabTransformIOFactoryHasBeenRegistered;

void ITKIOTransformMatlab_EXPORT MatlabTransformIOFactoryRegister__Private(void)
{
  if( ! MatlabTransformIOFactoryHasBeenRegistered )
    {
    MatlabTransformIOFactoryHasBeenRegistered = true;
    MatlabTransformIOFactory::RegisterOneFactory();
    }
}
} // end namespace itk

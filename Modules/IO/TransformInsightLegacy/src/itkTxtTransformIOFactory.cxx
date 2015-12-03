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
#include "itkTxtTransformIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkTxtTransformIO.h"
#include "itkVersion.h"

namespace itk
{
void TxtTransformIOFactory::PrintSelf(std::ostream &, Indent) const
{}

TxtTransformIOFactory::TxtTransformIOFactory()
{
  this->RegisterOverride( "itkTransformIOBaseTemplate",
                          "itkTxtTransformIO",
                          "Txt Transform float IO",
                          1,
                          CreateObjectFunction< TxtTransformIOTemplate< float > >::New() );
  this->RegisterOverride( "itkTransformIOBaseTemplate",
                          "itkTxtTransformIO",
                          "Txt Transform double IO",
                          1,
                          CreateObjectFunction< TxtTransformIOTemplate< double > >::New() );
}

TxtTransformIOFactory::~TxtTransformIOFactory()
{}

const char *
TxtTransformIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
TxtTransformIOFactory::GetDescription() const
{
  return "Txt TransformIO Factory, allows the"
         " loading of Nifti images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
static bool TxtTransformIOFactoryHasBeenRegistered;

void ITKIOTransformInsightLegacy_EXPORT TxtTransformIOFactoryRegister__Private(void)
{
  if( ! TxtTransformIOFactoryHasBeenRegistered )
    {
    TxtTransformIOFactoryHasBeenRegistered = true;
    TxtTransformIOFactory::RegisterOneFactory();
    }
}
} // end namespace itk

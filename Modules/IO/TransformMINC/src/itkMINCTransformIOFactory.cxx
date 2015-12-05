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
#include "itkVersion.h"
#include "itkMINCTransformIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkMINCTransformIO.h"
#include "itkTransformFactory.h"

namespace itk
{

void MINCTransformIOFactory::PrintSelf(std::ostream &, Indent) const
{}

MINCTransformIOFactory::MINCTransformIOFactory()
{
  this->RegisterOverride( "itkTransformIOBaseTemplate",
                          "itkMINCTransformIO",
                          "MINC XFM Transform float IO",
                          1,
                          CreateObjectFunction< MINCTransformIOTemplate< float > >::New() );

  this->RegisterOverride( "itkTransformIOBaseTemplate",
                          "itkMINCTransformIO",
                          "MINC XFM Transform double IO",
                          1,
                          CreateObjectFunction< MINCTransformIOTemplate< double > >::New() );
}

MINCTransformIOFactory::~MINCTransformIOFactory()
{}

const char *
MINCTransformIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
MINCTransformIOFactory::GetDescription() const
{
  return "MINC XFM TransformIO Factory, allows the"
        " loading of Minc XFM transforms into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
static bool MINCTransformIOFactoryHasBeenRegistered;

void ITKIOTransformMINC_EXPORT MINCTransformIOFactoryRegister__Private(void)
{
  if( ! MINCTransformIOFactoryHasBeenRegistered )
    {
    MINCTransformIOFactoryHasBeenRegistered = true;
    MINCTransformIOFactory::RegisterOneFactory();

    //TransformFactory< DisplacementFieldTransform<double,3> >::RegisterTransform ();
    // register additional transform type
    }
}

} // end namespace itk

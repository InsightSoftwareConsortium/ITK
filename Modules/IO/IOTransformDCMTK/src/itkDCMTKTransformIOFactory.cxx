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
#include "itkDCMTKTransformIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkDCMTKTransformIO.h"
#include "itkVersion.h"

namespace itk
{

DCMTKTransformIOFactory ::DCMTKTransformIOFactory()
{
  this->RegisterOverride("itkTransformIOBaseTemplate",
                         "itkDCMTKTransformIO",
                         "DCMTK Transform float IO",
                         1,
                         CreateObjectFunction<DCMTKTransformIO<float>>::New());

  this->RegisterOverride("itkTransformIOBaseTemplate",
                         "itkDCMTKTransformIO",
                         "DCMTK Transform double IO",
                         1,
                         CreateObjectFunction<DCMTKTransformIO<double>>::New());
}

DCMTKTransformIOFactory ::~DCMTKTransformIOFactory() {}

const char *
DCMTKTransformIOFactory ::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
DCMTKTransformIOFactory ::GetDescription() const
{
  return "DCMTK TransformIO Factory, allows the"
         " loading of DICOM transforms into Insight";
}

void
DCMTKTransformIOFactory ::PrintSelf(std::ostream &, Indent) const
{}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
static bool DCMTKTransformIOFactoryHasBeenRegistered;

void IOTransformDCMTK_EXPORT
DCMTKTransformIOFactoryRegister__Private(void)
{
  if (!DCMTKTransformIOFactoryHasBeenRegistered)
  {
    DCMTKTransformIOFactoryHasBeenRegistered = true;
    DCMTKTransformIOFactory::RegisterOneFactory();
  }
}

} // end namespace itk

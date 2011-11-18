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
#include "itkOpenCVVideoIOFactory.h"
#include "itkOpenCVVideoIO.h"
#include "itkVersion.h"

namespace itk
{
OpenCVVideoIOFactory::OpenCVVideoIOFactory()
{
  this->RegisterOverride( "itkVideoIOBase",
                          "itkOpenCVVideoIO",
                          "OpenCV Video IO",
                          1,
                          CreateObjectFunction< OpenCVVideoIO >::New() );
}

OpenCVVideoIOFactory::~OpenCVVideoIOFactory()
{}

const char *
OpenCVVideoIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
OpenCVVideoIOFactory::GetDescription() const
{
  return "OpenCV VideoIO Factory, allows the loading of AVI videos into Insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool OpenCVVideoIOFactoryHasBeenRegistered;

void OpenCVVideoIOFactoryRegister__Private(void)
{
  if( ! OpenCVVideoIOFactoryHasBeenRegistered )
    {
    OpenCVVideoIOFactoryHasBeenRegistered = true;
    OpenCVVideoIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk

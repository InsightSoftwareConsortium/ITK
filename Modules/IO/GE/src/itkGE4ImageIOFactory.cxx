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
#include "itkGE4ImageIOFactory.h"
#include "itkGE4ImageIO.h"
#include "itkVersion.h"

namespace itk
{
void GE4ImageIOFactory::PrintSelf(std::ostream &, Indent) const
{}

GE4ImageIOFactory::GE4ImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkGE4ImageIO",
                          "GE4 Image IO",
                          1,
                          CreateObjectFunction< GE4ImageIO >::New() );
}

GE4ImageIOFactory::~GE4ImageIOFactory()
{}

const char *
GE4ImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
GE4ImageIOFactory::GetDescription() const
{
  return "GE4 ImageIO Factory, allows the loading of GE4 images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool GE4ImageIOFactoryHasBeenRegistered;

void ITKIOGE_EXPORT GE4ImageIOFactoryRegister__Private(void)
{
  if( ! GE4ImageIOFactoryHasBeenRegistered )
    {
    GE4ImageIOFactoryHasBeenRegistered = true;
    GE4ImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk

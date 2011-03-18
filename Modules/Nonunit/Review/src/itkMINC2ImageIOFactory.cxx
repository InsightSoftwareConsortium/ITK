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
#include "itkMINC2ImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkMINC2ImageIO.h"
#include "itkVersion.h"

namespace itk
{
MINC2ImageIOFactory::MINC2ImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkMINC2ImageIO",
                          "MINC2 Image IO",
                          1,
                          CreateObjectFunction< MINC2ImageIO >::New() );
}

MINC2ImageIOFactory::~MINC2ImageIOFactory()
{}

const char *
MINC2ImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
MINC2ImageIOFactory::GetDescription(void) const
{
  return "MINC2 ImageIO Factory, allows the loading of MINC2 images into insight";
}
} // end namespace itk

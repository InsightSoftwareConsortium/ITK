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
#include "ITKIOPhilipsRECExport.h"

#include "itkPhilipsRECImageIOFactory.h"
#include "itkPhilipsRECImageIO.h"
#include "itkVersion.h"

/**
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/1381
 *
 */

namespace itk
{
PhilipsRECImageIOFactory::PhilipsRECImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkPhilipsRECImageIO",
                          "Philips REC Image IO",
                          1,
                          CreateObjectFunction< PhilipsRECImageIO >::New() );
}

PhilipsRECImageIOFactory::~PhilipsRECImageIOFactory()
{}

const char *
PhilipsRECImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
PhilipsRECImageIOFactory::GetDescription(void) const
{
  return "Philips REC ImageIO Factory, allows the loading of Philips REC images"
         " into Insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool PhilipsRECImageIOFactoryHasBeenRegistered;

void ITKIOPhilipsREC_EXPORT PhilipsRECImageIOFactoryRegister__Private(void)
{
  if( ! PhilipsRECImageIOFactoryHasBeenRegistered )
    {
    PhilipsRECImageIOFactoryHasBeenRegistered = true;
    PhilipsRECImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk

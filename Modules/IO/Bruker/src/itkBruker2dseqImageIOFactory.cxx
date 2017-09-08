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
#include "itkBruker2dseqImageIOFactory.h"
#include "itkBruker2dseqImageIO.h"
#include "itkVersion.h"

/*
* \author Don C. Bigler
*         The Pennsylvania State University 2005
*
* This implementation was contributed as a paper to the Insight Journal
* http://insight-journal.org/midas/handle.php?handle=1926/1381
*
*/

namespace itk
{
Bruker2dseqImageIOFactory::Bruker2dseqImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkBruker2dseqImageIO",
                          "Bruker2dseq Image IO",
                          1,
                          CreateObjectFunction< Bruker2dseqImageIO >::New() );
}

Bruker2dseqImageIOFactory::~Bruker2dseqImageIOFactory()
{}

const char *
Bruker2dseqImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
Bruker2dseqImageIOFactory::GetDescription(void) const
{
  return "Bruker2dseq ImageIO Factory, allows the loading of Bruker2dseq"
         " images into Insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool Bruker2dseqImageIOFactoryHasBeenRegistered;

void ITKIOBruker_EXPORT Bruker2dseqImageIOFactoryRegister__Private(void)
{
  if( ! Bruker2dseqImageIOFactoryHasBeenRegistered )
    {
    Bruker2dseqImageIOFactoryHasBeenRegistered = true;
    Bruker2dseqImageIOFactory::RegisterOneFactory();
    }
}
} // end namespace itk

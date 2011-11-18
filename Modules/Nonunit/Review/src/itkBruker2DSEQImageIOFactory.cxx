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
#include "itkBruker2DSEQImageIOFactory.h"
#include "itkBruker2DSEQImageIO.h"
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
Bruker2DSEQImageIOFactory::Bruker2DSEQImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkBruker2DSEQImageIO",
                          "Bruker 2DSEQ Image IO",
                          1,
                          CreateObjectFunction< Bruker2DSEQImageIO >::New() );
}

Bruker2DSEQImageIOFactory::~Bruker2DSEQImageIOFactory()
{}

const char *
Bruker2DSEQImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
Bruker2DSEQImageIOFactory::GetDescription(void) const
{
  return "Bruker 2DSEQ ImageIO Factory, allows the loading of most Bruker 2DSEQ"
         " images into Insight";
}
} // end namespace itk

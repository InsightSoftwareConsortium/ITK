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
#include "itkVoxBoCUBImageIOFactory.h"
#include "itkVoxBoCUBImageIO.h"
#include "itkVersion.h"

namespace itk
{
/**
 *
 * \author Burstein, Pablo D.; Yushkevich, Paul; Gee, James C.
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/303
 *
 */

VoxBoCUBImageIOFactory::VoxBoCUBImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkVoxBoCUBImageIO",
                          "VoxBo CUB Image IO",
                          1,
                          CreateObjectFunction< VoxBoCUBImageIO >::New() );
}

VoxBoCUBImageIOFactory::~VoxBoCUBImageIOFactory()
{}

const char *
VoxBoCUBImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
VoxBoCUBImageIOFactory::GetDescription() const
{
  return "VoxBo CUB ImageIO Factory, allows the loading of VoxBoCUB images into Insight";
}
} // end namespace itk

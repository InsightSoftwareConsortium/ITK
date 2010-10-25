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
#ifndef __itkBloxBoundaryPointImage_txx
#define __itkBloxBoundaryPointImage_txx

#include "itkImageRegionIterator.h"
#include "itkBloxBoundaryPointImage.h"

namespace itk
{
template< unsigned int VImageDimension >
BloxBoundaryPointImage< VImageDimension >
::BloxBoundaryPointImage()
{
  m_NumBoundaryPoints = 0;
}

template< unsigned int VImageDimension >
BloxBoundaryPointImage< VImageDimension >
::~BloxBoundaryPointImage()
{}

template< unsigned int VImageDimension >
void
BloxBoundaryPointImage< VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Total number of boundary points: "
     << m_NumBoundaryPoints << std::endl;
}
} // end namespace itk

#endif

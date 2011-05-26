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
#ifndef __itkBloxCoreAtomItem_txx
#define __itkBloxCoreAtomItem_txx

#include "itkBloxCoreAtomItem.h"

namespace itk
{
template< unsigned int VImageDimension >
BloxCoreAtomItem< VImageDimension >
::BloxCoreAtomItem()
{}

template< unsigned int VImageDimension >
BloxCoreAtomItem< VImageDimension >
::~BloxCoreAtomItem()
{}

template< unsigned int VImageDimension >
void
BloxCoreAtomItem< VImageDimension >
::CalcCenterAndDiameter()
{
  // Get boundary points
  PositionType P1 = m_BoundaryPointA->GetPhysicalPosition();
  PositionType P2 = m_BoundaryPointB->GetPhysicalPosition();

  // Calculate the center of the core atom
  m_CenterPosition = P1 + ( P2 - P1 ) / 2;

  // Calculate the diameter of the core atom
  m_Diameter = ( P1 - P2 ).GetNorm();
}
} // end namespace itk

#endif

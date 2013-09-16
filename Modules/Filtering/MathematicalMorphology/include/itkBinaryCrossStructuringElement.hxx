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
#ifndef __itkBinaryCrossStructuringElement_hxx
#define __itkBinaryCrossStructuringElement_hxx
#include "itkBinaryCrossStructuringElement.h"

#include "itkNumericTraits.h"

namespace itk
{
// Create the structuring element
template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
BinaryCrossStructuringElement< TPixel, VDimension, TAllocator >
::CreateStructuringElement()
{
  // Structuring element is defined to be 3x3x3...
  RadiusType radius;

  radius.Fill(1);
  this->SetRadius(radius);

  //
  // Zero out the neighborhood
  //
  Iterator kernel_it;
  for ( kernel_it = this->Begin(); kernel_it != this->End(); ++kernel_it )
    {
    *kernel_it = NumericTraits< TPixel >::Zero;
    }

  //
  // Set the face connected neighbors
  //
  unsigned int    d;
  OffsetValueType i;
  OffsetType      offset;
  offset.Fill(0);
  ( *this )[offset] = NumericTraits< TPixel >::One;
  for ( d = 0; d < VDimension; ++d )
    {
    for ( i = -1; i <= 1; i += 2 )
      {
      offset[d] = i;
      // a neighbor pixel in dimension d
      ( *this )[offset] = NumericTraits< TPixel >::One;
      }
    offset[d] = 0;
    }
}
} // namespace itk

#endif

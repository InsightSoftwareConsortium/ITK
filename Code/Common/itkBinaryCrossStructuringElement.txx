/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryCrossStructuringElement.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryCrossStructuringElement_txx
#define __itkBinaryCrossStructuringElement_txx
#include "itkBinaryCrossStructuringElement.h"

#include "itkNumericTraits.h"

namespace itk
{

// Create the structuring element
template <class TPixel, unsigned int VDimension, class TAllocator>
void
BinaryCrossStructuringElement<TPixel, VDimension, TAllocator>
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
  for (kernel_it=this->Begin(); kernel_it != this->End(); ++kernel_it)
    {
    *kernel_it = NumericTraits<TPixel>::Zero;
    }

  //
  // Set the face connected neighbors
  //
  unsigned int d;
  OffsetValueType i;
  OffsetType offset;
  offset.Fill(0);
  (*this)[offset] = NumericTraits<TPixel>::One;
  for (d=0; d < VDimension; ++d)
    {
    for (i=-1; i<=1; i+=2)
      {
      offset[d] = i;
      // a neighbor pixel in dimension d
      (*this)[offset] = NumericTraits<TPixel>::One; 
      }
    offset[d] = 0;
    }
}

} // namespace itk

#endif

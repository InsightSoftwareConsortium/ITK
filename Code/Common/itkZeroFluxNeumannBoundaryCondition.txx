/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroFluxNeumannBoundaryCondition.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkZeroFluxNeumannBoundaryCondition_txx
#define __itkZeroFluxNeumannBoundaryCondition_txx
#include "itkZeroFluxNeumannBoundaryCondition.h"
namespace itk
{
template<class TImage>
typename ZeroFluxNeumannBoundaryCondition<TImage>::PixelType
ZeroFluxNeumannBoundaryCondition<TImage>
::operator()(const OffsetType& point_index, const OffsetType& boundary_offset,
             const NeighborhoodType *data) const
{
  int linear_index = 0;

  // Return the value of the pixel at the closest boundary point.
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      linear_index += (point_index[i] + boundary_offset[i]) * data->GetStride(i);
    }

  return *(data->operator[](linear_index));
}

} // end namespace itk

#endif

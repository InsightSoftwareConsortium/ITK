/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroFluxNeumannBoundaryCondition.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef __itkZeroFluxNeumannBoundaryCondition_txx
#define __itkZeroFluxNeumannBoundaryCondition_txx
namespace itk
{
template<class TImage, class TNeighborhoodType>
ZeroFluxNeumannBoundaryCondition<TImage, TNeighborhoodType>::PixelType
ZeroFluxNeumannBoundaryCondition<TImage, TNeighborhoodType>
::operator()(const int *point_index, const int *boundary_offset,
             const TNeighborhoodType *data) const
{
  int linear_index = 0;
  // Return the value of the pixel at the closest boundary point.
  int index[ImageDimension];

  for (int i = 0; i < ImageDimension; ++i)
    {
      index[i] = point_index[i] + boundary_offset[i];
      linear_index += index[i] * data->GetStride(i);
    }

  return *(data->operator[](linear_index));
}

} // end namespace itk

#endif

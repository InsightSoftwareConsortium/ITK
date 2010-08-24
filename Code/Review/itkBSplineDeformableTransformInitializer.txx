/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineDeformableTransformInitializer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBSplineDeformableTransformInitializer_txx
#define __itkBSplineDeformableTransformInitializer_txx

#include "itkBSplineDeformableTransformInitializer.h"

namespace itk
{
template< class TTransform, class TImage >
BSplineDeformableTransformInitializer< TTransform, TImage >
::BSplineDeformableTransformInitializer()
{
  this->m_GridSizeInsideTheImage.Fill(5);
}

template< class TTransform, class TImage >
void
BSplineDeformableTransformInitializer< TTransform, TImage >
::InitializeTransform() const
{
  // Sanity check
  if ( !this->m_Image )
    {
    itkExceptionMacro("Reference Image has not been set");
    return;
    }

  if ( !this->m_Transform )
    {
    itkExceptionMacro("Transform has not been set");
    return;
    }

  // If the image come from a filter, then update that filter.
  if ( this->m_Image->GetSource() )
    {
    this->m_Image->GetSource()->Update();
    }

  typedef typename TransformType::RegionType RegionType;

  typename RegionType::SizeType numberOfGridNodesOutsideTheImageSupport;
  typename RegionType::SizeType totalGridSize;

  numberOfGridNodesOutsideTheImageSupport.Fill(TransformType::SplineOrder);

  totalGridSize = this->m_GridSizeInsideTheImage;
  totalGridSize += numberOfGridNodesOutsideTheImageSupport;

  RegionType gridRegion;
  gridRegion.SetSize(totalGridSize);

  typedef typename TransformType::SpacingType SpacingType;
  const SpacingType & imageSpacing = this->m_Image->GetSpacing();

  typedef typename TransformType::OriginType OriginType;
  const OriginType & imageOrigin = this->m_Image->GetOrigin();

  const typename TransformType::RegionType & imageRegion =
    this->m_Image->GetLargestPossibleRegion();

  typename ImageType::SizeType fixedImageSize = imageRegion.GetSize();

  SpacingType gridSpacing;
  SpacingType gridOriginShift;

  const unsigned int orderShift = TransformType::SplineOrder / 2;

  for ( unsigned int r = 0; r < SpaceDimension; r++ )
    {
    const unsigned int numberOfGridCells = this->m_GridSizeInsideTheImage[r] - 1;
    const unsigned int numberOfImagePixels = fixedImageSize[r];

    gridSpacing[r] = imageSpacing[r]
                     * static_cast< double >( numberOfImagePixels )
                     / static_cast< double >( numberOfGridCells );

    // Shift half image pixel to cover the image support
    const double imageSupportShift = -imageSpacing[r] / 2.0;

    // Shift by the number of extra grid cells required by
    // the BSpline order.
    const double gridSupportShift =  -1.0 * gridSpacing[r] * orderShift;

    // Combine both shifts. They are both aligned with the coordinate
    // system of the grid. Direction has not been considered so far.
    gridOriginShift[r] = gridSupportShift + imageSupportShift;
    }

  typename ImageType::DirectionType gridDirection = this->m_Image->GetDirection();
  SpacingType gridOriginOffset = gridDirection * gridOriginShift;

  OriginType gridOrigin = imageOrigin + gridOriginOffset;

  this->m_Transform->SetGridRegion(gridRegion);
  this->m_Transform->SetGridOrigin(gridOrigin);
  this->m_Transform->SetGridSpacing(gridSpacing);
  this->m_Transform->SetGridDirection(gridDirection);
}

template< class TTransform, class TImage >
void
BSplineDeformableTransformInitializer< TTransform, TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Transform   = " << std::endl;
  if ( this->m_Transform )
    {
    os << indent << this->m_Transform  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "Image   = " << std::endl;
  if ( this->m_Image )
    {
    os << indent << this->m_Image  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }
  os << "Grid size inside the image " << this->m_GridSizeInsideTheImage << std::endl;
  os << "Number of grid nodes inside the image " << this->m_NumberOfGridNodesInsideTheImage << std::endl;
}
}  // namespace itk

#endif

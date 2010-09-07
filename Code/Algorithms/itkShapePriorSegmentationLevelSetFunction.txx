/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapePriorSegmentationLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapePriorSegmentationLevelSetFunction_txx
#define __itkShapePriorSegmentationLevelSetFunction_txx

#include "itkShapePriorSegmentationLevelSetFunction.h"

namespace itk
{
/**
 * Constructor
 */
template< class TImageType, class TFeatureImageType >
ShapePriorSegmentationLevelSetFunction< TImageType, TFeatureImageType >
::ShapePriorSegmentationLevelSetFunction()
{
  m_ShapeFunction = NULL;
  m_ShapePriorWeight = NumericTraits< ScalarValueType >::Zero;
}

/**
 * PrintSelf
 */
template< class TImageType, class TFeatureImageType >
void
ShapePriorSegmentationLevelSetFunction< TImageType, TFeatureImageType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ShapeFunction: " << m_ShapeFunction.GetPointer() << std::endl;
  os << indent << "ShapePriorWeight: " << m_ShapePriorWeight << std::endl;
}

/**
 * Compute the equation value.
 */
template< class TImageType, class TFeatureImageType >
typename ShapePriorSegmentationLevelSetFunction< TImageType, TFeatureImageType >
::PixelType
ShapePriorSegmentationLevelSetFunction< TImageType, TFeatureImageType >
::ComputeUpdate(
  const NeighborhoodType & neighborhood,
  void *gd,
  const FloatOffsetType & offset)
{
  // Compute the generic level set update using superclass
  PixelType value = this->Superclass::ComputeUpdate(neighborhood, gd, offset);

  // Add the shape prior term
  if ( m_ShapeFunction && m_ShapePriorWeight != NumericTraits< ScalarValueType >::Zero )
    {
    IndexType                                 idx = neighborhood.GetIndex();
    ContinuousIndex< double, ImageDimension > cdx;
    for ( unsigned int i = 0; i < ImageDimension; ++i )
      {
      cdx[i] = static_cast< double >( idx[i] ) - offset[i];
      }
    typename ShapeFunctionType::PointType point;
    this->GetFeatureImage()->TransformContinuousIndexToPhysicalPoint(cdx, point);

    ScalarValueType shape_term = m_ShapePriorWeight
                                 * ( m_ShapeFunction->Evaluate(point) - neighborhood.GetCenterPixel() );

    value += shape_term;

    // collect max change to be used for calculating the time step
    ShapePriorGlobalDataStruct *globalData = (ShapePriorGlobalDataStruct *)gd;
    globalData->m_MaxShapePriorChange =
      vnl_math_max( globalData->m_MaxShapePriorChange, vnl_math_abs(shape_term) );
    }

  return value;
}

/**
 * Compute the global time step.
 */
template< class TImageType, class TFeatureImageType >
typename ShapePriorSegmentationLevelSetFunction< TImageType, TFeatureImageType >
::TimeStepType
ShapePriorSegmentationLevelSetFunction< TImageType, TFeatureImageType >
::ComputeGlobalTimeStep(void *gd) const
{
  TimeStepType dt;

  ShapePriorGlobalDataStruct *d = (ShapePriorGlobalDataStruct *)gd;

  d->m_MaxAdvectionChange += d->m_MaxPropagationChange + d->m_MaxShapePriorChange;

  if ( vnl_math_abs(d->m_MaxCurvatureChange) > 0.0 )
    {
    if ( d->m_MaxAdvectionChange > 0.0 )
      {
      dt = vnl_math_min( ( this->m_WaveDT / d->m_MaxAdvectionChange ),
                         (    this->m_DT / d->m_MaxCurvatureChange ) );
      }
    else
      {
      dt = this->m_DT / d->m_MaxCurvatureChange;
      }
    }
  else
    {
    if ( d->m_MaxAdvectionChange > 0.0 )
      {
      dt = this->m_WaveDT / d->m_MaxAdvectionChange;
      }
    else
      {
      dt = 0.0;
      }
    }

  double maxScaleCoefficient = 0.0;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    maxScaleCoefficient = vnl_math_max(this->m_ScaleCoefficients[i], maxScaleCoefficient);
    }
  dt /= maxScaleCoefficient;

  // reset the values
  d->m_MaxAdvectionChange  = 0;
  d->m_MaxPropagationChange = 0;
  d->m_MaxCurvatureChange = 0;
  d->m_MaxShapePriorChange = 0;

  return dt;
}
} // end namespace itk

#endif

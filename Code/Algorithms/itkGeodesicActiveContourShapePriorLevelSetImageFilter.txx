/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGeodesicActiveContourShapePriorLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGeodesicActiveContourShapePriorLevelSetImageFilter_txx_
#define __itkGeodesicActiveContourShapePriorLevelSetImageFilter_txx_

#include "itkGeodesicActiveContourShapePriorLevelSetImageFilter.h"

namespace itk {


template <class TInputImage, class TFeatureImage, class TOutputType>
GeodesicActiveContourShapePriorLevelSetImageFilter<TInputImage, TFeatureImage, TOutputType>
::GeodesicActiveContourShapePriorLevelSetImageFilter()
{
  /* Instantiate a geodesic active contour function and set it as the segmentation function. */
  m_GeodesicActiveContourFunction = GeodesicActiveContourFunctionType::New();
  this->SetShapePriorSegmentationFunction( m_GeodesicActiveContourFunction );

  /* Turn off interpolation. */
  this->InterpolateSurfaceLocationOff();
}
 
template <class TInputImage, class TFeatureImage, class TOutputType>
void
GeodesicActiveContourShapePriorLevelSetImageFilter<TInputImage, TFeatureImage, TOutputType>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "GeodesicActiveContourFunction: " << m_GeodesicActiveContourFunction.GetPointer();
}


template <class TInputImage, class TFeatureImage, class TOutputType>
void
GeodesicActiveContourShapePriorLevelSetImageFilter<TInputImage, TFeatureImage, TOutputType>
::GenerateData()
{

  // Make sure the SpeedImage is setup for the case when PropagationScaling
  // is zero
  if ( this->GetSegmentationFunction() && 
       this->GetSegmentationFunction()->GetPropagationWeight() == 0 )
    {
    this->GetSegmentationFunction()->AllocateSpeedImage();
    this->GetSegmentationFunction()->CalculateSpeedImage();
    }

  // Continue with Superclass implementation
  Superclass::GenerateData();

}

}// end namespace itk




#endif

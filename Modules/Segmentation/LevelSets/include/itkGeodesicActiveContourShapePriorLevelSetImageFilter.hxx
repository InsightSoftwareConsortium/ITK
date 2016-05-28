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
#ifndef itkGeodesicActiveContourShapePriorLevelSetImageFilter_hxx
#define itkGeodesicActiveContourShapePriorLevelSetImageFilter_hxx

#include "itkGeodesicActiveContourShapePriorLevelSetImageFilter.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TFeatureImage, typename TOutputType >
GeodesicActiveContourShapePriorLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::GeodesicActiveContourShapePriorLevelSetImageFilter()
{
  // Instantiate a geodesic active contour function and set it as the
  // segmentation function.
  m_GeodesicActiveContourFunction = GeodesicActiveContourFunctionType::New();
  this->SetShapePriorSegmentationFunction(m_GeodesicActiveContourFunction);

  // Turn off interpolation
  this->InterpolateSurfaceLocationOff();
}

template< typename TInputImage, typename TFeatureImage, typename TOutputType >
void
GeodesicActiveContourShapePriorLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::GenerateData()
{
  // Make sure the SpeedImage is setup for the case when PropagationScaling
  // is zero
  if ( this->GetSegmentationFunction()
       && Math::ExactlyEquals(this->GetSegmentationFunction()->GetPropagationWeight(), 0) )
    {
    this->GetSegmentationFunction()->AllocateSpeedImage();
    this->GetSegmentationFunction()->CalculateSpeedImage();
    }

  // Continue with Superclass implementation
  Superclass::GenerateData();
}

template< typename TInputImage, typename TFeatureImage, typename TOutputType >
void
GeodesicActiveContourShapePriorLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << "GeodesicActiveContourFunction: " << m_GeodesicActiveContourFunction.GetPointer()
    << std::endl;
}

} // end namespace itk

#endif

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
#ifndef itkNarrowBandCurvesLevelSetImageFilter_hxx
#define itkNarrowBandCurvesLevelSetImageFilter_hxx

#include "itkNarrowBandCurvesLevelSetImageFilter.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TFeatureImage, typename TOutputType >
NarrowBandCurvesLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::NarrowBandCurvesLevelSetImageFilter()
{
  /* Instantiate a geodesic active contour function and set it as the
    segmentation function. */
  m_CurvesFunction = CurvesFunctionType::New();

  this->SetSegmentationFunction(m_CurvesFunction);

  /* Use negative features by default. */
  this->ReverseExpansionDirectionOff();
}

template< typename TInputImage, typename TFeatureImage, typename TOutputType >
void
NarrowBandCurvesLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "CurvesFunction: " << m_CurvesFunction.GetPointer();
}

template< typename TInputImage, typename TFeatureImage, typename TOutputType >
void
NarrowBandCurvesLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::GenerateData()
{
  // Make sure the SpeedImage is setup for the case when PropagationScaling
  // is zero. This image is used by the curvature term.
  if ( this->GetSegmentationFunction()
       && Math::ExactlyEquals(this->GetSegmentationFunction()->GetPropagationWeight(), 0) )
    {
    this->GetSegmentationFunction()->AllocateSpeedImage();
    this->GetSegmentationFunction()->CalculateSpeedImage();
    }

  // Continue with Superclass implementation
  Superclass::GenerateData();
}
} // end namespace itk

#endif

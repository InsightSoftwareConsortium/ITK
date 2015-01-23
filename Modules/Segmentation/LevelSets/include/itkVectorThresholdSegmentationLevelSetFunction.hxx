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
#ifndef itkVectorThresholdSegmentationLevelSetFunction_hxx
#define itkVectorThresholdSegmentationLevelSetFunction_hxx

#include "itkVectorThresholdSegmentationLevelSetFunction.h"
#include "itkImageRegionIterator.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkLaplacianImageFilter.h"
#include "itkImageFileWriter.h"

namespace itk
{
template< typename TImageType, typename TFeatureImageType >
void VectorThresholdSegmentationLevelSetFunction< TImageType, TFeatureImageType >
::CalculateSpeedImage()
{
  ImageRegionConstIterator< FeatureImageType >
  fit( this->GetFeatureImage(), this->GetFeatureImage()->GetRequestedRegion() );
  ImageRegionIterator< ImageType >
  sit( this->GetSpeedImage(), this->GetFeatureImage()->GetRequestedRegion() );

  ScalarValueType threshold;
  for ( fit.GoToBegin(), sit.GoToBegin(); !fit.IsAtEnd(); ++sit, ++fit )
    {
    threshold = m_Threshold - std::sqrt( m_Mahalanobis->Evaluate( fit.Get() ) );
    sit.Set( static_cast< ScalarValueType >( threshold ) );
    }
}
} // end namespace itk

#endif

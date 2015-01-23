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
#ifndef itkThresholdSegmentationLevelSetFunction_hxx
#define itkThresholdSegmentationLevelSetFunction_hxx

#include "itkThresholdSegmentationLevelSetFunction.h"
#include "itkImageRegionIterator.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkLaplacianImageFilter.h"
#include "itkImageFileWriter.h"

namespace itk
{
template< typename TImageType, typename TFeatureImageType >
void ThresholdSegmentationLevelSetFunction< TImageType, TFeatureImageType >
::CalculateSpeedImage()
{
  typename GradientAnisotropicDiffusionImageFilter< TFeatureImageType, TFeatureImageType >::Pointer
  diffusion  = GradientAnisotropicDiffusionImageFilter< TFeatureImageType, TFeatureImageType >::New();
  typename LaplacianImageFilter< TFeatureImageType, TFeatureImageType >::Pointer
  laplacian = LaplacianImageFilter< TFeatureImageType, TFeatureImageType >::New();

  ImageRegionIterator< FeatureImageType > lit;
  ImageRegionConstIterator< FeatureImageType >
  fit( this->GetFeatureImage(), this->GetFeatureImage()->GetRequestedRegion() );
  ImageRegionIterator< ImageType >
  sit( this->GetSpeedImage(), this->GetFeatureImage()->GetRequestedRegion() );

  if ( m_EdgeWeight != 0.0 )
    {
    diffusion->SetInput( this->GetFeatureImage() );
    diffusion->SetConductanceParameter(m_SmoothingConductance);
    diffusion->SetTimeStep(m_SmoothingTimeStep);
    diffusion->SetNumberOfIterations(m_SmoothingIterations);

    laplacian->SetInput( diffusion->GetOutput() );
    laplacian->Update();

    lit = ImageRegionIterator< FeatureImageType >( laplacian->GetOutput(),
                                                   this->GetFeatureImage()->GetRequestedRegion() );
    lit.GoToBegin();
    }

  // Copy the meta information (spacing and origin) from the feature image
  this->GetSpeedImage()->CopyInformation( this->GetFeatureImage() );

  // Calculate the speed image
  ScalarValueType upper_threshold = static_cast< ScalarValueType >( m_UpperThreshold );
  ScalarValueType lower_threshold = static_cast< ScalarValueType >( m_LowerThreshold );
  ScalarValueType mid = ( ( upper_threshold - lower_threshold ) / 2.0 ) + lower_threshold;
  ScalarValueType threshold;
  for ( fit.GoToBegin(), sit.GoToBegin(); !fit.IsAtEnd(); ++sit, ++fit )
    {
    if ( static_cast< ScalarValueType >( fit.Get() ) < mid )
      {
      threshold = fit.Get() - lower_threshold;
      }
    else
      {
      threshold = upper_threshold - fit.Get();
      }

    if ( m_EdgeWeight != 0.0 )
      {
      sit.Set( static_cast< ScalarValueType >( threshold + m_EdgeWeight * lit.Get() ) );
      ++lit;
      }
    else
      {
      sit.Set( static_cast< ScalarValueType >( threshold ) );
      }
    }
}
} // end namespace itk

#endif

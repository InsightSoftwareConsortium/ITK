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
#ifndef itkCannySegmentationLevelSetFunction_hxx
#define itkCannySegmentationLevelSetFunction_hxx

#include "itkCannySegmentationLevelSetFunction.h"
#include "itkGradientImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageAlgorithm.h"

namespace itk
{
template< typename TImageType, typename TFeatureImageType >
void CannySegmentationLevelSetFunction< TImageType, TFeatureImageType >
::CalculateSpeedImage()
{
  // Create a distance transform to the canny edges
  this->CalculateDistanceImage();

  // Graft the distance transform into the Speed Image
  this->GetSpeedImage()->Graft( m_Distance->GetOutput() );
}

template< typename TImageType, typename TFeatureImageType >
void CannySegmentationLevelSetFunction< TImageType, TFeatureImageType >
::CalculateAdvectionImage()
{
  typename GradientImageFilter< ImageType, ScalarValueType, ScalarValueType >::Pointer
  gradient = GradientImageFilter< ImageType, ScalarValueType, ScalarValueType >::New();

  typedef typename GradientImageFilter< ImageType, ScalarValueType,
                                        ScalarValueType >::OutputImageType CovariantVectorImageType;

  typename MultiplyImageFilter< CovariantVectorImageType, ImageType,
                                CovariantVectorImageType >::Pointer multiply =
    MultiplyImageFilter< CovariantVectorImageType, ImageType, CovariantVectorImageType >::New();

  // Create a distance transform to the canny edges
  this->CalculateDistanceImage();

  gradient->SetInput( m_Distance->GetOutput() );
  gradient->Update();

  multiply->SetInput1( gradient->GetOutput() );
  multiply->SetInput2( m_Distance->GetOutput() );

  //  multiply->GraftOutput(dynamic_cast<CovariantVectorImageType
  // *>(this->GetAdvectionImage()));
  multiply->Update();

// Copy output to Advection Image
  ImageAlgorithm::Copy( multiply->GetOutput(),
                        this->GetAdvectionImage(),
                        this->GetAdvectionImage()->GetRequestedRegion(),
                        this->GetAdvectionImage()->GetRequestedRegion() );

}

template< typename TImageType, typename TFeatureImageType >
void CannySegmentationLevelSetFunction< TImageType, TFeatureImageType >
::CalculateDistanceImage()
{
  typename TFeatureImageType::Pointer tempFeature = TFeatureImageType::New();

  // The minipipeline might muck with its feature image requested
  // region. The rest of the class relies on the feature image requested
  // region as specified by the original level set
  // filter. We make a temporary shallow copy of feature image to
  // build the distance image.
  tempFeature->Graft( this->GetFeatureImage() );

  // AssignCannyInput either sets up a pipeline through the
  // CastImageFilter if TImageType != TFeatureImageType
  // or bypasses the Cast operation if TImageType == TFeatureType
  typename TImageType::Pointer junk;
  this->AssignCannyInput(tempFeature,junk);

  m_Canny->SetUpperThreshold(m_Threshold);
  m_Canny->SetVariance(m_Variance);
  m_Canny->SetMaximumError(0.01);

  m_Distance->SetInput( m_Canny->GetOutput() );
  m_Distance->GetOutput()->SetRequestedRegion( this->GetSpeedImage()->GetRequestedRegion() );
  m_Distance->Update();
}
} // end namespace itk

#endif

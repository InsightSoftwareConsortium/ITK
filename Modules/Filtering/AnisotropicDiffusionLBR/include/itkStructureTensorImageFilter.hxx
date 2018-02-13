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
//
//  Created by Jean-Marie Mirebeau on 21/11/2014.
//
//

#ifndef itkStructureTensorImageFilter_hxx
#define itkStructureTensorImageFilter_hxx

#include "itkStructureTensorImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"

namespace itk
{

template< typename TImage, typename TTensorImage >
StructureTensorImageFilter< TImage, TTensorImage >
::StructureTensorImageFilter():
  m_FeatureScale( 2 ),
  m_NoiseScale( 1 ),
  m_RescaleForUnitMaximumTrace( false ),
  m_UseGradientRecursiveGaussianImageFilter( true )
{
}


template< typename TImage, typename TTensorImage >
void
StructureTensorImageFilter< TImage, TTensorImage >
::IntermediateFilter( const Dispatch< true > & )
{
  using GradientFilterType = GradientRecursiveGaussianImageFilter< TImage, Self::CovariantImageType >;
  typename GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
  gradientFilter->SetInput(this->GetInput());
  gradientFilter->SetSigma(this->m_NoiseScale);

  using OuterFilterType = UnaryFunctorImageFilter< Self::CovariantImageType, Self::TensorImageType, Self::OuterFunctor >;
  typename OuterFilterType::Pointer outerFilter = OuterFilterType::New();
  outerFilter->SetInput(gradientFilter->GetOutput());

  outerFilter->Update();
  this->m_IntermediateResult = outerFilter->GetOutput();
}


template< typename TImage, typename TTensorImage >
void
StructureTensorImageFilter< TImage, TTensorImage >
::IntermediateFilter( const Dispatch< false > & )
{
  typename Self::ImageType::ConstPointer input = this->GetInput();
  typename Self::TensorImageType::Pointer output = Self::TensorImageType::New();
  output->CopyInformation(input);
  output->SetRegions(input->GetRequestedRegion());
  output->Allocate();
  output->FillBuffer(Self::TensorType(0.));

  for( unsigned int index = 0; index < Self::PixelType::Dimension; ++index )
    {
    using SelectionFilterType = VectorIndexSelectionCastImageFilter< Self::ImageType, Self::ScalarImageType >;
    typename SelectionFilterType::Pointer selectionFilter = SelectionFilterType::New();
    selectionFilter->SetIndex(index);
    selectionFilter->SetInput(input);

    using GaussianFilterType = RecursiveGaussianImageFilter< Self::ScalarImageType >;
    using GradientFilterType = GradientImageFilter< Self::ScalarImageType, Self::ScalarType, Self::ScalarType, Self::CovariantImageType >;
    using GradientGaussianFilterType = GradientRecursiveGaussianImageFilter< Self::ScalarImageType, Self::CovariantImageType >;

    typename GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();
    typename GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
    typename GradientGaussianFilterType::Pointer gradientGaussianFilter = GradientGaussianFilterType::New();

    gaussianFilter->SetSigma(this->m_NoiseScale);
    gradientGaussianFilter->SetSigma(this->m_NoiseScale);

    using OuterFilterType = UnaryFunctorImageFilter< Self::CovariantImageType, Self::TensorImageType, Self::OuterFunctor >;
    typename OuterFilterType::Pointer outerFilter = OuterFilterType::New();

    if( this->m_UseGradientRecursiveGaussianImageFilter )
      {
      gradientGaussianFilter->SetInput(selectionFilter->GetOutput());
      outerFilter->SetInput(gradientGaussianFilter->GetOutput());
      }
    else
      {
      gaussianFilter->SetInput(selectionFilter->GetOutput());
      gradientFilter->SetInput(gaussianFilter->GetOutput());
      outerFilter->SetInput(gradientFilter->GetOutput());
      }

    using AddFilterType = AddImageFilter< Self::TensorImageType >;
    typename AddFilterType::Pointer addFilter = AddFilterType::New();
    addFilter->InPlaceOn();
    addFilter->SetInput1(output);
    addFilter->SetInput2(outerFilter->GetOutput());
    addFilter->Update();
    output = addFilter->GetOutput();

    this->UpdateProgress(index/float( Self::PixelType::Dimension+1 ));
    }
  this->m_IntermediateResult = output;
}


template< typename TImage, typename TTensorImage >
void
StructureTensorImageFilter< TImage, TTensorImage >
::GenerateData()
{
  this->IntermediateFilter( Dispatch< std::numeric_limits< PixelType >::is_specialized >() );

  using GaussianFilterType = RecursiveGaussianImageFilter<TensorImageType>;
  typename GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();
  gaussianFilter->SetInput( m_IntermediateResult );
  gaussianFilter->SetSigma( m_FeatureScale );

  if( !m_RescaleForUnitMaximumTrace )
    {
    m_PostRescaling = 1.;
    gaussianFilter->Update();
    this->GraftOutput(gaussianFilter->GetOutput());
    return;
    }

  // *** Rescaling for normalization of largest trace ***

  using TraceFilterType = UnaryFunctorImageFilter<TensorImageType, ScalarImageType, TraceFunctor>;
  typename TraceFilterType::Pointer traceFilter = TraceFilterType::New();
  traceFilter->SetInput(gaussianFilter->GetOutput());

  using MaximumCalculatorType = MinimumMaximumImageCalculator<ScalarImageType>;
  typename MaximumCalculatorType::Pointer maximumCalculator = MaximumCalculatorType::New();
  maximumCalculator->SetImage(traceFilter->GetOutput());

  using ScaleFilterType = UnaryFunctorImageFilter<TensorImageType, TensorImageType, ScaleFunctor>;
  typename ScaleFilterType::Pointer scaleFilter = ScaleFilterType::New();
  scaleFilter->SetInput(gaussianFilter->GetOutput());

  traceFilter->Update();
  maximumCalculator->ComputeMaximum();
  m_PostRescaling = 1./maximumCalculator->GetMaximum();
  scaleFilter->GetFunctor().scaling = m_PostRescaling;
  scaleFilter->Update();
  this->GraftOutput(scaleFilter->GetOutput());
}

} // end namespace itk

#endif

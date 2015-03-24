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
#ifndef itkZeroCrossingBasedEdgeDetectionImageFilter_hxx
#define itkZeroCrossingBasedEdgeDetectionImageFilter_hxx

#include "itkZeroCrossingBasedEdgeDetectionImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkLaplacianImageFilter.h"
#include "itkZeroCrossingImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
ZeroCrossingBasedEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  typename  InputImageType::ConstPointer input  = this->GetInput();

  // Create the filters that are needed
  typename DiscreteGaussianImageFilter< TInputImage, TOutputImage >::Pointer
  gaussianFilter =
    DiscreteGaussianImageFilter< TInputImage, TOutputImage >::New();
  typename LaplacianImageFilter< TInputImage, TOutputImage >::Pointer laplacianFilter =
    LaplacianImageFilter< TInputImage, TOutputImage >::New();
  typename ZeroCrossingImageFilter< TInputImage, TOutputImage >::Pointer
  zerocrossingFilter =
    ZeroCrossingImageFilter< TInputImage, TOutputImage >::New();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  //Construct the mini-pipeline

  // Apply the Gaussian filter
  gaussianFilter->SetVariance(m_Variance);
  gaussianFilter->SetMaximumError(m_MaximumError);
  gaussianFilter->SetInput(input);
  progress->RegisterInternalFilter(gaussianFilter, 1.0f / 3.0f);

  // Calculate the laplacian of the smoothed image
  laplacianFilter->SetInput( gaussianFilter->GetOutput() );
  progress->RegisterInternalFilter(laplacianFilter, 1.0f / 3.0f);

  // Find the zero-crossings of the laplacian
  zerocrossingFilter->SetInput( laplacianFilter->GetOutput() );
  zerocrossingFilter->SetBackgroundValue(m_BackgroundValue);
  zerocrossingFilter->SetForegroundValue(m_ForegroundValue);
  zerocrossingFilter->GraftOutput( this->GetOutput() );
  progress->RegisterInternalFilter(zerocrossingFilter, 1.0f / 3.0f);
  zerocrossingFilter->Update();

  // Graft the output of the mini-pipeline back onto the filter's output.
  // This action copies back the region ivars and meta-data
  this->GraftOutput( zerocrossingFilter->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
ZeroCrossingBasedEdgeDetectionImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Variance: " << m_Variance << std::endl;
  os << indent << "MaximumError: " << m_MaximumError << std::endl;
  os << indent << "ForegroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_ForegroundValue )
     << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_BackgroundValue )
     << std::endl;
}
} //end of itk namespace

#endif

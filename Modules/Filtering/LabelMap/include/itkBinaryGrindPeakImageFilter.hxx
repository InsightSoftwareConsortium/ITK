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
#ifndef itkBinaryGrindPeakImageFilter_hxx
#define itkBinaryGrindPeakImageFilter_hxx

#include "itkBinaryGrindPeakImageFilter.h"
#include "itkBinaryImageToShapeLabelMapFilter.h"
#include "itkShapeOpeningLabelMapFilter.h"
#include "itkLabelMapToBinaryImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template <typename TInputImage>
BinaryGrindPeakImageFilter<TInputImage>
::BinaryGrindPeakImageFilter() :
  m_ForegroundValue( NumericTraits<InputImagePixelType>::max() ),
  m_BackgroundValue( NumericTraits<InputImagePixelType>::ZeroValue() ),
  m_FullyConnected ( false )
{
}

template <typename TInputImage>
void
BinaryGrindPeakImageFilter<TInputImage>
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need the whole input
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

template <typename TInputImage>
void
BinaryGrindPeakImageFilter<TInputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template<typename TInputImage>
void
BinaryGrindPeakImageFilter<TInputImage>
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  typedef typename itk::BinaryImageToShapeLabelMapFilter< InputImageType > LabelizerType;
  typename LabelizerType::Pointer labelizer = LabelizerType::New();
  labelizer->SetInput( this->GetInput() );
  labelizer->SetInputForegroundValue( m_ForegroundValue );
  labelizer->SetOutputBackgroundValue( m_BackgroundValue );
  labelizer->SetFullyConnected( m_FullyConnected );
  labelizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(labelizer, .65f);

  typedef typename LabelizerType::OutputImageType                  LabelMapType;
  typedef typename itk::ShapeOpeningLabelMapFilter< LabelMapType > OpeningType;
  typename OpeningType::Pointer opening = OpeningType::New();
  opening->SetInput( labelizer->GetOutput() );
  opening->SetAttribute( LabelMapType::LabelObjectType::NUMBER_OF_PIXELS_ON_BORDER );
  opening->SetLambda( 1 );
  opening->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(opening, .1f);

  typedef typename itk::LabelMapToBinaryImageFilter< LabelMapType, OutputImageType > BinarizerType;
  typename BinarizerType::Pointer binarizer = BinarizerType::New();
  binarizer->SetInput( opening->GetOutput() );
  binarizer->SetForegroundValue( m_ForegroundValue );
  binarizer->SetBackgroundValue( m_BackgroundValue );
  binarizer->SetBackgroundImage( this->GetInput() );
  binarizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(binarizer, .25f);

  binarizer->GraftOutput( this->GetOutput() );
  binarizer->Update();
  this->GraftOutput( binarizer->GetOutput() );
}

template<typename TInputImage>
void
BinaryGrindPeakImageFilter<TInputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: " << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_ForegroundValue) << std::endl;
  os << indent << "BackgroundValue: " << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_BackgroundValue) << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}
}// end namespace itk
#endif

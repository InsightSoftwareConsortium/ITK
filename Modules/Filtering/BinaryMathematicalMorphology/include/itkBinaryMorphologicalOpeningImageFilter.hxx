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
#ifndef itkBinaryMorphologicalOpeningImageFilter_hxx
#define itkBinaryMorphologicalOpeningImageFilter_hxx

#include "itkBinaryMorphologicalOpeningImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkProgressAccumulator.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Binary morphological closing and opening image filters"
 * by Lehmann G.
 * https://hdl.handle.net/1926/141
 * http://www.insight-journal.org/browse/publication/58
 *
 */

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
BinaryMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
::BinaryMorphologicalOpeningImageFilter()
{
  m_ForegroundValue = NumericTraits< PixelType >::max();
  m_BackgroundValue = NumericTraits< PixelType >::ZeroValue();
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
BinaryMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateData()
{
  // Allocate the outputs
  this->AllocateOutputs();

  /** set up erosion and dilation methods */
  typename BinaryDilateImageFilter< TInputImage, TOutputImage, TKernel >::Pointer
  dilate = BinaryDilateImageFilter< TInputImage, TOutputImage, TKernel >::New();

  typename BinaryErodeImageFilter< TInputImage, TInputImage, TKernel >::Pointer
  erode = BinaryErodeImageFilter< TInputImage, TInputImage, TKernel >::New();

  dilate->SetKernel( this->GetKernel() );
  dilate->ReleaseDataFlagOn();
  erode->SetKernel( this->GetKernel() );
  erode->ReleaseDataFlagOn();
  dilate->SetDilateValue(m_ForegroundValue);
  erode->SetErodeValue(m_ForegroundValue);
  erode->SetBackgroundValue(m_BackgroundValue);

  /** set up the minipipeline */
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(erode, .5f);
  progress->RegisterInternalFilter(dilate, .5f);

  erode->SetInput( this->GetInput() );
  dilate->SetInput( erode->GetOutput() );
  dilate->GraftOutput( this->GetOutput() );

  /** execute the minipipeline */
  dilate->Update();

  /** graft the minipipeline output back into this filter's output */
  this->GraftOutput( dilate->GetOutput() );
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
BinaryMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_ForegroundValue ) << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_BackgroundValue ) << std::endl;
}
} // end namespace itk
#endif

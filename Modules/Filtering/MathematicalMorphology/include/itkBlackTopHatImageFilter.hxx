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
#ifndef itkBlackTopHatImageFilter_hxx
#define itkBlackTopHatImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkBlackTopHatImageFilter.h"
#include "itkGrayscaleMorphologicalClosingImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
BlackTopHatImageFilter< TInputImage, TOutputImage, TKernel >
::BlackTopHatImageFilter()
{
  m_SafeBorder = true;
  m_Algorithm = HISTO;
  m_ForceAlgorithm = false;
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
BlackTopHatImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to a closing filter.
  typedef GrayscaleMorphologicalClosingImageFilter< TInputImage, TInputImage, TKernel > ClosingFilterType;
  typename ClosingFilterType::Pointer close = ClosingFilterType::New();

  close->SetInput( this->GetInput() );
  close->SetKernel( this->GetKernel() );
  close->SetSafeBorder(m_SafeBorder);
  if ( m_ForceAlgorithm )
    {
    close->SetAlgorithm(m_Algorithm);
    }
  else
    {
    m_Algorithm = close->GetAlgorithm();
    }

  // Need to subtract the input from the closed image
  typename SubtractImageFilter< TInputImage, TInputImage, TOutputImage >::Pointer
  subtract = SubtractImageFilter< TInputImage, TInputImage, TOutputImage >::New();

  subtract->SetInput1( close->GetOutput() );
  subtract->SetInput2( this->GetInput() );

  // graft our output to the subtract filter to force the proper regions
  // to be generated
  subtract->GraftOutput( this->GetOutput() );

  // run the algorithm
  progress->RegisterInternalFilter(close, .9f);
  progress->RegisterInternalFilter(subtract, .1f);

  subtract->Update();

  // graft the output of the subtract filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( subtract->GetOutput() );
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
BlackTopHatImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Algorithm: " << m_Algorithm << std::endl;
  os << indent << "SafeBorder: " << m_SafeBorder << std::endl;
  os << indent << "ForceAlgorithm: " << m_ForceAlgorithm << std::endl;
}
} // end namespace itk
#endif

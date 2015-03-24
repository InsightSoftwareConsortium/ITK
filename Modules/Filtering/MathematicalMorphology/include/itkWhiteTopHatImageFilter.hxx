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
#ifndef itkWhiteTopHatImageFilter_hxx
#define itkWhiteTopHatImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkWhiteTopHatImageFilter.h"
#include "itkGrayscaleMorphologicalOpeningImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
WhiteTopHatImageFilter< TInputImage, TOutputImage, TKernel >
::WhiteTopHatImageFilter()
{
  m_SafeBorder = true;
  m_Algorithm = HISTO;
  m_ForceAlgorithm = false;
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
WhiteTopHatImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to an opening filter.
  typename GrayscaleMorphologicalOpeningImageFilter< TInputImage, TInputImage, TKernel >::Pointer
  open = GrayscaleMorphologicalOpeningImageFilter< TInputImage, TInputImage, TKernel >::New();

  open->SetInput( this->GetInput() );
  open->SetKernel( this->GetKernel() );
  open->SetSafeBorder(m_SafeBorder);
  if ( m_ForceAlgorithm )
    {
    open->SetAlgorithm(m_Algorithm);
    }
  else
    {
    m_Algorithm = open->GetAlgorithm();
    }

  // Need to subtract the opened image from the input
  typename SubtractImageFilter< TInputImage, TInputImage, TOutputImage >::Pointer
  subtract = SubtractImageFilter< TInputImage, TInputImage, TOutputImage >::New();

  subtract->SetInput1( this->GetInput() );
  subtract->SetInput2( open->GetOutput() );

  // graft our output to the subtract filter to force the proper regions
  // to be generated
  subtract->GraftOutput( this->GetOutput() );

  // run the algorithm
  progress->RegisterInternalFilter(open, .9f);
  progress->RegisterInternalFilter(subtract, .1f);

  subtract->Update();

  // graft the output of the subtract filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( subtract->GetOutput() );
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
WhiteTopHatImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Algorithm: " << m_Algorithm << std::endl;
  os << indent << "SafeBorder: " << m_SafeBorder << std::endl;
  os << indent << "ForceAlgorithm: " << m_ForceAlgorithm << std::endl;
}
} // end namespace itk
#endif

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
#ifndef itkNormalizeToConstantImageFilter_hxx
#define itkNormalizeToConstantImageFilter_hxx

#include "itkNormalizeToConstantImageFilter.h"

#include "itkDivideImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressAccumulator.h"
#include "itkStatisticsImageFilter.h"

namespace itk {

template < typename TInputImage, typename TOutputImage >
NormalizeToConstantImageFilter< TInputImage, TOutputImage >
::NormalizeToConstantImageFilter() :
  m_Constant( NumericTraits<RealType>::OneValue() )
{
}

template < typename TInputImage, typename TOutputImage >
void
NormalizeToConstantImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method.
  Superclass::GenerateInputRequestedRegion();

  InputImageType * input0 = const_cast<InputImageType *>( this->GetInput(0) );
  if ( !input0 )
    {
    return;
    }

  input0->SetRequestedRegion( input0->GetLargestPossibleRegion() );
}


template< typename TInputImage, typename TOutputImage >
void
NormalizeToConstantImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  this->AllocateOutputs();
  const InputImageType * input0 = this->GetInput(0);
  OutputImageType * output0     = this->GetOutput(0);

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  typedef typename itk::StatisticsImageFilter< InputImageType > StatType;
  typename StatType::Pointer stat = StatType::New();
  stat->SetInput( input0 );
  progress->RegisterInternalFilter( stat, 0.5f );
  stat->SetNumberOfThreads( this->GetNumberOfThreads() );
  stat->Update();

  typedef typename itk::DivideImageFilter< InputImageType,
                                           OutputImageType,
                                           OutputImageType > DivType;
  typename DivType::Pointer div = DivType::New();
  div->SetInput( input0 );
  div->SetConstant2( static_cast< RealType >( stat->GetSum() ) / m_Constant );
  div->SetNumberOfThreads( this->GetNumberOfThreads() );

  progress->RegisterInternalFilter( div, 0.5f );

  div->GraftOutput( output0 );
  div->Update();
  this->GraftOutput( div->GetOutput() );
}


template< typename TInputImage, typename TOutputImage >
void
NormalizeToConstantImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Constant: "  << m_Constant << std::endl;
}

}// end namespace itk
#endif

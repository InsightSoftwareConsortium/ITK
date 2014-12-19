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
#ifndef itkSampleToSubsampleFilter_hxx
#define itkSampleToSubsampleFilter_hxx

#include "itkSampleToSubsampleFilter.h"

namespace itk
{
namespace Statistics
{
template< typename TSample >
SampleToSubsampleFilter< TSample >
::SampleToSubsampleFilter()
{
  this->ProcessObject::SetNumberOfRequiredInputs(1);
  this->ProcessObject::SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );
}

template< typename TSample >
SampleToSubsampleFilter< TSample >
::~SampleToSubsampleFilter()
{}

template< typename TSample >
void
SampleToSubsampleFilter< TSample >
::SetInput(const SampleType *sample)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< SampleType * >( sample ) );
}

template< typename TSample >
const typename
SampleToSubsampleFilter< TSample >::SampleType *
SampleToSubsampleFilter< TSample >
::GetInput() const
{
  const SampleType *input =
    static_cast< const SampleType * >( this->ProcessObject::GetInput(0) );

  return input;
}

template< typename TSample >
typename SampleToSubsampleFilter< TSample >::DataObjectPointer
SampleToSubsampleFilter< TSample >
::MakeOutput(DataObjectPointerArraySizeType)
{
  return SubsampleType::New().GetPointer();
}

template< typename TSample >
const typename SampleToSubsampleFilter< TSample >::OutputType *
SampleToSubsampleFilter< TSample >
::GetOutput() const
{
  const SubsampleType *output =
    static_cast< const SubsampleType * >( this->ProcessObject::GetOutput(0) );

  return output;
}

template< typename TSample >
void
SampleToSubsampleFilter< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}
} // end of namespace Statistics
} // end of namespace itk

#endif

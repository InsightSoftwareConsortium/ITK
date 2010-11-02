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
#ifndef __itkMeanSampleFilter_txx
#define __itkMeanSampleFilter_txx

#include "itkMeasurementVectorTraits.h"

namespace itk
{
namespace Statistics
{
template< class TSample >
MeanSampleFilter< TSample >
::MeanSampleFilter()
{
  this->ProcessObject::SetNumberOfRequiredInputs(1);
  this->ProcessObject::SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );
}

template< class TSample >
MeanSampleFilter< TSample >
::~MeanSampleFilter()
{}

template< class TSample >
void
MeanSampleFilter< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< class TSample >
void
MeanSampleFilter< TSample >
::SetInput(const SampleType *sample)
{
  this->ProcessObject::SetNthInput( 0, const_cast< SampleType * >( sample ) );
}

template< class TSample >
const TSample *
MeanSampleFilter< TSample >
::GetInput() const
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }

  return static_cast< const SampleType * >( this->ProcessObject::GetInput(0) );
}

template< class TSample >
typename MeanSampleFilter< TSample >::DataObjectPointer
MeanSampleFilter< TSample >
::MakeOutput( unsigned int itkNotUsed(idx) )
{
  return static_cast< DataObject * >( MeasurementVectorDecoratedType::New().GetPointer() );
}

template< class TSample >
const typename MeanSampleFilter< TSample >::MeasurementVectorDecoratedType *
MeanSampleFilter< TSample >
::GetOutput() const
{
  return static_cast< const MeasurementVectorDecoratedType * >(
           this->ProcessObject::GetOutput(0) );
}

template< class TSample >
const typename MeanSampleFilter< TSample >::MeasurementVectorType
MeanSampleFilter< TSample >
::GetMean() const
{
  return this->GetOutput()->Get();
}

template< class TSample >
void
MeanSampleFilter< TSample >
::GenerateData()
{
  const SampleType *input = this->GetInput();

  MeasurementVectorSizeType measurementVectorSize =
    input->GetMeasurementVectorSize();

  MeasurementVectorDecoratedType *decoratedOutput =
    static_cast< MeasurementVectorDecoratedType * >(
      this->ProcessObject::GetOutput(0) );

  MeasurementVectorType output = decoratedOutput->Get();

  typename TSample::ConstIterator iter = input->Begin();
  typename TSample::ConstIterator end =  input->End();
  double totalFrequency = 0.0;

  for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
    {
    output[dim] = itk::NumericTraits< MeasurementType >::Zero;
    }

  while ( iter != end )
    {
    double frequency = iter.GetFrequency();
    totalFrequency += frequency;

    for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
      {
      output[dim] += iter.GetMeasurementVector()[dim] * frequency;
      }
    ++iter;
    }

  // compute the mean if the total frequency is different from zero
  if ( totalFrequency != 0.0 )
    {
    for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
      {
      output[dim] /= totalFrequency;
      }
    }

  decoratedOutput->Set(output);
}
} // end of namespace Statistics
} // end of namespace itk

#endif

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
#ifndef itkMeanSampleFilter_hxx
#define itkMeanSampleFilter_hxx

#include "itkMeanSampleFilter.h"

#include <vector>
#include "itkCompensatedSummation.h"
#include "itkMeasurementVectorTraits.h"

namespace itk
{
namespace Statistics
{
template< typename TSample >
MeanSampleFilter< TSample >
::MeanSampleFilter()
{
  this->ProcessObject::SetNumberOfRequiredInputs(1);
  this->ProcessObject::SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );
}

template< typename TSample >
MeanSampleFilter< TSample >
::~MeanSampleFilter()
{}

template< typename TSample >
void
MeanSampleFilter< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< typename TSample >
void
MeanSampleFilter< TSample >
::SetInput(const SampleType *sample)
{
  this->ProcessObject::SetNthInput( 0, const_cast< SampleType * >( sample ) );
}

template< typename TSample >
const TSample *
MeanSampleFilter< TSample >
::GetInput() const
{
  return itkDynamicCastInDebugMode< const SampleType * >( this->GetPrimaryInput() );
}

template< typename TSample >
typename MeanSampleFilter< TSample >::DataObjectPointer
MeanSampleFilter< TSample >
::MakeOutput( DataObjectPointerArraySizeType itkNotUsed(idx) )
{
  MeasurementVectorRealType mean;
  (void)mean; // for complainty pants : valgrind
  NumericTraits<MeasurementVectorRealType>::SetLength( mean, this->GetMeasurementVectorSize() );
  // NumericTraits::SetLength also initializes array to zero
  typename MeasurementVectorDecoratedType::Pointer decoratedMean = MeasurementVectorDecoratedType::New();
  decoratedMean->Set( mean );
  return decoratedMean.GetPointer();
}

template< typename TSample >
const typename MeanSampleFilter< TSample >::MeasurementVectorDecoratedType *
MeanSampleFilter< TSample >
::GetOutput() const
{
  return itkDynamicCastInDebugMode< const MeasurementVectorDecoratedType * >(this->ProcessObject::GetOutput(0) );
}

template< typename TSample >
const typename MeanSampleFilter< TSample >::MeasurementVectorRealType
MeanSampleFilter< TSample >
::GetMean() const
{
  const MeasurementVectorDecoratedType * decorator = this->GetOutput();
  return decorator->Get();
}

template< typename TSample >
typename MeanSampleFilter< TSample >::MeasurementVectorSizeType
MeanSampleFilter< TSample >
::GetMeasurementVectorSize() const
{
  const SampleType *input = this->GetInput();

  if ( input )
    {
    return input->GetMeasurementVectorSize();
    }

  // Test if the Vector type knows its length
  MeasurementVectorType     vector;
  MeasurementVectorSizeType measurementVectorSize = NumericTraits<MeasurementVectorType>::GetLength(vector);

  if ( measurementVectorSize )
    {
    return measurementVectorSize;
    }

  measurementVectorSize = 1; // Otherwise set it to an innocuous value

  return measurementVectorSize;
}


template< typename TSample >
void
MeanSampleFilter< TSample >
::GenerateData()
{
  // set up input / output
  const SampleType *input = this->GetInput();

  const MeasurementVectorSizeType measurementVectorSize =
    input->GetMeasurementVectorSize();

  MeasurementVectorDecoratedType *decoratedOutput =
    itkDynamicCastInDebugMode< MeasurementVectorDecoratedType * >( this->ProcessObject::GetOutput(0) );

  MeasurementVectorRealType output = decoratedOutput->Get();

  NumericTraits<MeasurementVectorRealType>::SetLength( output, this->GetMeasurementVectorSize() );

  // algorithm start
  typedef CompensatedSummation< MeasurementRealType > MeasurementRealAccumulateType;
  std::vector< MeasurementRealAccumulateType > sum( measurementVectorSize );

  typedef typename SampleType::TotalAbsoluteFrequencyType TotalFrequencyType;
  TotalFrequencyType totalFrequency = NumericTraits< TotalFrequencyType >::ZeroValue();

  typename SampleType::ConstIterator iter =      input->Begin();
  const typename SampleType::ConstIterator end = input->End();

  for (; iter != end; ++iter )
    {
    const MeasurementVectorType & measurement = iter.GetMeasurementVector();

    const typename SampleType::AbsoluteFrequencyType frequency = iter.GetFrequency();
    totalFrequency += frequency;

    for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
      {
      const MeasurementRealType component =
        static_cast< MeasurementRealType >( measurement[dim] );

      sum[dim] += ( component * static_cast< MeasurementRealType >( frequency ) );
      }
    }

  // compute the mean if the total frequency is different from zero
  if ( totalFrequency > itk::Math::eps )
    {
    for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
      {
      output[dim] = ( sum[dim].GetSum() / static_cast< MeasurementRealType >( totalFrequency ) );
      }
    }
  else
    {
    itkExceptionMacro("Total frequency was too close to zero: " << totalFrequency );
    }

  decoratedOutput->Set( output );
}
} // end of namespace Statistics
} // end of namespace itk

#endif

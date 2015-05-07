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
#ifndef itkSampleToHistogramFilter_hxx
#define itkSampleToHistogramFilter_hxx

#include "itkSampleToHistogramFilter.h"
#include "itkStatisticsAlgorithm.h"

namespace itk
{
namespace Statistics
{
template< typename TSample, typename THistogram >
SampleToHistogramFilter< TSample, THistogram >
::SampleToHistogramFilter()
{
  this->ProcessObject::SetNumberOfRequiredInputs(1);
  this->ProcessObject::SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );

  this->SetMarginalScale(100);

  this->SetAutoMinimumMaximum(true);
}

template< typename TSample, typename THistogram >
SampleToHistogramFilter< TSample, THistogram >
::~SampleToHistogramFilter()
{}

template< typename TSample, typename THistogram >
void
SampleToHistogramFilter< TSample, THistogram >
::SetInput(const SampleType *sample)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< SampleType * >( sample ) );
}

template< typename TSample, typename THistogram >
const typename
SampleToHistogramFilter< TSample, THistogram >::SampleType *
SampleToHistogramFilter< TSample, THistogram >
::GetInput() const
{
  const SampleType *input =
    static_cast< const SampleType * >( this->ProcessObject::GetInput(0) );

  return input;
}

template< typename TSample, typename THistogram >
const typename
SampleToHistogramFilter< TSample, THistogram >::HistogramType *
SampleToHistogramFilter< TSample, THistogram >
::GetOutput() const
{
  const HistogramType *output =
    static_cast< const HistogramType * >( this->ProcessObject::GetOutput(0) );

  return output;
}

template< typename TSample, typename THistogram >
void
SampleToHistogramFilter< TSample, THistogram >
::GraftOutput(DataObject *graft)
{
  DataObject *output =
    const_cast< HistogramType * >( this->GetOutput() );

  // Call Histogram to copy meta-information, and the container
  output->Graft(graft);
}

template< typename TSample, typename THistogram >
typename SampleToHistogramFilter< TSample, THistogram >::DataObjectPointer
SampleToHistogramFilter< TSample, THistogram >
::MakeOutput(DataObjectPointerArraySizeType)
{
  return HistogramType::New().GetPointer();
}

template< typename TSample, typename THistogram >
void
SampleToHistogramFilter< TSample, THistogram >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  // m_AutoMinimumMaximum
  os << indent << "AutoMinimumMaximum: "
     << this->GetAutoMinimumMaximumInput() << std::endl;
  // m_MarginalScale
  os << indent << "MarginalScale: "
     << this->GetMarginalScaleInput() << std::endl;
  // m_HistogramBinMinimum
  os << indent << "HistogramBinMinimum: "
     << this->GetHistogramBinMinimumInput() << std::endl;
  // m_HistogramBinMaximum
  os << indent << "HistogramBinMaximum: "
     << this->GetHistogramBinMaximumInput() << std::endl;
  // m_HistogramSize
  os << indent << "HistogramSize: "
     << this->GetHistogramSizeInput() << std::endl;
}

template< typename TSample, typename THistogram >
void
SampleToHistogramFilter< TSample, THistogram >
::GenerateData()
{
  const SampleType *inputSample = this->GetInput();

  const InputHistogramMeasurementVectorObjectType *binMinimumObject =
    this->GetHistogramBinMinimumInput();

  const InputHistogramMeasurementVectorObjectType *binMaximumObject =
    this->GetHistogramBinMaximumInput();

  const InputHistogramMeasurementObjectType *marginalScaleObject =
    this->GetMarginalScaleInput();

  const InputBooleanObjectType *autoMinimumMaximum =
    this->GetAutoMinimumMaximumInput();

  const InputHistogramSizeObjectType *histogramSizeObject =
    this->GetHistogramSizeInput();

  if ( histogramSizeObject == ITK_NULLPTR )
    {
    itkSpecializedExceptionMacro(MissingHistogramSizeInput);
    }

  if ( marginalScaleObject == ITK_NULLPTR )
    {
    itkSpecializedExceptionMacro(MissingHistogramMarginalScaleInput);
    }

  HistogramSizeType histogramSize = histogramSizeObject->Get();

  HistogramMeasurementType marginalScale = marginalScaleObject->Get();

  HistogramType *outputHistogram =
    static_cast< HistogramType * >( this->ProcessObject::GetOutput(0) );

  const typename SampleType::InstanceIdentifier measurementVectorSize =
    inputSample->GetMeasurementVectorSize();

  if ( measurementVectorSize == 0 )
    {
    itkSpecializedExceptionMacro(NullSizeHistogramInputMeasurementVectorSize);
    }

  if ( histogramSize.Size() != measurementVectorSize )
    {
    itkSpecializedMessageExceptionMacro(HistogramWrongNumberOfComponents,
      "Histogram number of components: "
      << histogramSize.Size()
      << " doesn't match Measurement Vector Size: "
      << measurementVectorSize);
    }

  outputHistogram->SetMeasurementVectorSize(measurementVectorSize);

  typename SampleType::MeasurementVectorType lower;
  typename SampleType::MeasurementVectorType upper;

  NumericTraits<typename SampleType::MeasurementVectorType>::SetLength(lower,
    measurementVectorSize);
  NumericTraits<typename SampleType::MeasurementVectorType>::SetLength(upper,
    measurementVectorSize);

  HistogramMeasurementVectorType h_upper;
  HistogramMeasurementVectorType h_lower;

  NumericTraits<HistogramMeasurementVectorType>::SetLength(h_lower,
    measurementVectorSize);
  NumericTraits<HistogramMeasurementVectorType>::SetLength(h_upper,
    measurementVectorSize);

  const HistogramMeasurementType maximumPossibleValue =
    itk::NumericTraits< HistogramMeasurementType >::max();

  if ( autoMinimumMaximum && autoMinimumMaximum->Get() )
    {
    if ( inputSample->Size() )
      {
      Algorithm::FindSampleBound(
        inputSample,  inputSample->Begin(), inputSample->End(), lower, upper);

      for ( unsigned int i = 0; i < measurementVectorSize; i++ )
        {
        if ( !NumericTraits< HistogramMeasurementType >::is_integer )
          {
          const double margin =
            ( static_cast< HistogramMeasurementType >( upper[i] - lower[i] )
              / static_cast< HistogramMeasurementType >( histogramSize[i] ) )
            / static_cast< HistogramMeasurementType >( marginalScale );

          // Now we check if the upper[i] value can be increased by
          // the margin value without saturating the capacity of the
          // HistogramMeasurementType
          if ( ( maximumPossibleValue - upper[i] ) > margin )
            {
            h_upper[i] = static_cast< HistogramMeasurementType >( upper[i] + margin );
            }
          else
            {
            // an overflow would occur if we add 'margin' to the upper
            // therefore we just compromise in setting h_upper = upper.
            h_upper[i] = static_cast< HistogramMeasurementType >( upper[i] );
            // Histogram measurement type would force the clipping the max
            // value.
            // Therefore we must call the following to include the max value:
            outputHistogram->SetClipBinsAtEnds(false);
            // The above function is okay since here we are within the
            // autoMinMax
            // computation and clearly the user intended to include min and max.
            }
          }
        else
          {
          // h_upper[i] = SafeAssign(upper[i] + NumericTraits<MeasurementType>::OneValue());
          // if ( h_upper[i] <= upper[i] )
          if(upper[i] <
             (static_cast<MeasurementType>
              (NumericTraits<HistogramMeasurementType>::max()) -
              NumericTraits<MeasurementType>::OneValue()))
            {
            h_upper[i] = static_cast<HistogramMeasurementType>
              (upper[i] + NumericTraits<MeasurementType>::OneValue());
            }
          else
            {
            // an overflow would have occurred, therefore set upper to upper
            h_upper[i] = SafeAssign( upper[i] );
            // Histogram measurement type would force the clipping the max
            // value.
            // Therefore we must call the following to include the max value:
            outputHistogram->SetClipBinsAtEnds(false);
            // The above function is okay since here we are within the
            // autoMinMax
            // computation and clearly the user intended to include min and max.
            }
          }
        h_lower[i] = SafeAssign( lower[i] );
        }
      }
    else
      {
      for ( unsigned int i = 0; i < measurementVectorSize; i++ )
        {
        h_lower[i] = static_cast< HistogramMeasurementType >( lower[i] );
        h_upper[i] = static_cast< HistogramMeasurementType >( upper[i] );
        }
      }
    }
  else
    {
    if ( binMaximumObject == ITK_NULLPTR )
      {
      itkSpecializedExceptionMacro(MissingHistogramBinMaximumInput);
      }

    if ( binMinimumObject == ITK_NULLPTR )
      {
      itkSpecializedExceptionMacro(MissingHistogramBinMinimumInput);
      }

    h_upper = binMaximumObject->Get();
    h_lower = binMinimumObject->Get();
    }

  // initialize the Histogram object using the sizes and
  // the upper and lower bound from the FindSampleBound function
  outputHistogram->Initialize(histogramSize, h_lower, h_upper);

  typename SampleType::ConstIterator iter = inputSample->Begin();
  typename SampleType::ConstIterator last = inputSample->End();

  typename SampleType::MeasurementVectorType lvector;

  typename HistogramType::IndexType             index(measurementVectorSize);
  typename HistogramType::MeasurementVectorType hvector(measurementVectorSize);

  unsigned int i;

  while ( iter != last )
    {
    lvector = iter.GetMeasurementVector();
    for ( i = 0; i < inputSample->GetMeasurementVectorSize(); i++ )
      {
      hvector[i] =
        SafeAssign(lvector[i]);
      }

    outputHistogram->GetIndex(hvector, index);
    if ( !outputHistogram->IsIndexOutOfBounds(index) )
      {
      // if the measurement vector is out of bound then
      // the GetIndex method has returned an index set to the max size of
      // the invalid dimension - even if the hvector is less than the minimum
      // bin value.
      // If the index isn't valid, we don't increase the frequency.
      // See the comments in Histogram->GetIndex() for more info.
      outputHistogram->IncreaseFrequencyOfIndex(index, 1);
      }
    ++iter;
    }
}
} // end of namespace Statistics
} // end of namespace itk

#endif

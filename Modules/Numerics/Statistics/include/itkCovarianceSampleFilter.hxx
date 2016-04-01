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
#ifndef itkCovarianceSampleFilter_hxx
#define itkCovarianceSampleFilter_hxx

#include "itkCovarianceSampleFilter.h"
#include "itkMeanSampleFilter.h"

namespace itk
{
namespace Statistics
{
template< typename TSample >
CovarianceSampleFilter< TSample >
::CovarianceSampleFilter()
{
  this->ProcessObject::SetNumberOfRequiredInputs(1);
  this->ProcessObject::SetNumberOfRequiredOutputs(2);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );
  this->ProcessObject::SetNthOutput( 1, this->MakeOutput(1) );
}

template< typename TSample >
CovarianceSampleFilter< TSample >
::~CovarianceSampleFilter()
{}

template< typename TSample >
void
CovarianceSampleFilter< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< typename TSample >
void
CovarianceSampleFilter< TSample >
::SetInput(const SampleType *sample)
{
  this->ProcessObject::SetNthInput( 0, const_cast< SampleType * >( sample ) );
}

template< typename TSample >
const TSample *
CovarianceSampleFilter< TSample >
::GetInput() const
{
  return itkDynamicCastInDebugMode< const SampleType * >( this->GetPrimaryInput() );
}

template< typename TSample >
typename CovarianceSampleFilter< TSample >::DataObjectPointer
CovarianceSampleFilter< TSample >
::MakeOutput(DataObjectPointerArraySizeType index)
{
  MeasurementVectorSizeType measurementVectorSize = this->GetMeasurementVectorSize();

  if ( index == 0 )
    {
    MatrixType covarianceMatrix(measurementVectorSize, measurementVectorSize);
    covarianceMatrix.SetIdentity();
    typename MatrixDecoratedType::Pointer decoratedCovarianceMatrix = MatrixDecoratedType::New();
    decoratedCovarianceMatrix->Set(covarianceMatrix);
    return decoratedCovarianceMatrix.GetPointer();
    }

  if ( index == 1 )
    {
    MeasurementVectorRealType mean;
    (void)mean; // for complainty pants : valgrind
    NumericTraits<MeasurementVectorRealType>::SetLength(mean, this->GetMeasurementVectorSize());
    // NumericTraits::SetLength also initializes array to zero
    typename MeasurementVectorDecoratedType::Pointer decoratedMean = MeasurementVectorDecoratedType::New();
    decoratedMean->Set( mean );
    return decoratedMean.GetPointer();
    }
  itkExceptionMacro("Trying to create output of index " << index << " larger than the number of output");
}

template< typename TSample >
typename CovarianceSampleFilter< TSample >::MeasurementVectorSizeType
CovarianceSampleFilter< TSample >
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
inline void
CovarianceSampleFilter< TSample >
::GenerateData()
{
  // set up input / output
  const SampleType *input = this->GetInput();

  MeasurementVectorSizeType measurementVectorSize = input->GetMeasurementVectorSize();

  MatrixDecoratedType *decoratedOutput =
    itkDynamicCastInDebugMode< MatrixDecoratedType * >( this->ProcessObject::GetOutput(0) );

  MatrixType output = decoratedOutput->Get();
  output.SetSize( measurementVectorSize, measurementVectorSize );
  output.Fill( NumericTraits< typename MatrixType::ValueType >::ZeroValue() );

  MeasurementVectorDecoratedType *decoratedMeanOutput =
    itkDynamicCastInDebugMode< MeasurementVectorDecoratedType * >( this->ProcessObject::GetOutput(1) );

  // calculate mean
  typedef MeanSampleFilter< SampleType > MeanFilterType;
  typename MeanFilterType::Pointer meanFilter = MeanFilterType::New();

  meanFilter->SetInput( input );
  meanFilter->Update();

  const typename MeanFilterType::MeasurementVectorRealType mean = meanFilter->GetMean();
  decoratedMeanOutput->Set( mean );

  // covariance algorithm
  MeasurementVectorRealType diff;
  NumericTraits<MeasurementVectorRealType>::SetLength( diff, measurementVectorSize );

  typedef typename SampleType::TotalAbsoluteFrequencyType TotalFrequencyType;
  TotalFrequencyType totalFrequency = NumericTraits< TotalFrequencyType >::ZeroValue();

  typename SampleType::ConstIterator iter =      input->Begin();
  const typename SampleType::ConstIterator end = input->End();

  // fills the lower triangle and the diagonal cells in the covariance matrix
  for (; iter != end; ++iter )
    {
    const MeasurementVectorType & measurement = iter.GetMeasurementVector();

    const typename SampleType::AbsoluteFrequencyType frequency = iter.GetFrequency();
    totalFrequency += frequency;

    for ( unsigned int dim = 0; dim < measurementVectorSize; ++dim )
      {
      const MeasurementRealType component =
        static_cast< MeasurementRealType >( measurement[dim] );

      diff[dim] = ( component - mean[dim] );
      }

    // updates the covariance matrix
    for ( unsigned int row = 0; row < measurementVectorSize; ++row )
      {
      for ( unsigned int col = 0; col < row + 1; ++col )
        {
        output(row, col) +=
          ( static_cast< MeasurementRealType >( frequency ) * diff[row] * diff[col] );
        }
      }
    }

  // fills the upper triangle using the lower triangle
  for ( unsigned int row = 1; row < measurementVectorSize; ++row )
    {
    for ( unsigned int col = 0; col < row; ++col )
      {
      output(col, row) = output(row, col);
      }
    }

  const double normalizationFactor =
    ( static_cast< MeasurementRealType >( totalFrequency ) - 1.0 );

  if( normalizationFactor > itk::Math::eps )
    {
    const double inverseNormalizationFactor = 1.0 / normalizationFactor;

    output *= inverseNormalizationFactor;
    }
  else
    {
    itkExceptionMacro("Total Frequency was too close to 1.0. Value = " << totalFrequency );
    }

  decoratedOutput->Set( output );
}

template< typename TSample >
const typename CovarianceSampleFilter< TSample >::MatrixDecoratedType *
CovarianceSampleFilter< TSample >
::GetCovarianceMatrixOutput() const
{
  return static_cast< const MatrixDecoratedType * >( this->ProcessObject::GetOutput(0) );
}

template< typename TSample >
const typename CovarianceSampleFilter< TSample >::MatrixType
CovarianceSampleFilter< TSample >
::GetCovarianceMatrix() const
{
  return this->GetCovarianceMatrixOutput()->Get();
}

template< typename TSample >
const typename CovarianceSampleFilter< TSample >::MeasurementVectorDecoratedType *
CovarianceSampleFilter< TSample >
::GetMeanOutput() const
{
  return static_cast< const MeasurementVectorDecoratedType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TSample >
const typename CovarianceSampleFilter< TSample >::MeasurementVectorRealType
CovarianceSampleFilter< TSample >
::GetMean() const
{
  return this->GetMeanOutput()->Get();
}
} // end of namespace Statistics
} // end of namespace itk

#endif

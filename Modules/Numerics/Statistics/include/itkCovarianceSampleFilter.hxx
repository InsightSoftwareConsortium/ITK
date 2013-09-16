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
#ifndef __itkCovarianceSampleFilter_hxx
#define __itkCovarianceSampleFilter_hxx

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
    MatrixDecoratedType::Pointer decoratedCovarianceMatrix = MatrixDecoratedType::New();
    decoratedCovarianceMatrix->Set(covarianceMatrix);
    return decoratedCovarianceMatrix.GetPointer();
    }

  if ( index == 1 )
    {
    MeasurementVectorRealType mean;
    (void)mean; // for complainty pants : valgrind
    NumericTraits<MeasurementVectorRealType>::SetLength(mean, this->GetMeasurementVectorSize());
    mean.Fill( NumericTraits< MeasurementRealType >::Zero );
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
  const SampleType *input = this->GetInput();

  MeasurementVectorSizeType measurementVectorSize = input->GetMeasurementVectorSize();

  MatrixDecoratedType *decoratedOutput =
    itkDynamicCastInDebugMode< MatrixDecoratedType * >( this->ProcessObject::GetOutput(0) );

  MatrixType output = decoratedOutput->Get();

  MeasurementVectorDecoratedType *decoratedMeanOutput =
    itkDynamicCastInDebugMode< MeasurementVectorDecoratedType * >(this->ProcessObject::GetOutput(1));

  output.SetSize(measurementVectorSize, measurementVectorSize);
  output.Fill(0.0);

  double totalFrequency = 0.0;

  typename TSample::ConstIterator iter = input->Begin();
  typename TSample::ConstIterator end = input->End();

  MeasurementVectorRealType diff;
  MeasurementVectorType measurements;

  NumericTraits<MeasurementVectorRealType>::SetLength(diff, measurementVectorSize);
  NumericTraits<MeasurementVectorType>::SetLength(measurements, measurementVectorSize);

  typedef MeanSampleFilter< TSample > MeanFilterType;
  typename MeanFilterType::Pointer meanFilter = MeanFilterType::New();

  meanFilter->SetInput( input );
  meanFilter->Update();

  const typename MeanFilterType::MeasurementVectorDecoratedType * decorator = meanFilter->GetOutput();
  const typename MeanFilterType::MeasurementVectorRealType mean = decorator->Get();

  decoratedMeanOutput->Set( mean );

  iter = input->Begin();

  // fills the lower triangle and the diagonal cells in the covariance matrix
  while ( iter != end )
    {
    const double frequency = iter.GetFrequency();
    totalFrequency += frequency;
    measurements = iter.GetMeasurementVector();

    for ( unsigned int i = 0; i < measurementVectorSize; ++i )
      {
      diff[i] = static_cast< MeasurementRealType >( measurements[i] ) - mean[i];
      }

    // updates the covariance matrix
    for ( unsigned int row = 0; row < measurementVectorSize; ++row )
      {
      for ( unsigned int col = 0; col < row + 1; ++col )
        {
        output(row, col) += frequency * diff[row] * diff[col];
        }
      }
    ++iter;
    }

  // fills the upper triangle using the lower triangle
  for ( unsigned int row = 1; row < measurementVectorSize; ++row )
    {
    for ( unsigned int col = 0; col < row; ++col )
      {
      output(col, row) = output(row, col);
      }
    }

  if( totalFrequency - 1.0 > vnl_math::eps )
    {
    const double factor = 1.0 / ( totalFrequency - 1.0 );

    for ( unsigned int col = 0; col < measurementVectorSize; ++col )
      {
      for ( unsigned int row = 0; row < measurementVectorSize; ++row )
        {
        output(col, row) *= factor;
        }
      }
    decoratedOutput->Set(output);
    }
  else
    {
    itkExceptionMacro("Total Frequency was too close to 1.0. Value = " << totalFrequency );
    }
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

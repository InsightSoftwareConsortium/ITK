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
#ifndef itkWeightedCovarianceSampleFilter_hxx
#define itkWeightedCovarianceSampleFilter_hxx

#include "itkWeightedCovarianceSampleFilter.h"
#include "itkWeightedMeanSampleFilter.h"

namespace itk
{
namespace Statistics
{
template< typename TSample >
WeightedCovarianceSampleFilter< TSample >
::WeightedCovarianceSampleFilter()
{
  this->ProcessObject::SetNthInput(1, ITK_NULLPTR);
}

template< typename TSample >
WeightedCovarianceSampleFilter< TSample >
::~WeightedCovarianceSampleFilter()
{}

template< typename TSample >
void
WeightedCovarianceSampleFilter< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  // m_Weights
  os << indent << "Weights: " << this->GetWeightsInput() << std::endl;
  // m_WeightingFunction
  os << indent << "WeightingFunction: " << this->GetWeightingFunctionInput() << std::endl;
}

template< typename TSample >
inline void
WeightedCovarianceSampleFilter< TSample >
::GenerateData()
{
  // if weighting function is specifed, use it to compute the mean
  const InputWeightingFunctionObjectType *functionObject =
    this->GetWeightingFunctionInput();

  if ( functionObject != ITK_NULLPTR )
    {
    this->ComputeCovarianceMatrixWithWeightingFunction();
    return;
    }

  // if weight array is specified use it to compute the covariance
  const InputWeightArrayObjectType *weightArrayObject =
    this->GetWeightsInput();

  if ( weightArrayObject != ITK_NULLPTR )
    {
    this->ComputeCovarianceMatrixWithWeights();
    return;
    }

  // Otherwise compute the regular covariance matrix ( without weight
  // coefficients)
  Superclass::GenerateData();
}

template< typename TSample >
inline void
WeightedCovarianceSampleFilter< TSample >
::ComputeCovarianceMatrixWithWeightingFunction()
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
  const WeightingFunctionType * const weightingFunction = this->GetWeightingFunction();

  typedef WeightedMeanSampleFilter< SampleType > WeightedMeanFilterType;
  typename WeightedMeanFilterType::Pointer meanFilter = WeightedMeanFilterType::New();

  meanFilter->SetInput( input );
  meanFilter->SetWeightingFunction( weightingFunction );
  meanFilter->Update();

  const typename WeightedMeanFilterType::MeasurementVectorRealType mean = meanFilter->GetMean();
  decoratedMeanOutput->Set( mean );

  // covariance algorithm
  MeasurementVectorRealType diff;
  NumericTraits<MeasurementVectorRealType>::SetLength( diff, measurementVectorSize );

  WeightValueType totalWeight = NumericTraits< WeightValueType >::ZeroValue();

  WeightValueType totalSquaredWeight = NumericTraits< WeightValueType >::ZeroValue();

  typename SampleType::ConstIterator iter =      input->Begin();
  const typename SampleType::ConstIterator end = input->End();

  // fills the lower triangle and the diagonal cells in the covariance matrix
  for (; iter != end; ++iter )
    {
    const MeasurementVectorType & measurement = iter.GetMeasurementVector();

    const typename SampleType::AbsoluteFrequencyType frequency = iter.GetFrequency();

    const WeightValueType rawWeight = weightingFunction->Evaluate( measurement );

    const WeightValueType weight = ( rawWeight * static_cast< WeightValueType >( frequency ) );
    totalWeight += weight;
    totalSquaredWeight += ( weight * weight );

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
          ( static_cast< MeasurementRealType >( weight ) * diff[row] * diff[col] );
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

  const double normalizationFactor = ( totalWeight - ( totalSquaredWeight / totalWeight ) );

  if( normalizationFactor > itk::Math::eps )
    {
    const double inverseNormalizationFactor = 1.0 / normalizationFactor;

    output *= inverseNormalizationFactor;
    }
  else
    {
    itkExceptionMacro("Normalization factor was too close to zero. Value = " << normalizationFactor );
    }

  decoratedOutput->Set( output );
}

template< typename TSample >
inline void
WeightedCovarianceSampleFilter< TSample >
::ComputeCovarianceMatrixWithWeights()
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
  const WeightArrayType & weightsArray = this->GetWeights();

  typedef WeightedMeanSampleFilter< SampleType > WeightedMeanFilterType;
  typename WeightedMeanFilterType::Pointer meanFilter = WeightedMeanFilterType::New();

  meanFilter->SetInput( input );
  meanFilter->SetWeights( weightsArray );
  meanFilter->Update();

  const typename WeightedMeanFilterType::MeasurementVectorRealType mean = meanFilter->GetMean();
  decoratedMeanOutput->Set( mean );

  // covariance algorithm
  MeasurementVectorRealType diff;
  NumericTraits<MeasurementVectorRealType>::SetLength( diff, measurementVectorSize );

  WeightValueType totalWeight = NumericTraits< WeightValueType >::ZeroValue();

  WeightValueType totalSquaredWeight = NumericTraits< WeightValueType >::ZeroValue();

  typename SampleType::ConstIterator iter =      input->Begin();
  const typename SampleType::ConstIterator end = input->End();

  // fills the lower triangle and the diagonal cells in the covariance matrix
  for ( unsigned int sampleVectorIndex = 0;
        iter != end;
        ++iter, ++sampleVectorIndex )
    {
    const MeasurementVectorType & measurement = iter.GetMeasurementVector();

    const typename SampleType::AbsoluteFrequencyType frequency = iter.GetFrequency();

    const WeightValueType rawWeight = weightsArray[sampleVectorIndex];

    const WeightValueType weight = ( rawWeight * static_cast< WeightValueType >( frequency ) );
    totalWeight += weight;
    totalSquaredWeight += ( weight * weight );

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
          ( static_cast< MeasurementRealType >( weight ) * diff[row] * diff[col] );
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

  const double normalizationFactor = ( totalWeight - ( totalSquaredWeight / totalWeight ) );

  if( normalizationFactor > itk::Math::eps )
    {
    const double inverseNormalizationFactor = 1.0 / normalizationFactor;

    output *= inverseNormalizationFactor;
    }
  else
    {
    itkExceptionMacro("Normalization factor was too close to zero. Value = " << normalizationFactor );
    }

  decoratedOutput->Set( output );
}
} // end of namespace Statistics
} // end of namespace itk

#endif

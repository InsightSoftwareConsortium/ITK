/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedCovarianceSampleFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWeightedCovarianceSampleFilter_txx
#define __itkWeightedCovarianceSampleFilter_txx

namespace itk
{
namespace Statistics
{
template< class TSample >
WeightedCovarianceSampleFilter< TSample >
::WeightedCovarianceSampleFilter()
{
  this->ProcessObject::SetNthInput(1, NULL);
}

template< class TSample >
WeightedCovarianceSampleFilter< TSample >
::~WeightedCovarianceSampleFilter()
{}

template< class TSample >
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

template< class TSample >
inline void
WeightedCovarianceSampleFilter< TSample >
::GenerateData()
{
  // if weighting function is specifed, use it to compute the mean
  const InputWeightingFunctionObjectType *functionObject =
    this->GetWeightingFunctionInput();

  if ( functionObject != NULL )
    {
    this->ComputeCovarianceMatrixWithWeightingFunction();
    return;
    }

  // if weight array is specified use it to compute the covariance
  const InputWeightArrayObjectType *weightArrayObject =
    this->GetWeightsInput();

  if ( weightArrayObject != NULL )
    {
    this->ComputeCovarianceMatrixWithWeights();
    return;
    }

  // Otherwise compute the regular covariance matrix ( without weight
  // coefficients)
  Superclass::GenerateData();
  return;
}

template< class TSample >
inline void
WeightedCovarianceSampleFilter< TSample >
::ComputeCovarianceMatrixWithWeightingFunction()
{
  const SampleType *input = this->GetInput();

  MeasurementVectorSizeType measurementVectorSize =
    input->GetMeasurementVectorSize();

  MatrixDecoratedType *decoratedOutput =
    static_cast< MatrixDecoratedType * >(
      this->ProcessObject::GetOutput(0) );

  MatrixType output = decoratedOutput->Get();

  MeasurementVectorDecoratedType *decoratedMeanOutput =
    static_cast< MeasurementVectorDecoratedType * >(
      this->ProcessObject::GetOutput(1) );

  output.SetSize(measurementVectorSize, measurementVectorSize);
  output.Fill(0.0);

  MeasurementVectorType mean;
  MeasurementVectorTraits::SetLength(mean, measurementVectorSize);
  mean.Fill(0.0);

  typename TSample::ConstIterator iter = input->Begin();
  typename TSample::ConstIterator end = input->End();

  MeasurementVectorType diff;
  MeasurementVectorTraits::SetLength(diff, measurementVectorSize);
  MeasurementVectorType measurements;
  MeasurementVectorTraits::SetLength(measurements, measurementVectorSize);

  double weight;
  double totalWeight = 0.0;
  double sumSquaredWeight = 0.0;

  // if weighting function is specifed, use it to compute the mean
  const InputWeightingFunctionObjectType *functionObject =
    this->GetWeightingFunctionInput();

  const WeightingFunctionType *weightFunction = functionObject->Get();

  //Compute the mean first
  while ( iter != end )
    {
    measurements = iter.GetMeasurementVector();
    weight = iter.GetFrequency() * weightFunction->Evaluate(measurements);
    totalWeight += weight;
    sumSquaredWeight += weight * weight;

    for ( unsigned int i = 0; i < measurementVectorSize; ++i )
      {
      mean[i] += weight * measurements[i];
      }
    ++iter;
    }

  for ( unsigned int i = 0; i < measurementVectorSize; ++i )
    {
    mean[i] = mean[i] / totalWeight;
    }

  decoratedMeanOutput->Set(mean);

  //reset iterator
  iter = input->Begin();
  // fills the lower triangle and the diagonal cells in the covariance matrix
  while ( iter != end )
    {
    measurements = iter.GetMeasurementVector();
    weight = iter.GetFrequency() * weightFunction->Evaluate(measurements);
    for ( unsigned int i = 0; i < measurementVectorSize; ++i )
      {
      diff[i] = measurements[i] - mean[i];
      }

    // updates the covariance matrix
    for ( unsigned int row = 0; row < measurementVectorSize; row++ )
      {
      for ( unsigned int col = 0; col < row + 1; col++ )
        {
        output(row, col) += weight * diff[row] * diff[col];
        }
      }
    ++iter;
    }

  // fills the upper triangle using the lower triangle
  for ( unsigned int row = 1; row < measurementVectorSize; row++ )
    {
    for ( unsigned int col = 0; col < row; col++ )
      {
      output(col, row) = output(row, col);
      }
    }

  output /= ( totalWeight - ( sumSquaredWeight / totalWeight ) );

  decoratedOutput->Set(output);
}

template< class TSample >
inline void
WeightedCovarianceSampleFilter< TSample >
::ComputeCovarianceMatrixWithWeights()
{
  const SampleType *input = this->GetInput();

  MeasurementVectorSizeType measurementVectorSize =
    input->GetMeasurementVectorSize();

  MatrixDecoratedType *decoratedOutput =
    static_cast< MatrixDecoratedType * >(
      this->ProcessObject::GetOutput(0) );

  MatrixType output = decoratedOutput->Get();

  MeasurementVectorDecoratedType *decoratedMeanOutput =
    static_cast< MeasurementVectorDecoratedType * >(
      this->ProcessObject::GetOutput(1) );

  output.SetSize(measurementVectorSize, measurementVectorSize);
  output.Fill(0.0);

  MeasurementVectorType mean;
  MeasurementVectorTraits::SetLength(mean, measurementVectorSize);
  mean.Fill(0.0);

  typename TSample::ConstIterator iter = input->Begin();
  typename TSample::ConstIterator end = input->End();

  MeasurementVectorType diff;
  MeasurementVectorTraits::SetLength(diff, measurementVectorSize);
  MeasurementVectorType measurements;
  MeasurementVectorTraits::SetLength(measurements, measurementVectorSize);

  double weight;
  double totalWeight = 0.0;
  double sumSquaredWeight = 0.0;

  const InputWeightArrayObjectType *weightArrayObject = this->GetWeightsInput();
  const WeightArrayType             weightArray = weightArrayObject->Get();

  //Compute the mean first
  unsigned int measurementVectorIndex = 0;
  while ( iter != end )
    {
    measurements = iter.GetMeasurementVector();
    weight = iter.GetFrequency() * ( weightArray )[measurementVectorIndex];
    totalWeight += weight;
    sumSquaredWeight += weight * weight;

    for ( unsigned int i = 0; i < measurementVectorSize; ++i )
      {
      mean[i] += weight * measurements[i];
      }
    ++iter;
    ++measurementVectorIndex;
    }

  for ( unsigned int i = 0; i < measurementVectorSize; ++i )
    {
    mean[i] = mean[i] / totalWeight;
    }

  decoratedMeanOutput->Set(mean);

  //reset iterator
  iter = input->Begin();
  // fills the lower triangle and the diagonal cells in the covariance matrix
  measurementVectorIndex = 0;
  while ( iter != end )
    {
    weight = iter.GetFrequency() * ( weightArray )[measurementVectorIndex];
    measurements = iter.GetMeasurementVector();

    for ( unsigned int i = 0; i < measurementVectorSize; ++i )
      {
      diff[i] = measurements[i] - mean[i];
      }

    // updates the covariance matrix
    for ( unsigned int row = 0; row < measurementVectorSize; row++ )
      {
      for ( unsigned int col = 0; col < row + 1; col++ )
        {
        output(row, col) += weight * diff[row] * diff[col];
        }
      }
    ++iter;
    ++measurementVectorIndex;
    }

  // fills the upper triangle using the lower triangle
  for ( unsigned int row = 1; row < measurementVectorSize; row++ )
    {
    for ( unsigned int col = 0; col < row; col++ )
      {
      output(col, row) = output(row, col);
      }
    }

  output /= ( totalWeight - ( sumSquaredWeight / totalWeight ) );

  decoratedOutput->Set(output);
}
} // end of namespace Statistics
} // end of namespace itk

#endif

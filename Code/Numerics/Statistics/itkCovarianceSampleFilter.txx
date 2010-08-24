/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovarianceSampleFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCovarianceSampleFilter_txx
#define __itkCovarianceSampleFilter_txx

#include "itkMeasurementVectorTraits.h"

namespace itk
{
namespace Statistics
{
template< class TSample >
CovarianceSampleFilter< TSample >
::CovarianceSampleFilter()
{
  this->ProcessObject::SetNumberOfRequiredInputs(1);
  this->ProcessObject::SetNumberOfRequiredOutputs(2);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );
  this->ProcessObject::SetNthOutput( 1, this->MakeOutput(1) );
}

template< class TSample >
CovarianceSampleFilter< TSample >
::~CovarianceSampleFilter()
{}

template< class TSample >
void
CovarianceSampleFilter< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< class TSample >
void
CovarianceSampleFilter< TSample >
::SetInput(const SampleType *sample)
{
  this->ProcessObject::SetNthInput( 0, const_cast< SampleType * >( sample ) );
}

template< class TSample >
const TSample *
CovarianceSampleFilter< TSample >
::GetInput() const
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }

  return static_cast< const SampleType * >
         ( this->ProcessObject::GetInput(0) );
}

template< class TSample >
typename CovarianceSampleFilter< TSample >::DataObjectPointer
CovarianceSampleFilter< TSample >
::MakeOutput(unsigned int index)
{
  MeasurementVectorSizeType measurementVectorSize = this->GetMeasurementVectorSize();

  if ( index == 0 )
    {
    MatrixType covarianceMatrix(measurementVectorSize, measurementVectorSize);
    covarianceMatrix.SetIdentity();
    MatrixDecoratedType::Pointer decoratedCovarianceMatrix = MatrixDecoratedType::New();
    decoratedCovarianceMatrix->Set(covarianceMatrix);
    return static_cast< DataObject * >( decoratedCovarianceMatrix.GetPointer() );
    }

  if ( index == 1 )
    {
    typedef typename MeasurementVectorTraitsTypes< MeasurementVectorType >::ValueType ValueType;
    MeasurementVectorType mean;
    (void)mean; // for complainty pants : valgrind
    MeasurementVectorTraits::SetLength( mean,  this->GetMeasurementVectorSize() );
    mean.Fill(NumericTraits< ValueType >::Zero);
    typename MeasurementVectorDecoratedType::Pointer decoratedMean = MeasurementVectorDecoratedType::New();
    decoratedMean->Set(mean);
    return static_cast< DataObject * >( decoratedMean.GetPointer() );
    }
  itkExceptionMacro("Trying to create output of index " << index << " larger than the number of output");
}

template< class TSample >
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
  MeasurementVectorSizeType measurementVectorSize = MeasurementVectorTraits::GetLength(vector);

  if ( measurementVectorSize )
    {
    return measurementVectorSize;
    }

  measurementVectorSize = 1; // Otherwise set it to an innocuous value

  return measurementVectorSize;
}

template< class TSample >
inline void
CovarianceSampleFilter< TSample >
::GenerateData()
{
  const SampleType *input = this->GetInput();

  MeasurementVectorSizeType measurementVectorSize = input->GetMeasurementVectorSize();

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

  double frequency;
  double totalFrequency = 0.0;

  typename TSample::ConstIterator iter = input->Begin();
  typename TSample::ConstIterator end = input->End();

  MeasurementVectorType diff;
  MeasurementVectorType measurements;

  MeasurementVectorTraits::SetLength(diff, measurementVectorSize);
  MeasurementVectorTraits::SetLength(measurements, measurementVectorSize);

  //Compute the mean first
  while ( iter != end )
    {
    frequency = iter.GetFrequency();
    totalFrequency += frequency;
    measurements = iter.GetMeasurementVector();

    for ( unsigned int i = 0; i < measurementVectorSize; ++i )
      {
      mean[i] += frequency * measurements[i];
      }
    ++iter;
    }

  for ( unsigned int i = 0; i < measurementVectorSize; ++i )
    {
    mean[i] = mean[i] / totalFrequency;
    }

  decoratedMeanOutput->Set(mean);

  //reset the total frequency and iterator
  iter = input->Begin();
  // fills the lower triangle and the diagonal cells in the covariance matrix
  while ( iter != end )
    {
    frequency = iter.GetFrequency();
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
        output(row, col) += frequency * diff[row] * diff[col];
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

  output /= ( totalFrequency - 1.0 );

  decoratedOutput->Set(output);
}

template< class TSample >
const typename CovarianceSampleFilter< TSample >::MatrixDecoratedType *
CovarianceSampleFilter< TSample >
::GetCovarianceMatrixOutput() const
{
  return static_cast< const MatrixDecoratedType * >( this->ProcessObject::GetOutput(0) );
}

template< class TSample >
const typename CovarianceSampleFilter< TSample >::MatrixType
CovarianceSampleFilter< TSample >
::GetCovarianceMatrix() const
{
  return this->GetCovarianceMatrixOutput()->Get();
}

template< class TSample >
const typename CovarianceSampleFilter< TSample >::MeasurementVectorDecoratedType *
CovarianceSampleFilter< TSample >
::GetMeanOutput() const
{
  return static_cast< const MeasurementVectorDecoratedType * >( this->ProcessObject::GetOutput(1) );
}

template< class TSample >
const typename CovarianceSampleFilter< TSample >::MeasurementVectorType
CovarianceSampleFilter< TSample >
::GetMean() const
{
  return this->GetMeanOutput()->Get();
}
} // end of namespace Statistics
} // end of namespace itk

#endif

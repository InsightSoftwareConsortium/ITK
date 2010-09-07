/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedMeanSampleFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWeightedMeanSampleFilter_txx
#define __itkWeightedMeanSampleFilter_txx

#include "itkMeasurementVectorTraits.h"
#include "itkNumericTraits.h"

namespace itk
{
namespace Statistics
{
template< class TSample >
WeightedMeanSampleFilter< TSample >
::WeightedMeanSampleFilter()
{
  this->ProcessObject::SetNthInput(1, NULL);
}

template< class TSample >
WeightedMeanSampleFilter< TSample >
::~WeightedMeanSampleFilter()
{}

template< class TSample >
void
WeightedMeanSampleFilter< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  // m_Weights
  os << indent << "Weights: " << this->GetWeightsInput() << std::endl;
  // m_WeightingFunction
  os << indent << "Weighting Function: " << this->GetWeightingFunctionInput() << std::endl;
}

template< class TSample >
void
WeightedMeanSampleFilter< TSample >
::GenerateData()
{
  // if weighting function is specifed, use it to compute the mean
  const InputWeightingFunctionObjectType *functionObject =
    this->GetWeightingFunctionInput();

  if ( functionObject != NULL )
    {
    this->ComputeMeanWithWeightingFunction();
    return;
    }

  // if weight array is specified use it to compute the mean
  const InputWeightArrayObjectType *weightArrayObject =
    this->GetWeightsInput();

  if ( weightArrayObject != NULL )
    {
    this->ComputeMeanWithWeights();
    return;
    }

  // Otherwise compute the regular mean ( without weight coefficients)
  Superclass::GenerateData();
  return;
}

template< class TSample >
void
WeightedMeanSampleFilter< TSample >
::ComputeMeanWithWeights()
{
  const SampleType *input = this->GetInput();

  MeasurementVectorSizeType measurementVectorSize =
    input->GetMeasurementVectorSize();

  MeasurementVectorDecoratedType *decoratedOutput =
    static_cast< MeasurementVectorDecoratedType * >(
      this->ProcessObject::GetOutput(0) );

  MeasurementVectorType output = decoratedOutput->Get();

  //reset the output
  for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
    {
    output[dim] = NumericTraits< MeasurementType >::Zero;
    }

  typename TSample::ConstIterator iter = input->Begin();
  typename TSample::ConstIterator end =  input->End();
  double totalWeight = 0.0;
  double weight;

  typename TSample::MeasurementVectorType measurements;

  const InputWeightArrayObjectType *weightArrayObject = this->GetWeightsInput();
  const WeightArrayType             weightArray = weightArrayObject->Get();

  int measurementVectorIndex = 0;

  while ( iter != end )
    {
    measurements = iter.GetMeasurementVector();
    weight = iter.GetFrequency() * ( weightArray )[measurementVectorIndex];
    totalWeight += weight;

    for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
      {
      output[dim] += measurements[dim] * weight;
      }
    ++measurementVectorIndex;
    ++iter;
    }

  if ( totalWeight != 0.0 )
    {
    for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
      {
      output[dim] /= totalWeight;
      }
    }

  decoratedOutput->Set(output);
}

template< class TSample >
void
WeightedMeanSampleFilter< TSample >
::ComputeMeanWithWeightingFunction()
{
  const SampleType *input = this->GetInput();

  MeasurementVectorSizeType measurementVectorSize =
    input->GetMeasurementVectorSize();

  MeasurementVectorDecoratedType *decoratedOutput =
    static_cast< MeasurementVectorDecoratedType * >(
      this->ProcessObject::GetOutput(0) );

  MeasurementVectorType output = decoratedOutput->Get();

  //reset the output
  for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
    {
    output[dim] = NumericTraits< MeasurementType >::Zero;
    }

  typename TSample::ConstIterator iter = input->Begin();
  typename TSample::ConstIterator end =  input->End();
  double totalWeight = 0.0;
  double weight;

  typename TSample::MeasurementVectorType measurements;

  // if weighting function is specifed, use it to compute the mean
  const InputWeightingFunctionObjectType *functionObject =
    this->GetWeightingFunctionInput();

  const WeightingFunctionType *weightFunction = functionObject->Get();

  while ( iter != end )
    {
    measurements = iter.GetMeasurementVector();
    weight =
      iter.GetFrequency() * weightFunction->Evaluate(measurements);
    totalWeight += weight;
    for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
      {
      output[dim] += measurements[dim] * weight;
      }
    ++iter;
    }

  if ( totalWeight != 0.0 )
    {
    for ( unsigned int dim = 0; dim < measurementVectorSize; dim++ )
      {
      output[dim] /= totalWeight;
      }
    }

  decoratedOutput->Set(output);
}
} // end of namespace Statistics
} // end of namespace itk

#endif

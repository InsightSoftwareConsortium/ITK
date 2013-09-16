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
#ifndef __itkWeightedMeanSampleFilter_h
#define __itkWeightedMeanSampleFilter_h

#include "itkMeanSampleFilter.h"
#include "itkFunctionBase.h"
#include "itkDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/** \class WeightedMeanSampleFilter
 * \brief Given a sample where each measurement vector has
 * associated weight value, this filter computes the sample mean
 *
 * To run this algorithm, you have plug in the target sample data
 * using SetInput method and provides weight by an array or function.
 *. Then call the Update method to run the alogithm.
 *
 * \sa MeanSampleFilter
 *
 * \ingroup ITKStatistics
 */
template< typename TSample >
class WeightedMeanSampleFilter : public MeanSampleFilter< TSample >
{
public:
  /**Standard class typedefs. */
  typedef WeightedMeanSampleFilter    Self;
  typedef MeanSampleFilter< TSample > Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /**Standard Macros */
  itkTypeMacro(WeightedMeanSampleFilter, MeanSampleFilter);
  itkNewMacro(Self);

  /** Traits derived from the base class */
  typedef typename Superclass::SampleType                     SampleType;
  typedef typename Superclass::MeasurementType                MeasurementType;
  typedef typename Superclass::MeasurementVectorType          MeasurementVectorType;
  typedef typename Superclass::MeasurementVectorSizeType      MeasurementVectorSizeType;
  typedef typename Superclass::MeasurementVectorDecoratedType MeasurementVectorDecoratedType;
  typedef typename Superclass::OutputType                     OutputType;
  typedef typename Superclass::MeasurementRealType            MeasurementRealType;
  typedef typename Superclass::MeasurementVectorRealType      MeasurementVectorRealType;

  /** Array typedef for weights */
  typedef Array< double > WeightArrayType;

  /** Type of DataObjects to use for the weight array type */
  typedef SimpleDataObjectDecorator< WeightArrayType > InputWeightArrayObjectType;

  /** Method to set the input value of the weight array */
  itkSetGetDecoratedInputMacro(Weights, WeightArrayType);

  /** Weight calculation function typedef */
  typedef FunctionBase< MeasurementVectorType, double > WeightingFunctionType;

  /** Type of DataObjects to use for Weight function */
  typedef DataObjectDecorator< WeightingFunctionType > InputWeightingFunctionObjectType;

  /** Method to set the weighting function */
  itkSetGetDecoratedObjectInputMacro(WeightingFunction, WeightingFunctionType);

protected:
  WeightedMeanSampleFilter();
  virtual ~WeightedMeanSampleFilter();
  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateData();

  // compute mean with weight array
  void ComputeMeanWithWeights();

  // compute mean using a weighting function
  void ComputeMeanWithWeightingFunction();

private:
  WeightedMeanSampleFilter(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};                                        // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightedMeanSampleFilter.hxx"
#endif

#endif

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
#ifndef itkWeightedCovarianceSampleFilter_h
#define itkWeightedCovarianceSampleFilter_h

#include "itkFunctionBase.h"
#include "itkCovarianceSampleFilter.h"
#include "itkDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/** \class WeightedCovarianceSampleFilter
 * \brief Calculates the covariance matrix of the target sample data.
 *  where each measurement vector has an associated weight value
 *
 * Weight values can be specified in two ways: using a weighting function
 * or an array containing weight values. If none of these two is specified,
 * the covariance matrix is generated with equal weights.
 *
 * \sa CovarianceSampleFilter
 *
 * \ingroup ITKStatistics
 */

template< typename TSample >
class ITK_TEMPLATE_EXPORT WeightedCovarianceSampleFilter:
  public CovarianceSampleFilter< TSample >
{
public:
  /** Standard class typedefs. */
  typedef WeightedCovarianceSampleFilter    Self;
  typedef CovarianceSampleFilter< TSample > Superclass;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;

  /** Standard Macros */
  itkTypeMacro(WeightedCovarianceSampleFilter, CovarianceSampleFilter);
  itkNewMacro(Self);

  /** Types derived from the base class */
  typedef typename Superclass::SampleType                     SampleType;
  typedef typename Superclass::MeasurementVectorType          MeasurementVectorType;
  typedef typename Superclass::MeasurementVectorSizeType      MeasurementVectorSizeType;
  typedef typename Superclass::MeasurementType                MeasurementType;

  /** Types derived from the base class */
  typedef typename Superclass::MeasurementVectorRealType      MeasurementVectorRealType;
  typedef typename Superclass::MeasurementRealType            MeasurementRealType;


  /** Type of weight values */
  typedef double WeightValueType;


  /** Array type for weights */
  typedef Array< WeightValueType > WeightArrayType;

  /** Type of DataObjects to use for the weight array type */
  typedef SimpleDataObjectDecorator< WeightArrayType > InputWeightArrayObjectType;

  /** Method to set the input value of the weight array */
  itkSetGetDecoratedInputMacro(Weights, WeightArrayType);


  /** Weight calculation function type */
  typedef FunctionBase< MeasurementVectorType, WeightValueType > WeightingFunctionType;

  /** Type of DataObjects to use for Weight function */
  typedef DataObjectDecorator< WeightingFunctionType > InputWeightingFunctionObjectType;

  /** Method to set/get the weighting function */
  itkSetGetDecoratedObjectInputMacro(WeightingFunction, WeightingFunctionType);


  /** Types derived from the base class */
  typedef typename Superclass::MatrixType          MatrixType;
  typedef typename Superclass::MatrixDecoratedType MatrixDecoratedType;

  /** Types derived from the base class */
  typedef typename Superclass::MeasurementVectorDecoratedType MeasurementVectorDecoratedType;
  typedef typename Superclass::OutputType                     OutputType;

protected:
  WeightedCovarianceSampleFilter();
  virtual ~WeightedCovarianceSampleFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

  /** Compute covariance matrix with weights computed from a function */
  void ComputeCovarianceMatrixWithWeightingFunction();

  /** Compute covariance matrix with weights specified in an array */
  void ComputeCovarianceMatrixWithWeights();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WeightedCovarianceSampleFilter);

};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightedCovarianceSampleFilter.hxx"
#endif

#endif

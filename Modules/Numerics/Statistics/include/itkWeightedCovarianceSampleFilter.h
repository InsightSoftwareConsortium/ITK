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
#ifndef __itkWeightedCovarianceSampleFilter_h
#define __itkWeightedCovarianceSampleFilter_h

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
 * \ingroup ITK-Statistics
 */

template< class TSample >
class ITK_EXPORT WeightedCovarianceSampleFilter:
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

  /** Traits derived from the base class */
  itkSuperclassTraitMacro(SampleType)
  itkSuperclassTraitMacro(MeasurementVectorType)
  itkSuperclassTraitMacro(MeasurementVectorSizeType)
  itkSuperclassTraitMacro(MeasurementVectorDecoratedType)
  itkSuperclassTraitMacro(OutputType)
  itkSuperclassTraitMacro(MeasurementRealType)
  itkSuperclassTraitMacro(MeasurementVectorRealType)

  /** Typedef for WeightedCovariance output */
  typedef VariableSizeMatrix< double > MatrixType;

  /** Weight calculation function typedef */
  typedef FunctionBase< MeasurementVectorType, double > WeightingFunctionType;

  /** VariableSizeMatrix is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  typedef  SimpleDataObjectDecorator< MatrixType > MatrixDecoratedType;

  /** Array typedef for weights */
  typedef Array< double > WeightArrayType;

  /** Type of DataObjects to use for the weight array type */
  typedef SimpleDataObjectDecorator< WeightArrayType > InputWeightArrayObjectType;

  /** Method to set the input value of the weight array */
  itkSetDecoratedInputMacro(Weights, WeightArrayType, 1);

  /** Type of DataObjects to use for Weight function */
  typedef DataObjectDecorator< WeightingFunctionType > InputWeightingFunctionObjectType;

  /** Method to set the weighting function */
  itkSetDecoratedObjectInputMacro(WeightingFunction, WeightingFunctionType, 2);

protected:
  WeightedCovarianceSampleFilter();
  virtual ~WeightedCovarianceSampleFilter();
  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateData();

  /** Compute covariance matrix with weights computed from a function */
  void ComputeCovarianceMatrixWithWeightingFunction();

  /** Compute covariance matrix with weights specified in an array */
  void ComputeCovarianceMatrixWithWeights();

private:
  WeightedCovarianceSampleFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented

};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightedCovarianceSampleFilter.txx"
#endif

#endif

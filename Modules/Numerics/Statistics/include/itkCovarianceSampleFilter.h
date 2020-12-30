/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkCovarianceSampleFilter_h
#define itkCovarianceSampleFilter_h

#include "itkProcessObject.h"

#include "itkVariableSizeMatrix.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/**
 *\class CovarianceSampleFilter
 * \brief Calculates the covariance matrix of the target sample data.
 *
 * The filter calculates first the sample mean and use it in the covariance
 * calculation. The covariance is computed as follows
 * Let \f$\Sigma\f$ denotes covariance matrix for the sample, then:
 * When \f$x_{i}\f$ is \f$i\f$th component of a measurement vector
 * \f$\vec x\f$, \f$\mu_{i}\f$ is the \f$i\f$th component of the \f$\vec\mu\f$,
 * and the \f$\sigma_{ij}\f$ is the \f$ij\f$th component \f$\Sigma\f$,
 * \f$\sigma_{ij} = (x_{i} - \mu_{i})(x_{j} - \mu_{j})\f$
 *
 * This estimator is an unbiased one, because it divisor in the covariance
 * computation takes into account that one degree of freedom has been taken
 * for computing the mean.
 *
 * Without the plugged in mean vector, this calculator will perform
 * the single pass mean and covariance calculation algorithm.
 *
 * \ingroup ITKStatistics
 */

template <typename TSample>
class ITK_TEMPLATE_EXPORT CovarianceSampleFilter : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CovarianceSampleFilter);

  /** Standard class type aliases. */
  using Self = CovarianceSampleFilter;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using SampleType = TSample;

  /** Standard Macros */
  itkTypeMacro(CovarianceSampleFilter, ProcessObject);
  itkNewMacro(Self);

  /** Type of each measurement vector in sample */
  using MeasurementVectorType = typename SampleType::MeasurementVectorType;

  /** Type of the length of each measurement vector */
  using MeasurementVectorSizeType = typename SampleType::MeasurementVectorSizeType;

  /** Type of measurement vector component value */
  using MeasurementType = typename SampleType::MeasurementType;

  /** Type of a measurement vector, holding floating point values */
  using MeasurementVectorRealType = typename NumericTraits<MeasurementVectorType>::RealType;

  /** Type of a floating point measurement component value */
  using MeasurementRealType = typename NumericTraits<MeasurementType>::RealType;


  /** Method to set the sample */
  using Superclass::SetInput;
  void
  SetInput(const SampleType * sample);

  /** Method to get the sample */
  const SampleType *
  GetInput() const;


  /** Type of covariance matrix output */
  using MatrixType = VariableSizeMatrix<MeasurementRealType>;

  /** Return the covariance matrix */
  const MatrixType
  GetCovarianceMatrix() const;

  /** VariableSizeMatrix is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  using MatrixDecoratedType = SimpleDataObjectDecorator<MatrixType>;
  const MatrixDecoratedType *
  GetCovarianceMatrixOutput() const;


  /** Return the mean vector */
  const MeasurementVectorRealType
  GetMean() const;

  /** MeasurementVector is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  using MeasurementVectorDecoratedType = SimpleDataObjectDecorator<MeasurementVectorRealType>;
  const MeasurementVectorDecoratedType *
  GetMeanOutput() const;
  using OutputType = MeasurementVectorDecoratedType;


  MeasurementVectorSizeType
  GetMeasurementVectorSize() const;

protected:
  CovarianceSampleFilter();
  ~CovarianceSampleFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** DataObject pointer */
  using DataObjectPointer = DataObject::Pointer;

  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType index) override;

  void
  GenerateData() override;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCovarianceSampleFilter.hxx"
#endif

#endif

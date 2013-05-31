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
#ifndef __itkCovarianceSampleFilter_h
#define __itkCovarianceSampleFilter_h

#include "itkProcessObject.h"

#include "itkVariableSizeMatrix.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/** \class CovarianceSampleFilter
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

template< class TSample >
class CovarianceSampleFilter:
  public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef CovarianceSampleFilter     Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef TSample                    SampleType;

  /** Standard Macros */
  itkTypeMacro(CovarianceSampleFilter, ProcessObject);
  itkNewMacro(Self);

  /** Length of a measurement vector */
  typedef typename TSample::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** Measurement vector type */
  typedef typename TSample::MeasurementVectorType MeasurementVectorType;

  /** Type of vector elements */
  typedef typename TSample::MeasurementType                           MeasurementType;
  typedef typename NumericTraits< MeasurementType >::RealType         MeasurementRealType;

  /** Type of the measurement vector type */
  typedef typename NumericTraits< MeasurementVectorType >::RealType   MeasurementVectorRealType;

  /** Typedef for Covariance output */
  typedef VariableSizeMatrix< double > MatrixType;

  /** Method to set/get the sample */
  using Superclass::SetInput;
  void SetInput(const SampleType *sample);

  const SampleType *  GetInput() const;

  /** VariableSizeMatrix is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  typedef  SimpleDataObjectDecorator< MatrixType > MatrixDecoratedType;

  /** MeasurementVector is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  typedef  SimpleDataObjectDecorator< MeasurementVectorRealType > MeasurementVectorDecoratedType;

  typedef MeasurementVectorDecoratedType OutputType;

  /** Return the covariance matrix */
  const MatrixType GetCovarianceMatrix() const;

  const MatrixDecoratedType * GetCovarianceMatrixOutput() const;

  /** Return the mean vector */
  const MeasurementVectorRealType GetMean() const;

  const MeasurementVectorDecoratedType * GetMeanOutput() const;

  MeasurementVectorSizeType GetMeasurementVectorSize() const;

protected:
  CovarianceSampleFilter();
  virtual ~CovarianceSampleFilter();
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** DataObject pointer */
  typedef DataObject::Pointer DataObjectPointer;

  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx);

  void GenerateData();

private:
  CovarianceSampleFilter(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented

};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCovarianceSampleFilter.hxx"
#endif

#endif

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
#ifndef itkStandardDeviationPerComponentSampleFilter_h
#define itkStandardDeviationPerComponentSampleFilter_h

#include "itkProcessObject.h"

#include "itkVariableSizeMatrix.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkNumericTraitsFixedArrayPixel.h"

namespace itk
{
namespace Statistics
{
/** \class StandardDeviationPerComponentSampleFilter
 * \brief Calculates the covariance matrix of the target sample data.
 *
 * The filter calculates first the sample mean and use it in the covariance
 * calculation. The covariance is computed as follows
 * Let \f$\Sigma\f$ denotes covariance matrix for the sample, then:
 * When \f$x_{i}\f$ is \f$i\f$th component of a measurement vector
 * \f$\vec x\f$, \f$\mu_{i}\f$ is the \f$i\f$th componet of the \f$\vec\mu\f$,
 * and the \f$\sigma_{ij}\f$ is the \f$ij\f$th componet \f$\Sigma\f$,
 * \f$\sigma_{ij} = (x_{i} - \mu_{i})(x_{j} - \mu_{j})\f$
 *
 * Without the plugged in mean vector, this calculator will perform
 * the single pass mean and covariance calculation algorithm.
 *
 * \ingroup ITKStatistics
 */

template< typename TSample >
class ITK_TEMPLATE_EXPORT StandardDeviationPerComponentSampleFilter:
  public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef StandardDeviationPerComponentSampleFilter Self;
  typedef ProcessObject                             Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;
  typedef TSample                                   SampleType;

  /** Standard Macros */
  itkTypeMacro(StandardDeviationPerComponentSampleFilter, ProcessObject);
  itkNewMacro(Self);

  /** Length of a measurement vector */
  typedef typename TSample::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** Measurement vector type */
  typedef typename TSample::MeasurementVectorType                   MeasurementVectorType;
  typedef typename NumericTraits< MeasurementVectorType >::RealType MeasurementVectorRealType;

  /** Method to set/get the sample */
  using Superclass::SetInput;
  void SetInput(const SampleType *sample);

  const SampleType *  GetInput() const;

  /** MeasurementVector is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  typedef  SimpleDataObjectDecorator< MeasurementVectorRealType > MeasurementVectorRealDecoratedType;

  typedef MeasurementVectorRealDecoratedType OutputType;

  /** Return the standard deviation vector */
  const MeasurementVectorRealType GetStandardDeviationPerComponent() const;

  const MeasurementVectorRealDecoratedType * GetStandardDeviationPerComponentOutput() const;

  /** Return the mean vector */
  const MeasurementVectorRealType GetMeanPerComponent() const;

  const MeasurementVectorRealDecoratedType * GetMeanPerComponentOutput() const;

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(StandardDeviationPerComponentSampleFilter);

  StandardDeviationPerComponentSampleFilter();
  virtual ~StandardDeviationPerComponentSampleFilter() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** DataObject pointer */
  typedef DataObject::Pointer DataObjectPointer;

  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

  MeasurementVectorSizeType GetMeasurementVectorSize() const;

private:
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStandardDeviationPerComponentSampleFilter.hxx"
#endif

#endif

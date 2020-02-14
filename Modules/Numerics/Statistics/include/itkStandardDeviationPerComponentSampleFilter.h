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
/**
 *\class StandardDeviationPerComponentSampleFilter
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
 * Without the plugged in mean vector, this calculator will perform
 * the single pass mean and covariance calculation algorithm.
 *
 * \ingroup ITKStatistics
 */

template <typename TSample>
class ITK_TEMPLATE_EXPORT StandardDeviationPerComponentSampleFilter : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(StandardDeviationPerComponentSampleFilter);

  /** Standard class type aliases. */
  using Self = StandardDeviationPerComponentSampleFilter;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using SampleType = TSample;

  /** Standard Macros */
  itkTypeMacro(StandardDeviationPerComponentSampleFilter, ProcessObject);
  itkNewMacro(Self);

  /** Length of a measurement vector */
  using MeasurementVectorSizeType = typename TSample::MeasurementVectorSizeType;

  /** Measurement vector type */
  using MeasurementVectorType = typename TSample::MeasurementVectorType;
  using MeasurementVectorRealType = typename NumericTraits<MeasurementVectorType>::RealType;

  /** Method to set/get the sample */
  using Superclass::SetInput;
  void
  SetInput(const SampleType * sample);

  const SampleType *
  GetInput() const;

  /** MeasurementVector is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  using MeasurementVectorRealDecoratedType = SimpleDataObjectDecorator<MeasurementVectorRealType>;

  using OutputType = MeasurementVectorRealDecoratedType;

  /** Return the standard deviation vector */
  const MeasurementVectorRealType
  GetStandardDeviationPerComponent() const;

  const MeasurementVectorRealDecoratedType *
  GetStandardDeviationPerComponentOutput() const;

  /** Return the mean vector */
  const MeasurementVectorRealType
  GetMeanPerComponent() const;

  const MeasurementVectorRealDecoratedType *
  GetMeanPerComponentOutput() const;

protected:
  StandardDeviationPerComponentSampleFilter();
  ~StandardDeviationPerComponentSampleFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** DataObject pointer */
  using DataObjectPointer = DataObject::Pointer;

  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

  void
  GenerateData() override;

  MeasurementVectorSizeType
  GetMeasurementVectorSize() const;

private:
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStandardDeviationPerComponentSampleFilter.hxx"
#endif

#endif

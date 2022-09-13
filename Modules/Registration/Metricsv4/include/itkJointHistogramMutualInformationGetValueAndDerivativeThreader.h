/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkJointHistogramMutualInformationGetValueAndDerivativeThreader_h
#define itkJointHistogramMutualInformationGetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"

#include <memory> // For unique_ptr.

namespace itk
{

/** \class JointHistogramMutualInformationGetValueAndDerivativeThreader
 * \brief Processes points for
 * JointHistogramMutualInformationImageToImageMetricv4 \c
 * GetValueAndDerivative().
 *
 * \ingroup ITKMetricsv4
 */
template <typename TDomainPartitioner, typename TImageToImageMetric, typename TJointHistogramMetric>
class ITK_TEMPLATE_EXPORT JointHistogramMutualInformationGetValueAndDerivativeThreader
  : public ImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(JointHistogramMutualInformationGetValueAndDerivativeThreader);

  /** Standard class type aliases. */
  using Self = JointHistogramMutualInformationGetValueAndDerivativeThreader;
  using Superclass = ImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(JointHistogramMutualInformationGetValueAndDerivativeThreader,
               ImageToImageMetricv4GetValueAndDerivativeThreader);

  itkNewMacro(Self);

  using typename Superclass::DomainType;
  using typename Superclass::AssociateType;

  using typename Superclass::VirtualPointType;
  using typename Superclass::VirtualIndexType;
  using typename Superclass::FixedImagePointType;
  using typename Superclass::FixedImagePixelType;
  using typename Superclass::FixedImageGradientType;
  using typename Superclass::MovingImagePointType;
  using typename Superclass::MovingImagePixelType;
  using typename Superclass::MovingImageGradientType;
  using typename Superclass::MeasureType;
  using typename Superclass::DerivativeType;
  using typename Superclass::DerivativeValueType;
  using typename Superclass::JacobianType;

  using JointHistogramMetricType = TJointHistogramMetric;
  using InternalComputationValueType = typename JointHistogramMetricType::InternalComputationValueType;
  using JointPDFInterpolatorType = typename JointHistogramMetricType::JointPDFInterpolatorType;
  using MarginalPDFInterpolatorType = typename JointHistogramMetricType::MarginalPDFInterpolatorType;
  using JointPDFInterpolatorPointer = typename JointHistogramMetricType::JointPDFInterpolatorPointer;
  using MarginalPDFInterpolatorPointer = typename JointHistogramMetricType::MarginalPDFInterpolatorPointer;
  using NumberOfParametersType = typename JointHistogramMetricType::NumberOfParametersType;
  using JointPDFType = typename JointHistogramMetricType::JointPDFType;
  using MarginalPDFType = typename JointHistogramMetricType::MarginalPDFType;
  using MarginalPDFPointType = typename MarginalPDFType::PointType;
  using JointPDFPointType = typename JointPDFType::PointType;
  using JointPDFValueType = typename JointHistogramMetricType::JointPDFValueType;

protected:
  JointHistogramMutualInformationGetValueAndDerivativeThreader();
  ~JointHistogramMutualInformationGetValueAndDerivativeThreader() override = default;

  using JointHistogramType = Image<SizeValueType, 2>;

  void
  BeforeThreadedExecution() override;

  void
  AfterThreadedExecution() override;

  bool
  ProcessPoint(const VirtualIndexType &        virtualIndex,
               const VirtualPointType &        virtualPoint,
               const FixedImagePointType &     mappedFixedPoint,
               const FixedImagePixelType &     fixedImageValue,
               const FixedImageGradientType &  mappedFixedImageGradient,
               const MovingImagePointType &    mappedMovingPoint,
               const MovingImagePixelType &    movingImageValue,
               const MovingImageGradientType & movingImageGradient,
               MeasureType &                   metricValueReturn,
               DerivativeType &                localDerivativeReturn,
               const ThreadIdType              threadId) const override;

  inline InternalComputationValueType
  ComputeFixedImageMarginalPDFDerivative(const MarginalPDFPointType & margPDFpoint, const ThreadIdType threadId) const;

  inline InternalComputationValueType
  ComputeMovingImageMarginalPDFDerivative(const MarginalPDFPointType & margPDFpoint, const ThreadIdType threadId) const;

  inline InternalComputationValueType
  ComputeJointPDFDerivative(const JointPDFPointType & jointPDFpoint,
                            const ThreadIdType        threadId,
                            const SizeValueType       ind) const;
  struct JointHistogramMIPerThreadStruct
  {
    JointPDFInterpolatorPointer    JointPDFInterpolator;
    MarginalPDFInterpolatorPointer FixedImageMarginalPDFInterpolator;
    MarginalPDFInterpolatorPointer MovingImageMarginalPDFInterpolator;
  };
  itkPadStruct(ITK_CACHE_LINE_ALIGNMENT, JointHistogramMIPerThreadStruct, PaddedJointHistogramMIPerThreadStruct);
  itkAlignedTypedef(ITK_CACHE_LINE_ALIGNMENT,
                    PaddedJointHistogramMIPerThreadStruct,
                    AlignedJointHistogramMIPerThreadStruct);
  std::unique_ptr<AlignedJointHistogramMIPerThreadStruct[]> m_JointHistogramMIPerThreadVariables;

private:
  /** Internal pointer to the metric object in use by this threader.
   *  This will avoid costly dynamic casting in tight loops. */
  TJointHistogramMetric * m_JointAssociate;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkJointHistogramMutualInformationGetValueAndDerivativeThreader.hxx"
#endif

#endif

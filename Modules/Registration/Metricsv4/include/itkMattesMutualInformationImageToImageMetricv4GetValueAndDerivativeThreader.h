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
#ifndef itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader_h
#define itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"

#include <mutex>

namespace itk
{

/** \class MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Processes points for MattesMutualInformationImageToImageMetricv4 \c
 * GetValueAndDerivative.
 *
 * \ingroup ITKMetricsv4
 */
template <typename TDomainPartitioner, typename TImageToImageMetric, typename TMattesMutualInformationMetric>
class ITK_TEMPLATE_EXPORT MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader
  : public ImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader);

  /** Standard class type aliases. */
  using Self = MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader;
  using Superclass = ImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader,
               ImageToImageMetricv4GetValueAndDerivativeThreader);

  itkNewMacro(Self);

  using typename Superclass::DomainType;
  using typename Superclass::AssociateType;

  using ImageToImageMetricv4Type = typename Superclass::ImageToImageMetricv4Type;
  using typename Superclass::VirtualPointType;
  using typename Superclass::VirtualIndexType;
  using typename Superclass::FixedImagePointType;
  using typename Superclass::FixedImageIndexType;
  using typename Superclass::FixedImagePixelType;
  using typename Superclass::FixedImageGradientType;
  using typename Superclass::MovingImagePointType;
  using typename Superclass::MovingImagePixelType;
  using typename Superclass::MovingImageGradientType;
  using typename Superclass::MeasureType;
  using typename Superclass::DerivativeType;
  using typename Superclass::DerivativeValueType;
  using typename Superclass::NumberOfParametersType;

  using MovingTransformType = typename ImageToImageMetricv4Type::MovingTransformType;

  using PDFValueType = typename TMattesMutualInformationMetric::PDFValueType;
  using JointPDFType = typename TMattesMutualInformationMetric::JointPDFType;
  using JointPDFRegionType = typename TMattesMutualInformationMetric::JointPDFRegionType;
  using JointPDFIndexType = typename TMattesMutualInformationMetric::JointPDFIndexType;
  using JointPDFValueType = typename TMattesMutualInformationMetric::JointPDFValueType;
  using JointPDFSizeType = typename TMattesMutualInformationMetric::JointPDFSizeType;
  using JointPDFDerivativesType = typename TMattesMutualInformationMetric::JointPDFDerivativesType;
  using JointPDFDerivativesIndexType = typename TMattesMutualInformationMetric::JointPDFDerivativesIndexType;
  using JointPDFDerivativesValueType = typename TMattesMutualInformationMetric::JointPDFDerivativesValueType;
  using JointPDFDerivativesRegionType = typename TMattesMutualInformationMetric::JointPDFDerivativesRegionType;
  using JointPDFDerivativesSizeType = typename TMattesMutualInformationMetric::JointPDFDerivativesSizeType;

  using CubicBSplineFunctionType = typename TMattesMutualInformationMetric::CubicBSplineFunctionType;
  using CubicBSplineDerivativeFunctionType =
    typename TMattesMutualInformationMetric::CubicBSplineDerivativeFunctionType;

  using JacobianType = typename TMattesMutualInformationMetric::JacobianType;

protected:
  MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader()
    : m_MattesAssociate(nullptr)
  {}

  void
  BeforeThreadedExecution() override;

  void
  AfterThreadedExecution() override;

  /** This function computes the local voxel-wise contribution of
   *  the metric to the global integral of the metric/derivative.
   */
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

  /** Compute PDF derivative contribution for each parameter of a displacement field. */
  virtual void
  ComputePDFDerivativesLocalSupportTransform(const JacobianType &            jacobian,
                                             const MovingImageGradientType & movingImageGradient,
                                             const PDFValueType &            cubicBSplineDerivativeValue,
                                             DerivativeValueType *           localSupportDerivativeResultPtr) const;

private:
  /** Internal pointer to the Mattes metric object in use by this threader.
   *  This will avoid costly dynamic casting in tight loops. */
  TMattesMutualInformationMetric * m_MattesAssociate;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader.hxx"
#endif

#endif

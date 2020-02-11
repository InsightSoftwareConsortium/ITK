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
#ifndef itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader_h
#define itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{

/** \class CorrelationImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Processes points for CorrelationImageToImageMetricv4 \c
 * GetValueAndDerivative.
 *
 * \ingroup ITKMetricsv4
 */
template <typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
class ITK_TEMPLATE_EXPORT CorrelationImageToImageMetricv4GetValueAndDerivativeThreader
  : public ImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CorrelationImageToImageMetricv4GetValueAndDerivativeThreader);

  /** Standard class type aliases. */
  using Self = CorrelationImageToImageMetricv4GetValueAndDerivativeThreader;
  using Superclass = ImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(CorrelationImageToImageMetricv4GetValueAndDerivativeThreader,
               ImageToImageMetricv4GetValueAndDerivativeThreader);

  itkNewMacro(Self);

  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;

  using ImageToImageMetricv4Type = typename Superclass::ImageToImageMetricv4Type;
  using VirtualIndexType = typename Superclass::VirtualIndexType;
  using VirtualPointType = typename Superclass::VirtualPointType;
  using FixedImagePointType = typename Superclass::FixedImagePointType;
  using FixedImagePixelType = typename Superclass::FixedImagePixelType;
  using FixedImageGradientType = typename Superclass::FixedImageGradientType;
  using MovingImagePointType = typename Superclass::MovingImagePointType;
  using MovingImagePixelType = typename Superclass::MovingImagePixelType;
  using MovingImageGradientType = typename Superclass::MovingImageGradientType;
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using DerivativeValueType = typename Superclass::DerivativeValueType;

  using FixedTransformType = typename ImageToImageMetricv4Type::FixedTransformType;
  using FixedOutputPointType = typename FixedTransformType::OutputPointType;
  using MovingTransformType = typename ImageToImageMetricv4Type::MovingTransformType;
  using MovingOutputPointType = typename MovingTransformType::OutputPointType;

  using InternalComputationValueType = typename Superclass::InternalComputationValueType;
  using NumberOfParametersType = typename Superclass::NumberOfParametersType;

protected:
  CorrelationImageToImageMetricv4GetValueAndDerivativeThreader();
  ~CorrelationImageToImageMetricv4GetValueAndDerivativeThreader() override;

  /** Overload: Resize and initialize per thread objects:
   *    number of valid points
   *    moving transform jacobian
   *    cross-correlation specific variables
   *  */
  void
  BeforeThreadedExecution() override;

  /** Overload:
   * Collects the results from each thread and sums them.  Results are stored
   * in the enclosing class \c m_Value and \c m_DerivativeResult.  Behavior
   * depends on m_AverageValueAndDerivativeByNumberOfValuePoints,
   * m_NumberOfValidPoints, to average the value sum, and to average
   * derivative sums for global transforms only (i.e. transforms without local
   * support).  */
  void
  AfterThreadedExecution() override;

  /** Overload to avoid execution of adding entries to m_MeasurePerThread
   * StorePointDerivativeResult() after this function calls ProcessPoint().
   * Method called by the threaders to process the given virtual point.  This
   * in turn calls \c TransformAndEvaluateFixedPoint, \c
   * TransformAndEvaluateMovingPoint, and \c ProcessPoint. */
  bool
  ProcessVirtualPoint(const VirtualIndexType & virtualIndex,
                      const VirtualPointType & virtualPoint,
                      const ThreadIdType       threadId) override;

  /** This function computes the local voxel-wise contribution of
   *  the metric to the global integral of the metric/derivative.
   */
  bool
  ProcessPoint(const VirtualIndexType &        virtualIndex,
               const VirtualPointType &        virtualPoint,
               const FixedImagePointType &     mappedFixedPoint,
               const FixedImagePixelType &     mappedFixedPixelValue,
               const FixedImageGradientType &  mappedFixedImageGradient,
               const MovingImagePointType &    mappedMovingPoint,
               const MovingImagePixelType &    mappedMovingPixelValue,
               const MovingImageGradientType & mappedMovingImageGradient,
               MeasureType &                   metricValueReturn,
               DerivativeType &                localDerivativeReturn,
               const ThreadIdType              threadId) const override;

private:
  /*
   * the per-thread memory for computing the correlation and its derivatives
   * \bar f (CorrelationImageToImageMetricv4::m_AverageFix ) and
   * \bar m (CorrelationImageToImageMetricv4::m_AverageMov ), the average pixel
   * intensity, computed using the helper
   * class CorrelationHelperImageToImageMetricv4GetValueAndDerivativeThreader.
   * say f_i is the i-th pixel of fixed image, m_i is the i-th pixel of moving
   * image: see the comments below
   */
  struct CorrelationMetricValueDerivativePerThreadStruct
  {                                   // keep cumulative summation over points for:
    InternalComputationValueType fm;  // (f_i - \bar f) * (m_i - \bar m)
    InternalComputationValueType m2;  // (m_i - \bar m)^2
    InternalComputationValueType f2;  // (f_i - \bar m)^2
    InternalComputationValueType m;   // m_i
    InternalComputationValueType f;   // f_i
    DerivativeType               fdm; // (f_i - \bar f) * dm_i/dp
    DerivativeType               mdm; // (m_i - \bar m) * dm_i/dp
  };

  itkPadStruct(ITK_CACHE_LINE_ALIGNMENT,
               CorrelationMetricValueDerivativePerThreadStruct,
               PaddedCorrelationMetricValueDerivativePerThreadStruct);
  itkAlignedTypedef(ITK_CACHE_LINE_ALIGNMENT,
                    PaddedCorrelationMetricValueDerivativePerThreadStruct,
                    AlignedCorrelationMetricValueDerivativePerThreadStruct);
  /* per thread variables for correlation and its derivatives */
  mutable AlignedCorrelationMetricValueDerivativePerThreadStruct * m_CorrelationMetricValueDerivativePerThreadVariables;

  /** Internal pointer to the metric object in use by this threader.
   *  This will avoid costly dynamic casting in tight loops. */
  TCorrelationMetric * m_CorrelationAssociate;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader.hxx"
#endif

#endif

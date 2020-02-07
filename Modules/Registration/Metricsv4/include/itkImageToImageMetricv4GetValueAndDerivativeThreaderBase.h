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
#ifndef itkImageToImageMetricv4GetValueAndDerivativeThreaderBase_h
#define itkImageToImageMetricv4GetValueAndDerivativeThreaderBase_h

#include "itkDomainThreader.h"
#include "itkCompensatedSummation.h"

namespace itk
{

/** \class ImageToImageMetricv4GetValueAndDerivativeThreaderBase
 * \brief Provides threading for ImageToImageMetricv4::GetValueAndDerivative.
 *
 *  \tparam TDomainPartitioner type of the Domain,
 *  ThreadedImageRegionPartitioner or ThreadedIndexedContainerPartitioner
 *  \tparam TImageToImageMetricv4 type of the ImageToImageMetricv4
 *
 *  This class provides a \c BeforeThreadedExecution, and \c
 *  AfterThreadedExecution.
 *
 *  The \c ThreadedExecution in
 *  ImageToImageMetricv4GetValueAndDerivativeThreader calls \c
 *  ProcessVirtualPoint on every point in the virtual image domain.  \c
 *  ProcessVirtualPoint calls \c ProcessPoint on each point.
 *
 * \ingroup ITKMetricsv4 */
template <typename TDomainPartitioner, typename TImageToImageMetricv4>
class ITK_TEMPLATE_EXPORT ImageToImageMetricv4GetValueAndDerivativeThreaderBase
  : public DomainThreader<TDomainPartitioner, TImageToImageMetricv4>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToImageMetricv4GetValueAndDerivativeThreaderBase);

  /** Standard class type aliases. */
  using Self = ImageToImageMetricv4GetValueAndDerivativeThreaderBase;
  using Superclass = DomainThreader<TDomainPartitioner, TImageToImageMetricv4>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(ImageToImageMetricv4GetValueAndDerivativeThreaderBase, DomainThreader);

  /** Superclass types. */
  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;

  /** Types of the target class. */
  using ImageToImageMetricv4Type = TImageToImageMetricv4;
  using VirtualImageType = typename ImageToImageMetricv4Type::VirtualImageType;
  using VirtualIndexType = typename ImageToImageMetricv4Type::VirtualIndexType;
  using VirtualPointType = typename ImageToImageMetricv4Type::VirtualPointType;
  using FixedImagePointType = typename ImageToImageMetricv4Type::FixedImagePointType;
  using FixedImagePixelType = typename ImageToImageMetricv4Type::FixedImagePixelType;
  using FixedImageIndexType = typename ImageToImageMetricv4Type::FixedImageIndexType;
  using FixedImageGradientType = typename ImageToImageMetricv4Type::FixedImageGradientType;
  using MovingImagePointType = typename ImageToImageMetricv4Type::MovingImagePointType;
  using MovingImagePixelType = typename ImageToImageMetricv4Type::MovingImagePixelType;
  using MovingImageGradientType = typename ImageToImageMetricv4Type::MovingImageGradientType;

  using FixedTransformType = typename ImageToImageMetricv4Type::FixedTransformType;
  using FixedOutputPointType = typename FixedTransformType::OutputPointType;
  using MovingTransformType = typename ImageToImageMetricv4Type::MovingTransformType;
  using MovingOutputPointType = typename MovingTransformType::OutputPointType;

  using MeasureType = typename ImageToImageMetricv4Type::MeasureType;
  using DerivativeType = typename ImageToImageMetricv4Type::DerivativeType;
  using DerivativeValueType = typename ImageToImageMetricv4Type::DerivativeValueType;
  using JacobianType = typename ImageToImageMetricv4Type::JacobianType;
  using ImageDimensionType = typename ImageToImageMetricv4Type::ImageDimensionType;

  using InternalComputationValueType = typename ImageToImageMetricv4Type::InternalComputationValueType;
  using NumberOfParametersType = typename ImageToImageMetricv4Type::NumberOfParametersType;

  using CompensatedDerivativeValueType = CompensatedSummation<DerivativeValueType>;
  using CompensatedDerivativeType = std::vector<CompensatedDerivativeValueType>;

  /** Access the GetValueAndDerivative() accesor in image metric base. */
  virtual bool
  GetComputeDerivative() const;

protected:
  ImageToImageMetricv4GetValueAndDerivativeThreaderBase();
  ~ImageToImageMetricv4GetValueAndDerivativeThreaderBase() override;

  /** Resize and initialize per thread objects. */
  void
  BeforeThreadedExecution() override;

  /** Collects the results from each thread and sums them.  Results are stored
   * in the enclosing class \c m_Value and \c m_DerivativeResult.  Behavior
   * depends on m_AverageValueAndDerivativeByNumberOfValuePoints,
   * m_NumberOfValidPoints, to average the value sum, and to average
   * derivative sums for global transforms only (i.e. transforms without local
   * support).  */
  void
  AfterThreadedExecution() override;

  /** Method called by the threaders to process the given virtual point.  This
   * in turn calls \c TransformAndEvaluateFixedPoint, \c
   * TransformAndEvaluateMovingPoint, and \c ProcessPoint.
   * And adds entries to m_MeasurePerThread and m_LocalDerivativesPerThread,
   * m_NumberOfValidPointsPerThread. */
  virtual bool
  ProcessVirtualPoint(const VirtualIndexType & virtualIndex,
                      const VirtualPointType & virtualPoint,
                      const ThreadIdType       threadId);

  /** Method to calculate the metric value and derivative
   * given a point, value and image derivative for both fixed and moving
   * spaces. The provided values have been calculated from \c virtualPoint,
   * which is provided in case it's needed.
   * \param virtualIndex
   * \param virtualPoint is the point within the virtual domain from which
   * the passed parameters have been calculated.
   * \param mappedFixedPoint is a valid point within the moving image space
   *  that has passed bounds checking, and lies within any mask that may
   *  be assigned.
   * \param mappedFixedPixelValue holds the pixel value at the mapped fixed
   *  point.
   * \param mappedFixedImageGradient holds the image gradient at the fixed point,
   *  but only when \c m_GradientSource is set to calculate fixed image gradients
   *  (either when it's set to calculate only fixed gradients, or both fixed and
   *  moving). Otherwise, the value is meaningless and should be ignored.
   * \param mappedMovingPoint
   * \param mappedMovingPixelValue
   * \param mappedMovingImageGradient
   *  These three parameters hold the point, pixel value and image gradient for
   *  the moving image space, as described above for the fixed image space.
   * Results must be returned by derived classes in:
   *   \param metricValueReturn
   *   \param localDerivativeReturn
   * \param threadId may be used as needed, for example to access any per-thread
   * data cached during pre-processing by the derived class.
   * \warning  This is called from the threader, and thus must be thread-safe.
   */
  virtual bool
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
               const ThreadIdType              threadId) const = 0;


  /** Store derivative result from a single point calculation.
   * \warning If this method is overridden or otherwise not used
   * in a derived class, be sure to *accumulate* results. */
  virtual void
  StorePointDerivativeResult(const VirtualIndexType & virtualIndex, const ThreadIdType threadId);

  struct GetValueAndDerivativePerThreadStruct
  {
    /** Intermediary threaded metric value storage. */
    InternalComputationValueType Measure;
    /** Intermediary threaded metric value storage. */
    DerivativeType Derivatives;
    /** Intermediary threaded metric value storage. This is used only with global transforms. */
    CompensatedDerivativeType CompensatedDerivatives;
    /** Intermediary threaded metric value storage. */
    DerivativeType LocalDerivatives;
    /** Intermediary threaded metric value storage. */
    SizeValueType NumberOfValidPoints;
    /** Pre-allocated transform jacobian objects, for use as needed by derived
     * classes for efficiency. */
    JacobianType MovingTransformJacobian;
    JacobianType MovingTransformJacobianPositional;
  };
  itkPadStruct(ITK_CACHE_LINE_ALIGNMENT,
               GetValueAndDerivativePerThreadStruct,
               PaddedGetValueAndDerivativePerThreadStruct);
  itkAlignedTypedef(ITK_CACHE_LINE_ALIGNMENT,
                    PaddedGetValueAndDerivativePerThreadStruct,
                    AlignedGetValueAndDerivativePerThreadStruct);
  mutable AlignedGetValueAndDerivativePerThreadStruct * m_GetValueAndDerivativePerThreadVariables;

  /** Cached values to avoid call overhead.
   *  These will only be set once threading has been started. */
  mutable NumberOfParametersType m_CachedNumberOfParameters;
  mutable NumberOfParametersType m_CachedNumberOfLocalParameters;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToImageMetricv4GetValueAndDerivativeThreaderBase.hxx"
#endif

#endif

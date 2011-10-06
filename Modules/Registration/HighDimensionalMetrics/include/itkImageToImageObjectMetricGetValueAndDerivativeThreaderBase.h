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
#ifndef __itkImageToImageObjectMetricGetValueAndDerivativeThreaderBase_h
#define __itkImageToImageObjectMetricGetValueAndDerivativeThreaderBase_h

#include "itkDomainThreader.h"

namespace itk
{

/** \class ImageToImageObjectMetricGetValueAndDerivativeThreaderBase
 * \brief Provides threading for ImageToImageObjectMetric::GetValueAndDerivative.
 *
 *  \tparam TDomainPartitioner type of the Domain,
 *  ThreadedImageRegionPartitioner or ThreadedIndexedContainerPartitioner
 *  \tparam TImageToImageObjectMetric type of the ImageToImageObjectMetric
 *
 *  This class provides a \c BeforeThreadedExecution, and \c
 *  AfterThreadedExecution.
 *
 *  The \c ThreadedExecution in
 *  ImageToImageObjectMetricGetValueAndDerivativeThreader calls \c
 *  ProcessVirtualPoint on every point in the virtual image domain.  \c
 *  ProcessVirtualPoint calls \c ProcessPoint on each point.
 *
 * \ingroup ITKHighDimensionalMetrics */
template < class TDomainPartitioner, class TImageToImageObjectMetric >
class ImageToImageObjectMetricGetValueAndDerivativeThreaderBase
  : public DomainThreader< TDomainPartitioner, TImageToImageObjectMetric >
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageObjectMetricGetValueAndDerivativeThreaderBase  Self;
  typedef DomainThreader< TDomainPartitioner, TImageToImageObjectMetric >
                                                                     Superclass;
  typedef SmartPointer< Self >                                       Pointer;
  typedef SmartPointer< const Self >                                 ConstPointer;

  itkTypeMacro( ImageToImageObjectMetricGetValueAndDerivativeThreaderBase, DomainThreader );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  /** Types of the target class. */
  typedef TImageToImageObjectMetric                                      ImageToImageObjectMetricType;
  typedef typename ImageToImageObjectMetricType::VirtualImageType        VirtualImageType;
  typedef typename ImageToImageObjectMetricType::VirtualIndexType        VirtualIndexType;
  typedef typename ImageToImageObjectMetricType::VirtualPointType        VirtualPointType;
  typedef typename ImageToImageObjectMetricType::FixedImagePointType     FixedImagePointType;
  typedef typename ImageToImageObjectMetricType::FixedImagePixelType     FixedImagePixelType;
  typedef typename ImageToImageObjectMetricType::FixedImageGradientType  FixedImageGradientType;
  typedef typename ImageToImageObjectMetricType::MovingImagePointType    MovingImagePointType;
  typedef typename ImageToImageObjectMetricType::MovingImagePixelType    MovingImagePixelType;
  typedef typename ImageToImageObjectMetricType::MovingImageGradientType MovingImageGradientType;

  typedef typename ImageToImageObjectMetricType::FixedTransformType      FixedTransformType;
  typedef typename FixedTransformType::OutputPointType                   FixedOutputPointType;
  typedef typename ImageToImageObjectMetricType::MovingTransformType     MovingTransformType;
  typedef typename MovingTransformType::OutputPointType                  MovingOutputPointType;

  typedef typename ImageToImageObjectMetricType::MeasureType             MeasureType;
  typedef typename ImageToImageObjectMetricType::DerivativeType          DerivativeType;
  typedef typename ImageToImageObjectMetricType::DerivativeValueType     DerivativeValueType;
  typedef typename ImageToImageObjectMetricType::JacobianType            JacobianType;

  typedef typename ImageToImageObjectMetricType::InternalComputationValueType InternalComputationValueType;
  typedef typename ImageToImageObjectMetricType::NumberOfParametersType       NumberOfParametersType;

protected:
  /** Constructor. */
  ImageToImageObjectMetricGetValueAndDerivativeThreaderBase();

  /** Resize and initialize per thread objects. */
  virtual void BeforeThreadedExecution();

  /** Collects the results from each thread and sums them.  Results are stored
   * in the enclosing class \c m_Value and \c m_DerivativeResult.  Behavior
   * depends on m_AverageValueAndDerivativeByNumberOfValuePoints,
   * m_NumberOfValidPoints, to average the value sum, and to average
   * derivative sums for global transforms only (i.e. transforms without local
   * support).  */
  virtual void AfterThreadedExecution();

  /** Method called by the threaders to process the given virtual point.  This
   * in turn calls \c TransformAndEvaluateFixedPoint, \c
   * TransformAndEvaluateMovingPoint, and \c ProcessPoint.
   * And adds entries to m_MeasurePerThread and m_LocalDerivativesPerThread,
   * m_NumberOfValidPointsPerThread. */
  virtual bool ProcessVirtualPoint( const VirtualIndexType & virtualIndex,
                                    const VirtualPointType & virtualPoint,
                                    const ThreadIdType threadId );

  /** Method to calculate the metric value and derivative
   * given a point, value and image derivative for both fixed and moving
   * spaces. The provided values have been calculated from \c virtualPoint,
   * which is provided in case it's needed.
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
   *   \param metricValueReturn, and
   *   \param localDerivativeReturn
   * \param threadID may be used as needed, for example to access any per-thread
   * data cached during pre-processing by the derived class.
   * \warning  This is called from the threader, and thus must be thread-safe.
   */
  virtual bool ProcessPoint(
        const VirtualPointType &          virtualPoint,
        const FixedImagePointType &       mappedFixedPoint,
        const FixedImagePixelType &       mappedFixedPixelValue,
        const FixedImageGradientType &    mappedFixedImageGradient,
        const MovingImagePointType &      mappedMovingPoint,
        const MovingImagePixelType &      mappedMovingPixelValue,
        const MovingImageGradientType &   mappedMovingImageGradient,
        MeasureType &                     metricValueReturn,
        DerivativeType &                  localDerivativeReturn,
        const ThreadIdType                threadID ) const = 0;


  /** Store derivative result from a single point calculation.
   * \warning If this method is overridden or otherwise not used
   * in a derived class, be sure to *accumulate* results. */
  virtual void StorePointDerivativeResult( const VirtualIndexType & virtualIndex,
                                           const ThreadIdType threadID );

  /** Intermediary threaded metric value storage. */
  mutable std::vector< InternalComputationValueType > m_MeasurePerThread;
  mutable std::vector< DerivativeType >               m_DerivativesPerThread;
  mutable std::vector< DerivativeType >               m_LocalDerivativesPerThread;
  mutable std::vector< SizeValueType >                m_NumberOfValidPointsPerThread;
  /** Pre-allocated transform jacobian objects, for use as needed by dervied
   * classes for efficiency. */
  mutable std::vector< JacobianType >                 m_MovingTransformJacobianPerThread;

private:
  ImageToImageObjectMetricGetValueAndDerivativeThreaderBase( const Self & ); // purposely not implemented
  void operator=( const Self & ); // purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageObjectMetricGetValueAndDerivativeThreaderBase.hxx"
#endif

#endif

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
#ifndef itkANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader_h
#define itkANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"
#include "itkThreadedImageRegionPartitioner.h"
#include "itkThreadedIndexedContainerPartitioner.h"
#include "itkConstNeighborhoodIterator.h"

#include <deque>

namespace itk
{

/**
 *  A template struct to identify different input type arguments. This is used
 *  for function overloading by different threaders. Refer to the comments below.
 */
template <typename T>
struct IdentityHelper
{
  using MyType = T;
};

/** \class ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Threading implementation for ANTS CC metric \c ANTSNeighborhoodCorrelationImageToImageMetricv4 .
 * Supports both dense and sparse threading ways. The dense threader iterates over the whole image domain
 * in order and use a neighborhood scanning window to compute the local cross correlation metric and
 * its derivative incrementally inside the window. The sparse threader uses a sampled point set partitioner to
 * computer local cross correlation only at the sampled positions.
 *
 * This threader class is designed to host the dense and sparse threader under the same name so most computation
 * routine functions and interior member variables can be shared. This eliminates the need to duplicate codes
 * for two threaders. This is made by using function overloading and a helper class to identify different types of
 * domain partitioners.
 *
 *
 * \ingroup ITKMetricsv4
 */
template <typename TDomainPartitioner, typename TImageToImageMetric, typename TNeighborhoodCorrelationMetric>
class ITK_TEMPLATE_EXPORT ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader
  : public ImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader);

  /** Standard class type aliases. */
  using Self = ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader;
  using Superclass = ImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader,
               ImageToImageMetricv4GetValueAndDerivativeThreader);

  itkNewMacro(Self);

  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;

  using VirtualImageType = typename Superclass::VirtualImageType;
  using VirtualPointType = typename Superclass::VirtualPointType;
  using VirtualIndexType = typename Superclass::VirtualIndexType;
  using FixedImagePointType = typename Superclass::FixedImagePointType;
  using FixedImagePixelType = typename Superclass::FixedImagePixelType;
  using FixedImageGradientType = typename Superclass::FixedImageGradientType;
  using MovingImagePointType = typename Superclass::MovingImagePointType;
  using MovingImagePixelType = typename Superclass::MovingImagePixelType;
  using MovingImageGradientType = typename Superclass::MovingImageGradientType;
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using DerivativeValueType = typename Superclass::DerivativeValueType;

  using NeighborhoodCorrelationMetricType = TNeighborhoodCorrelationMetric;

  using ImageRegionType = typename NeighborhoodCorrelationMetricType::ImageRegionType;
  using InternalComputationValueType = typename NeighborhoodCorrelationMetricType::InternalComputationValueType;
  using ImageDimensionType = typename NeighborhoodCorrelationMetricType::ImageDimensionType;
  using JacobianType = typename NeighborhoodCorrelationMetricType::JacobianType;
  using NumberOfParametersType = typename NeighborhoodCorrelationMetricType::NumberOfParametersType;
  using FixedImageType = typename NeighborhoodCorrelationMetricType::FixedImageType;
  using MovingImageType = typename NeighborhoodCorrelationMetricType::MovingImageType;
  using RadiusType = typename NeighborhoodCorrelationMetricType::RadiusType;

  // interested values here updated during scanning
  using QueueRealType = InternalComputationValueType;
  using SumQueueType = std::deque<QueueRealType>;
  using ScanIteratorType = ConstNeighborhoodIterator<VirtualImageType>;

  // one ScanMemType for each thread
  using ScanMemType = struct
  {
    // queues used in the scanning
    // sum of the fixed value squared
    SumQueueType QsumFixed2;
    // sum of the moving value squared
    SumQueueType QsumMoving2;
    SumQueueType QsumFixed;
    SumQueueType QsumMoving;
    SumQueueType QsumFixedMoving;
    SumQueueType Qcount;

    QueueRealType fixedA;
    QueueRealType movingA;
    QueueRealType sFixedMoving;
    QueueRealType sFixedFixed;
    QueueRealType sMovingMoving;

    FixedImageGradientType  fixedImageGradient;
    MovingImageGradientType movingImageGradient;

    FixedImagePointType  mappedFixedPoint;
    MovingImagePointType mappedMovingPoint;
    VirtualPointType     virtualPoint;
  };

  // For dense scan over one image region
  using ScanParametersType = struct
  {
    // const values during scanning
    ImageRegionType scanRegion;
    SizeValueType   numberOfFillZero; // for each queue
    SizeValueType   windowLength;     // number of voxels in the scanning window
    IndexValueType  scanRegionBeginIndexDim0;

    typename FixedImageType::ConstPointer   fixedImage;
    typename MovingImageType::ConstPointer  movingImage;
    typename VirtualImageType::ConstPointer virtualImage;
    RadiusType                              radius;
  };

protected:
  ANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader()
    : m_ANTSAssociate(nullptr)
  {}

  /**
   * Dense threader and sparse threader invoke different in multi-threading. This class uses overloaded
   * implementations of \c ProcessVirtualPoint_impl and \c ThreadExecution_impl in order to handle the
   * dense and sparse cases differently. The helper class IdentityHelper allows for correct overloading
   * these methods when substituting different type of the threaded partitioner
   *
   * 1) Dense threader: through its own \c ThreadedExecution. \c ProcessVirtualPoint and
   * \c ProcessPoint of the base class are thus not used.
   *
   * 2) Sparse threader: through its own \c ProcessVirtualPoint. \c ThreadedExecution still invokes (mostly)
   * from the base class.
   *
   * In order to invoke different \c ThreadedExecution by different threader, we use function overloading
   * techniques to resolve which version of  \c ThreadedExecution and \c ProcessVirtualPoint by
   * the type of the domain partitioner.
   *
   * Specifically, a helper class \c IdentityHelper is used as a function parameter, with the sole purpose
   * to differentiate different types of domain partitioners: \c ThreadedIndexedContainerPartitioner for sparse
   * or \c ThreadedImageRegionPartitioner for dense. \c IdentityHelper is simply a class template, ie. a struct
   * wrapper of type template arguments.
   *
   * This technique takes advantage of SFINAE (Substitution Failure Is Not An Error) in specializing function
   * templates. The helper class \c IdentityHelper is used to overload w.r.t different partitioner types.
   * More discussion can be found at:
   * http://stackoverflow.com/questions/3052579/explicit-specialization-in-non-namespace-scope
   *
   * */

  /** Method called by the threaders to process the given virtual point.  This
   * in turn calls \c TransformAndEvaluateFixedPoint, \c
   * TransformAndEvaluateMovingPoint, and \c ProcessPoint.
   * And adds entries to m_MeasurePerThread and m_LocalDerivativesPerThread,
   * m_NumberOfValidPointsPerThread. */
  bool
  ProcessVirtualPoint(const VirtualIndexType & virtualIndex,
                      const VirtualPointType & virtualPoint,
                      const ThreadIdType       threadId) override
  {
    return ProcessVirtualPoint_impl(IdentityHelper<TDomainPartitioner>(), virtualIndex, virtualPoint, threadId);
  }

  /* specific overloading for sparse CC metric */
  bool
  ProcessVirtualPoint_impl(IdentityHelper<ThreadedIndexedContainerPartitioner> itkNotUsed(self),
                           const VirtualIndexType &                            virtualIndex,
                           const VirtualPointType &                            virtualPoint,
                           const ThreadIdType                                  threadId);

  /* for other default case */
  template <typename T>
  bool
  ProcessVirtualPoint_impl(IdentityHelper<T>        itkNotUsed(self),
                           const VirtualIndexType & virtualIndex,
                           const VirtualPointType & virtualPoint,
                           const ThreadIdType       threadId)
  {
    return Superclass::ProcessVirtualPoint(virtualIndex, virtualPoint, threadId);
  }


  /** \c ProcessPoint() must be overloaded since it is a pure virtual function.
   * It is not used for either sparse or dense threader.
   * */
  bool
  ProcessPoint(const VirtualIndexType &        itkNotUsed(virtualIndex),
               const VirtualPointType &        itkNotUsed(virtualPoint),
               const FixedImagePointType &     itkNotUsed(mappedFixedPoint),
               const FixedImagePixelType &     itkNotUsed(mappedFixedPixelValue),
               const FixedImageGradientType &  itkNotUsed(mappedFixedImageGradient),
               const MovingImagePointType &    itkNotUsed(mappedMovingPoint),
               const MovingImagePixelType &    itkNotUsed(mappedMovingPixelValue),
               const MovingImageGradientType & itkNotUsed(mappedMovingImageGradient),
               MeasureType &                   itkNotUsed(metricValueReturn),
               DerivativeType &                itkNotUsed(localDerivativeReturn),
               const ThreadIdType              itkNotUsed(threadId)) const override
  {
    itkExceptionMacro("ProcessPoint should never be reached in ANTS CC metric threader class.");
  }

  void
  ThreadedExecution(const DomainType & domain, const ThreadIdType threadId) override
  {
    ThreadedExecution_impl(IdentityHelper<TDomainPartitioner>(), domain, threadId);
  }

  /* specific overloading for dense threader only based CC metric */
  void
  ThreadedExecution_impl(
    IdentityHelper<ThreadedImageRegionPartitioner<TImageToImageMetric::VirtualImageDimension>> itkNotUsed(self),
    const DomainType &                                                                         virtualImageSubRegion,
    const ThreadIdType                                                                         threadId);

  /* for other default case */
  template <typename T>
  void
  ThreadedExecution_impl(IdentityHelper<T> itkNotUsed(self), const DomainType & domain, const ThreadIdType threadId);

  /** Common functions for computing correlation over scanning windows **/

  /** Create an iterator over the virtual sub region */
  void
  InitializeScanning(const ImageRegionType & scanRegion,
                     ScanIteratorType &      scanIt,
                     ScanMemType &           scanMem,
                     ScanParametersType &    scanParameters) const;

  /** Update the queues for the next point.  Calls either \c
   * UpdateQueuesAtBeginningOfLine or \c UpdateQueuesToNextScanWindow. */
  void
  UpdateQueues(const ScanIteratorType &   scanIt,
               ScanMemType &              scanMem,
               const ScanParametersType & scanParameters,
               const ThreadIdType         threadId) const;

  void
  UpdateQueuesAtBeginningOfLine(const ScanIteratorType &   scanIt,
                                ScanMemType &              scanMem,
                                const ScanParametersType & scanParameters,
                                const ThreadIdType         threadId) const;

  /** Increment the iterator and check to see if we're at the end of the
   * line.  If so, go to the next line.  Otherwise, add the
   * the values for the next hyperplane. */
  void
  UpdateQueuesToNextScanWindow(const ScanIteratorType &   scanIt,
                               ScanMemType &              scanMem,
                               const ScanParametersType & scanParameters,
                               const ThreadIdType         threadId) const;

  /** Test to see if there are any voxels we need to handle in the current
   * window. */
  bool
  ComputeInformationFromQueues(const ScanIteratorType &   scanIt,
                               ScanMemType &              scanMem,
                               const ScanParametersType & scanParameters,
                               const ThreadIdType         threadId) const;

  void
  ComputeMovingTransformDerivative(const ScanIteratorType &   scanIt,
                                   ScanMemType &              scanMem,
                                   const ScanParametersType & scanParameters,
                                   DerivativeType &           deriv,
                                   MeasureType &              localCC,
                                   const ThreadIdType         threadId) const;

private:
  /** Internal pointer to the metric object in use by this threader.
   *  This will avoid costly dynamic casting in tight loops. */
  TNeighborhoodCorrelationMetric * m_ANTSAssociate;
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkANTSNeighborhoodCorrelationImageToImageMetricv4GetValueAndDerivativeThreader.hxx"
#endif

#endif

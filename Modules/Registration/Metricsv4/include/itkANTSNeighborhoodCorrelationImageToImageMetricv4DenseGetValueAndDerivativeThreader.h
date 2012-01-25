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
#ifndef __itkANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader_h
#define __itkANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"
#include "itkThreadedImageRegionPartitioner.h"

namespace itk
{

/** \class ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader
 * \brief Processes points for NeighborhoodScanningWindow calculation.
 *
 * \ingroup ITKMetricsv4
 */
template< class TImageToImageMetric, class TNeighborhoodCorrelationMetric >
class ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader
  : public ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< TImageToImageMetric::VirtualImageDimension >, TImageToImageMetric >
{
public:
  /** Standard class typedefs. */
  typedef ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader Self;
  typedef ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< TImageToImageMetric::VirtualImageDimension >, TImageToImageMetric >
                                                                                                Superclass;
  typedef SmartPointer< Self >                                                                  Pointer;
  typedef SmartPointer< const Self >                                                            ConstPointer;

  itkTypeMacro( ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader, ImageToImageMetricv4GetValueAndDerivativeThreader );

  itkNewMacro( Self );

  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  typedef typename Superclass::VirtualImageType        VirtualImageType;
  typedef typename Superclass::VirtualPointType        VirtualPointType;
  typedef typename Superclass::VirtualIndexType        VirtualIndexType;
  typedef typename Superclass::FixedImagePointType     FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType     FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType  FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType    MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType    MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType MovingImageGradientType;
  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;
  typedef typename Superclass::DerivativeValueType     DerivativeValueType;

  typedef TNeighborhoodCorrelationMetric                                 NeighborhoodCorrelationMetricType;
  typedef typename NeighborhoodCorrelationMetricType::ScanIteratorType   ScanIteratorType;
  typedef typename NeighborhoodCorrelationMetricType::ScanMemType        ScanMemType;
  typedef typename NeighborhoodCorrelationMetricType::ScanParametersType ScanParametersType;
  typedef typename NeighborhoodCorrelationMetricType::ImageRegionType    ImageRegionType;
  typedef typename NeighborhoodCorrelationMetricType::InternalComputationValueType InternalComputationValueType;
  typedef typename NeighborhoodCorrelationMetricType::SumQueueType       SumQueueType;
  typedef typename NeighborhoodCorrelationMetricType::ImageDimensionType ImageDimensionType;
  typedef typename NeighborhoodCorrelationMetricType::JacobianType       JacobianType;
  typedef typename NeighborhoodCorrelationMetricType::NumberOfParametersType       NumberOfParametersType;

protected:
  ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader() {}

  /** \c ProcessVirtualPoint and \c ProcessPoint are not used in the
   * NeighborhoodScanningWindowGetValueAndDerivativeThreader implementation.
   * */
  virtual bool ProcessPoint(
        const VirtualIndexType &          itkNotUsed(virtualIndex),
        const VirtualPointType &          itkNotUsed(virtualPoint),
        const FixedImagePointType &       itkNotUsed(mappedFixedPoint),
        const FixedImagePixelType &       itkNotUsed(mappedFixedPixelValue),
        const FixedImageGradientType &    itkNotUsed(mappedFixedImageGradient),
        const MovingImagePointType &      itkNotUsed(mappedMovingPoint),
        const MovingImagePixelType &      itkNotUsed(mappedMovingPixelValue),
        const MovingImageGradientType &   itkNotUsed(mappedMovingImageGradient),
        MeasureType &                     itkNotUsed(metricValueReturn),
        DerivativeType &                  itkNotUsed(localDerivativeReturn),
        const ThreadIdType                itkNotUsed(threadID) ) const
    {
    return false;
    }

  virtual void ThreadedExecution( const DomainType& domain,
                                  const ThreadIdType threadId );

  /** Update the queues for the next point.  Calls either \c
   * UpdateQueuesAtBeginningOfLine or \c UpdateQueuesToNextScanWindow. */
  void UpdateQueues(const ScanIteratorType &scanIt,
    ScanMemType &scanMem, const ScanParametersType &scanParameters,
    const ThreadIdType threadID) const;

  void UpdateQueuesAtBeginningOfLine(
    const ScanIteratorType &scanIt, ScanMemType &scanMem,
    const ScanParametersType &scanParameters,
    const ThreadIdType threadID) const;

  /** Increment the iterator and check to see if we're at the end of the
   * line.  If so, go to the next line.  Otherwise, add the
   * the values for the next hyperplane. */
  void UpdateQueuesToNextScanWindow(
    const ScanIteratorType &scanIt, ScanMemType &scanMem,
    const ScanParametersType &scanParameters,
    const ThreadIdType threadID) const;

  /** Test to see if there are any voxels we need to handle in the current
   * window. */
  bool ComputeInformationFromQueues(
    const ScanIteratorType &scanIt, ScanMemType &scanMem,
    const ScanParametersType &scanParameters,
    const ThreadIdType threadID) const;

  void ComputeMovingTransformDerivative(
    const ScanIteratorType &scanIt, ScanMemType &scanMem,
    const ScanParametersType &scanParameters, DerivativeType &deriv,
    MeasureType &local_cc, const ThreadIdType threadID) const;

private:
  ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader( const Self & ); // purposely not implemented
  void operator=( const Self & ); // purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader.hxx"
#endif

#endif

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
#ifndef __itkImageToImageObjectMetricGetValueAndDerivativeThreader_h
#define __itkImageToImageObjectMetricGetValueAndDerivativeThreader_h

#include "itkImageToImageObjectMetricGetValueAndDerivativeThreaderBase.h"
#include "itkNumericTraits.h"
#include "itkThreadedImageRegionPartitioner.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{

/** \class ImageToImageObjectMetricGetValueAndDerivativeThreader
 * \brief Provides threading for ImageToImageObjectMetric::GetValueAndDerivative.
 *
 * \tparam TDomainPartitioner type of the Domain,
 * ThreadedImageRegionPartitioner or ThreadedIndexedContainerPartitioner
 * \tparam TImageToImageObjectMetric type of the ImageToImageObjectMetric
 *
 * This class implements ThreadedExecution.  Template specialization is
 * provided for ThreadedImageRegionPartitioner and
 * ThreadedIndexedContainerPartitioner.
 *
 * \sa ImageToImageObjectMetricGetValueAndDerivativeThreaderBase
 * \ingroup ITKHighDimensionalMetrics
 * */
template < class TDomainPartitioner, class TImageToImageObjectMetric >
class ImageToImageObjectMetricGetValueAndDerivativeThreader
{};

/** \class ImageToImageObjectMetricGetValueAndDerivativeThreader
 * \brief Specialization for ThreadedImageRegionPartitioner.
 * \ingroup ITKHighDimensionalMetrics
 * */
template < class TImageToImageObjectMetric >
class ImageToImageObjectMetricGetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< TImageToImageObjectMetric::VirtualImageDimension >, TImageToImageObjectMetric >
  : public ImageToImageObjectMetricGetValueAndDerivativeThreaderBase< ThreadedImageRegionPartitioner< TImageToImageObjectMetric::VirtualImageDimension >, TImageToImageObjectMetric >
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageObjectMetricGetValueAndDerivativeThreader  Self;
  typedef ImageToImageObjectMetricGetValueAndDerivativeThreaderBase< ThreadedImageRegionPartitioner< TImageToImageObjectMetric::VirtualImageDimension >, TImageToImageObjectMetric >
                                                                 Superclass;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;

  itkTypeMacro( ImageToImageObjectMetricGetValueAndDerivativeThreader, ImageToImageObjectMetricGetValueAndDerivativeThreaderBase );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  /** Types of the target class. */
  typedef TImageToImageObjectMetric                    ImageToImageObjectMetricType;
  typedef typename Superclass::VirtualImageType        VirtualImageType;
  typedef typename Superclass::VirtualIndexType        VirtualIndexType;
  typedef typename Superclass::VirtualPointType        VirtualPointType;
  typedef typename Superclass::FixedImagePointType     FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType     FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType  FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType    MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType    MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType MovingImageGradientType;

  typedef typename Superclass::FixedTransformType      FixedTransformType;
  typedef typename Superclass::FixedOutputPointType    FixedOutputPointType;
  typedef typename Superclass::MovingTransformType     MovingTransformType;
  typedef typename Superclass::MovingOutputPointType   MovingOutputPointType;

  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;
  typedef typename Superclass::DerivativeValueType     DerivativeValueType;
  typedef typename Superclass::JacobianType            JacobianType;

  typedef typename Superclass::InternalComputationValueType InternalComputationValueType;
  typedef typename Superclass::NumberOfParametersType       NumberOfParametersType;

protected:
  /** Constructor. */
  ImageToImageObjectMetricGetValueAndDerivativeThreader() {}

  /** Walk through the given virtual image domain, and call \c ProcessVirtualPoint on every
   * point. */
  virtual void ThreadedExecution( const DomainType & subdomain,
                                  const ThreadIdType threadId );

private:
  ImageToImageObjectMetricGetValueAndDerivativeThreader( const Self & ); // purposely not implemented
  void operator=( const Self & ); // purposely not implemented
};

/** \class ImageToImageObjectMetricGetValueAndDerivativeThreader
 * \brief Specialization for ThreadedIndexedContainerPartitioner.
 * \ingroup ITKHighDimensionalMetrics
 * */
template < class TImageToImageObjectMetric >
class ImageToImageObjectMetricGetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, TImageToImageObjectMetric >
  : public ImageToImageObjectMetricGetValueAndDerivativeThreaderBase< ThreadedIndexedContainerPartitioner, TImageToImageObjectMetric >
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageObjectMetricGetValueAndDerivativeThreader  Self;
  typedef ImageToImageObjectMetricGetValueAndDerivativeThreaderBase< ThreadedIndexedContainerPartitioner, TImageToImageObjectMetric >
                                                                 Superclass;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;

  itkTypeMacro( ImageToImageObjectMetricGetValueAndDerivativeThreader, ImageToImageObjectMetricGetValueAndDerivativeThreaderBase );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  /** Types of the target class. */
  typedef typename Superclass::ImageToImageObjectMetricType     ImageToImageObjectMetricType;
  typedef typename Superclass::VirtualImageType        VirtualImageType;
  typedef typename Superclass::VirtualIndexType        VirtualIndexType;
  typedef typename Superclass::VirtualPointType        VirtualPointType;
  typedef typename Superclass::FixedImagePointType     FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType     FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType  FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType    MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType    MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType MovingImageGradientType;

  typedef typename Superclass::FixedTransformType      FixedTransformType;
  typedef typename Superclass::FixedOutputPointType    FixedOutputPointType;
  typedef typename Superclass::MovingTransformType     MovingTransformType;
  typedef typename Superclass::MovingOutputPointType   MovingOutputPointType;

  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;
  typedef typename Superclass::DerivativeValueType     DerivativeValueType;
  typedef typename Superclass::JacobianType            JacobianType;

  typedef typename Superclass::InternalComputationValueType InternalComputationValueType;
  typedef typename Superclass::NumberOfParametersType       NumberOfParametersType;

protected:
  /** Constructor. */
  ImageToImageObjectMetricGetValueAndDerivativeThreader() {}

  /** Walk through the given virtual image domain, and call \c ProcessVirtualPoint on every
   * point. */
  virtual void ThreadedExecution( const DomainType & subdomain,
                                  const ThreadIdType threadId );

private:
  ImageToImageObjectMetricGetValueAndDerivativeThreader( const Self & ); // purposely not implemented
  void operator=( const Self & ); // purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageObjectMetricGetValueAndDerivativeThreader.hxx"
#endif

#endif

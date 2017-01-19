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
#ifndef itkImageToImageMetricv4GetValueAndDerivativeThreader_h
#define itkImageToImageMetricv4GetValueAndDerivativeThreader_h

#include "itkImageToImageMetricv4GetValueAndDerivativeThreaderBase.h"
#include "itkNumericTraits.h"
#include "itkThreadedImageRegionPartitioner.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{

/** \class ImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Provides threading for ImageToImageMetricv4::GetValueAndDerivative.
 *
 * \tparam TDomainPartitioner type of the Domain,
 * ThreadedImageRegionPartitioner or ThreadedIndexedContainerPartitioner
 * \tparam TImageToImageMetricv4 type of the ImageToImageMetricv4
 *
 * This class implements ThreadedExecution.  Template specialization is
 * provided for ThreadedImageRegionPartitioner and
 * ThreadedIndexedContainerPartitioner.
 *
 * \sa ImageToImageMetricv4GetValueAndDerivativeThreaderBase
 * \ingroup ITKMetricsv4
 * */
template < typename TDomainPartitioner, typename TImageToImageMetricv4 >
class ITK_TEMPLATE_EXPORT ImageToImageMetricv4GetValueAndDerivativeThreader
{};

/** \class ImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Specialization for ThreadedImageRegionPartitioner.
 * \ingroup ITKMetricsv4
 * */
template < typename TImageToImageMetricv4 >
class ITK_TEMPLATE_EXPORT ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< TImageToImageMetricv4::VirtualImageDimension >, TImageToImageMetricv4 >
  : public ImageToImageMetricv4GetValueAndDerivativeThreaderBase< ThreadedImageRegionPartitioner< TImageToImageMetricv4::VirtualImageDimension >, TImageToImageMetricv4 >
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageMetricv4GetValueAndDerivativeThreader      Self;
  typedef ImageToImageMetricv4GetValueAndDerivativeThreaderBase< ThreadedImageRegionPartitioner< TImageToImageMetricv4::VirtualImageDimension >, TImageToImageMetricv4 >
                                                                 Superclass;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;

  itkTypeMacro( ImageToImageMetricv4GetValueAndDerivativeThreader, ImageToImageMetricv4GetValueAndDerivativeThreaderBase );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  /** Types of the target class. */
  typedef TImageToImageMetricv4                        ImageToImageMetricv4Type;
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
  typedef typename Superclass::ImageDimensionType           ImageDimensionType;

protected:
  /** Constructor. */
  ImageToImageMetricv4GetValueAndDerivativeThreader() {}

  /** Walk through the given virtual image domain, and call \c ProcessVirtualPoint on every
   * point. */
  virtual void ThreadedExecution( const DomainType & subdomain,
                                  const ThreadIdType threadId ) ITK_OVERRIDE;

  /** Get cached values for efficiency. Only valid once threading has started.
   *  These methods should be used in tight loops (inlining helps measurably).
   *  Put these methods here so derived threaders can access them directly. */
  inline NumberOfParametersType GetCachedNumberOfParameters() const
  {
    return this->m_CachedNumberOfParameters;
  }
  inline NumberOfParametersType GetCachedNumberOfLocalParameters() const
  {
    return this->m_CachedNumberOfLocalParameters;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToImageMetricv4GetValueAndDerivativeThreader);
};

/** \class ImageToImageMetricv4GetValueAndDerivativeThreader
 * \brief Specialization for ThreadedIndexedContainerPartitioner.
 * \ingroup ITKMetricsv4
 * */
template < typename TImageToImageMetricv4 >
class ITK_TEMPLATE_EXPORT ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, TImageToImageMetricv4 >
  : public ImageToImageMetricv4GetValueAndDerivativeThreaderBase< ThreadedIndexedContainerPartitioner, TImageToImageMetricv4 >
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageMetricv4GetValueAndDerivativeThreader      Self;
  typedef ImageToImageMetricv4GetValueAndDerivativeThreaderBase< ThreadedIndexedContainerPartitioner, TImageToImageMetricv4 >
                                                                 Superclass;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;

  itkTypeMacro( ImageToImageMetricv4GetValueAndDerivativeThreader, ImageToImageMetricv4GetValueAndDerivativeThreaderBase );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  /** Types of the target class. */
  typedef typename Superclass::ImageToImageMetricv4Type ImageToImageMetricv4Type;
  typedef typename Superclass::VirtualImageType         VirtualImageType;
  typedef typename Superclass::VirtualIndexType         VirtualIndexType;
  typedef typename Superclass::VirtualPointType         VirtualPointType;
  typedef typename Superclass::FixedImagePointType      FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType      FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType   FixedImageGradientType;
  typedef typename Superclass::MovingImagePointType     MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType     MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType  MovingImageGradientType;

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
  ImageToImageMetricv4GetValueAndDerivativeThreader() {}

  /** Walk through the given virtual image domain, and call \c ProcessVirtualPoint on every
   * point. */
  virtual void ThreadedExecution( const DomainType & subdomain,
                                  const ThreadIdType threadId ) ITK_OVERRIDE;

  /** Get cached values for efficiency. Only valid once threading has started.
   *  These methods should be used in tight loops (inlining helps measurably).
   *  Put these methods here so derived threaders can access them directly. */
  inline NumberOfParametersType GetCachedNumberOfParameters() const
  {
    return this->m_CachedNumberOfParameters;
  }
  inline NumberOfParametersType GetCachedNumberOfLocalParameters() const
  {
    return this->m_CachedNumberOfLocalParameters;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToImageMetricv4GetValueAndDerivativeThreader);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.hxx"
#endif

#endif

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
#ifndef __itkDemonsImageToImageMetricv4_h
#define __itkDemonsImageToImageMetricv4_h

#include "itkImageToImageMetricv4.h"

#include "itkDemonsImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{

/** \class DemonsImageToImageMetricv4
 *
 *  \brief Class implementing rudimentary demons metric.
 *
 *  See
 *  DemonsImageToImageMetricv4GetValueAndDerivativeThreader::ProcessPoint for algorithm implementation.
 *
 * \ingroup ITKMetricsv4
 */
template <class TFixedImage, class TMovingImage, class TVirtualImage = TFixedImage >
class ITK_EXPORT DemonsImageToImageMetricv4 :
public ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
{
public:
  /** Standard class typedefs. */
  typedef DemonsImageToImageMetricv4                                     Self;
  typedef ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage> Superclass;
  typedef SmartPointer<Self>                                             Pointer;
  typedef SmartPointer<const Self>                                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DemonsImageToImageMetricv4, ImageToImageMetricv4);

  /** Superclass types */
  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;

  typedef typename Superclass::FixedImagePointType     FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType     FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType  FixedImageGradientType;

  typedef typename Superclass::MovingImagePointType    MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType    MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType MovingImageGradientType;

  typedef typename Superclass::MovingTransformType        MovingTransformType;
  typedef typename Superclass::JacobianType               JacobianType;
  typedef typename Superclass::VirtualImageType           VirtualImageType;
  typedef typename Superclass::VirtualIndexType           VirtualIndexType;
  typedef typename Superclass::VirtualPointType           VirtualPointType;
  typedef typename Superclass::VirtualSampledPointSetType VirtualSampledPointSetType;

  /* Image dimension accessors */
  itkStaticConstMacro(VirtualImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<TVirtualImage>::ImageDimension);
  itkStaticConstMacro(FixedImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<TFixedImage>::ImageDimension);
  itkStaticConstMacro(MovingImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<TMovingImage>::ImageDimension);

protected:
  DemonsImageToImageMetricv4();
  virtual ~DemonsImageToImageMetricv4();

  friend class DemonsImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >;
  friend class DemonsImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >;
  typedef DemonsImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >
    DemonsDenseGetValueAndDerivativeThreaderType;
  typedef DemonsImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >
    DemonsSparseGetValueAndDerivativeThreaderType;

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  DemonsImageToImageMetricv4(const Self &); //purposely not implemented
  void operator = (const Self &); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDemonsImageToImageMetricv4.hxx"
#endif

#endif

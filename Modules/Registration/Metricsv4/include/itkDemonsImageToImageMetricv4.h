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
#ifndef __itkDemonsImageToImageObjectMetric_h
#define __itkDemonsImageToImageObjectMetric_h

#include "itkImageToImageObjectMetric.h"

#include "itkDemonsImageToImageObjectMetricGetValueAndDerivativeThreader.h"

namespace itk
{

/** \class DemonsImageToImageObjectMetric
 *
 *  \brief Class implementing rudimentary demons metric.
 *
 *  See
 *  DemonsImageToImageObjectMetricGetValueAndDerivativeThreader::ProcessPoint for algorithm implementation.
 *
 * \ingroup ITKHighDimensionalMetrics
 */
template <class TFixedImage, class TMovingImage, class TVirtualImage = TFixedImage >
class ITK_EXPORT DemonsImageToImageObjectMetric :
public ImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>
{
public:

  /** Standard class typedefs. */
  typedef DemonsImageToImageObjectMetric                      Self;
  typedef ImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>
                                                              Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DemonsImageToImageObjectMetric, ImageToImageObjectMetric);

  /** Superclass types */
  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;

  typedef typename Superclass::FixedImagePointType     FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType     FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType  FixedImageGradientType;

  typedef typename Superclass::MovingImagePointType    MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType    MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType MovingImageGradientType;

  typedef typename Superclass::MovingTransformType     MovingTransformType;
  typedef typename Superclass::JacobianType            JacobianType;
  typedef typename Superclass::VirtualImageType        VirtualImageType;
  typedef typename Superclass::VirtualIndexType        VirtualIndexType;
  typedef typename Superclass::VirtualPointType        VirtualPointType;
  typedef typename Superclass::VirtualSampledPointSetType
                                                       VirtualSampledPointSetType;

  /* Image dimension accessors */
  itkStaticConstMacro(VirtualImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<TVirtualImage>::ImageDimension);
  itkStaticConstMacro(FixedImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<TFixedImage>::ImageDimension);
  itkStaticConstMacro(MovingImageDimension, ImageDimensionType,
      ::itk::GetImageDimension<TMovingImage>::ImageDimension);

protected:
  DemonsImageToImageObjectMetric();
  virtual ~DemonsImageToImageObjectMetric();

  friend class DemonsImageToImageObjectMetricGetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >;
  friend class DemonsImageToImageObjectMetricGetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >;
  typedef DemonsImageToImageObjectMetricGetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< Superclass::VirtualImageDimension >, Superclass, Self >
    DemonsDenseGetValueAndDerivativeThreaderType;
  typedef DemonsImageToImageObjectMetricGetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Superclass, Self >
    DemonsSparseGetValueAndDerivativeThreaderType;

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  DemonsImageToImageObjectMetric(const Self &); //purposely not implemented
  void operator = (const Self &); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDemonsImageToImageObjectMetric.hxx"
#endif

#endif

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

namespace itk
{

/** \class DemonsImageToImageObjectMetric
 *
 *  \brief Class implementing rudimentary demons metric.
 *
 *  See \c GetValueAndDerivativeProcessPoint for algorithm implementation.
 *
 * \ingroup ITKHighDimensionalMetrics
 */
template <class TFixedImage,
          class TMovingImage,
          class TVirtualImage = TFixedImage >
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

  /** superclass types */
  typedef typename Superclass::MeasureType             MeasureType;
  typedef typename Superclass::DerivativeType          DerivativeType;
  typedef typename Superclass::VirtualPointType        VirtualPointType;
  typedef typename Superclass::FixedImagePointType     FixedImagePointType;
  typedef typename Superclass::FixedImagePixelType     FixedImagePixelType;
  typedef typename Superclass::FixedImageGradientType
                                                       FixedImageGradientType;

  typedef typename Superclass::MovingImagePointType    MovingImagePointType;
  typedef typename Superclass::MovingImagePixelType    MovingImagePixelType;
  typedef typename Superclass::MovingImageGradientType
                                                       MovingImageGradientType;

  typedef typename Superclass::MovingTransformType     MovingTransformType;
  typedef typename Superclass::JacobianType            JacobianType;

  /** Initialize. Must be called before first call to GetValue or
   *  GetValueAndDerivative, after metric settings are changed. */
  virtual void Initialize(void) throw ( itk::ExceptionObject );

  void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative) const;

  /** Evaluate and return the metric value.
   * \warning Not yet implemented. */
  MeasureType GetValue() const;

protected:

  /* Worker routine to process each point */
  bool GetValueAndDerivativeProcessPoint(
                    const VirtualPointType &           virtualPoint,
                    const FixedImagePointType &        mappedFixedPoint,
                    const FixedImagePixelType &        fixedImageValue,
                    const FixedImageGradientType &     fixedImageGradient,
                    const MovingImagePointType &       mappedMovingPoint,
                    const MovingImagePixelType &       movingImageValue,
                    const MovingImageGradientType &    movingImageGradient,
                    MeasureType &                      metricValueResult,
                    DerivativeType &                   localDerivativeReturn,
                    const ThreadIdType                 threadID) const;

  DemonsImageToImageObjectMetric();
  virtual ~DemonsImageToImageObjectMetric();
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

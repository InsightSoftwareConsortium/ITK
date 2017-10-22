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
#ifndef itkMeanReciprocalSquareDifferencePointSetToImageMetric_h
#define itkMeanReciprocalSquareDifferencePointSetToImageMetric_h

#include "itkPointSetToImageMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"

namespace itk
{
/** \class MeanReciprocalSquareDifferencePointSetToImageMetric
 * \brief Computes similarity between pixel values of a point set and
 * intensity values in an image.
 *
 * This metric computes the average squared difference between pixels in
 * the point set and transformed point set pixels in the moving image
 * after passing the difference through a function of type
 * \f$ \frac{1}{1+ \frac{ difference^2 }{ \lambda^2 } }\f$.
 * \f$\lambda\f$ controls the capture radius of the metric. The term capture
 * radius used here is in terms of intensity domain and not in the spatial
 * domain.
 *
 * Spatial correspondance between both images is established through a
 * Transform.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template< typename TFixedPointSet, typename TMovingImage >
class ITK_TEMPLATE_EXPORT MeanReciprocalSquareDifferencePointSetToImageMetric:
  public PointSetToImageMetric< TFixedPointSet, TMovingImage >
{
public:

  /** Standard class typedefs. */
  typedef  MeanReciprocalSquareDifferencePointSetToImageMetric  Self;
  typedef PointSetToImageMetric< TFixedPointSet, TMovingImage > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeanReciprocalSquareDifferencePointSetToImageMetric, Object);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType                RealType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::TransformPointer        TransformPointer;
  typedef typename Superclass::TransformParametersType TransformParametersType;
  typedef typename Superclass::TransformJacobianType   TransformJacobianType;
  typedef typename Superclass::InputPointType          InputPointType;
  typedef typename Superclass::OutputPointType         OutputPointType;
  typedef typename Superclass::GradientPixelType       GradientPixelType;

  typedef typename Superclass::MeasureType               MeasureType;
  typedef typename Superclass::DerivativeType            DerivativeType;
  typedef typename Superclass::FixedPointSetType         FixedPointSetType;
  typedef typename Superclass::MovingImageType           MovingImageType;
  typedef typename Superclass::FixedPointSetConstPointer FixedPointSetConstPointer;
  typedef typename Superclass::MovingImageConstPointer   MovingImageConstPointer;

  typedef typename Superclass::PointIterator     PointIterator;
  typedef typename Superclass::PointDataIterator PointDataIterator;

  /** Get the derivatives of the match measure. */
  void GetDerivative(const TransformParametersType & parameters,
                     DerivativeType & Derivative) const ITK_OVERRIDE;

  /**  Get the value for single valued optimizers. */
  MeasureType GetValue(const TransformParametersType & parameters) const ITK_OVERRIDE;

  /**  Get value and derivatives for multiple valued optimizers. */
  void GetValueAndDerivative(const TransformParametersType & parameters,
                             MeasureType & Value, DerivativeType & Derivative) const ITK_OVERRIDE;

  /**  Set/Get the lambda distance. (controls the capture radius of the metric).
     */
  itkSetMacro(Lambda, double);
  itkGetConstMacro(Lambda, double);

protected:
  MeanReciprocalSquareDifferencePointSetToImageMetric();
  virtual ~MeanReciprocalSquareDifferencePointSetToImageMetric() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  MeanReciprocalSquareDifferencePointSetToImageMetric(const Self &); //purposely
                                                                     // not
                                                                     // implemented
  void operator=(const Self &);                                      //purposely
                                                                     // not
                                                                     // implemented

  double m_Lambda;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanReciprocalSquareDifferencePointSetToImageMetric.hxx"
#endif

#endif

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
#ifndef itkNormalizedCorrelationPointSetToImageMetric_h
#define itkNormalizedCorrelationPointSetToImageMetric_h

#include "itkPointSetToImageMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"

namespace itk
{
/** \class NormalizedCorrelationPointSetToImageMetric
 * \brief Computes similarity between pixel values of a point set and
 * intensity values of an image.
 *
 * This metric computes the correlation between point values in the fixed
 * point-set and pixel values in the moving image. The correlation is
 * normalized by the autocorrelation values of both the point-set and the
 * moving image. The spatial correspondence between the point-set and the image
 * is established through a Transform. Pixel values are taken from the fixed
 * point-set. Their positions are mapped to the moving image and result in
 * general in non-grid position on it.  Values at these non-grid position of
 * the moving image are interpolated using a user-selected Interpolator.
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template< typename TFixedPointSet, typename TMovingImage >
class ITK_TEMPLATE_EXPORT NormalizedCorrelationPointSetToImageMetric:
  public PointSetToImageMetric< TFixedPointSet, TMovingImage >
{
public:

  /** Standard class typedefs. */
  typedef NormalizedCorrelationPointSetToImageMetric            Self;
  typedef PointSetToImageMetric< TFixedPointSet, TMovingImage > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedCorrelationPointSetToImageMetric,
               PointSetToImageMetric);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType                RealType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::TransformPointer        TransformPointer;
  typedef typename Superclass::TransformParametersType TransformParametersType;
  typedef typename Superclass::TransformJacobianType   TransformJacobianType;
  typedef typename Superclass::GradientPixelType       GradientPixelType;

  typedef typename Superclass::MeasureType               MeasureType;
  typedef typename Superclass::DerivativeType            DerivativeType;
  typedef typename Superclass::FixedPointSetType         FixedPointSetType;
  typedef typename Superclass::MovingImageType           MovingImageType;
  typedef typename Superclass::FixedPointSetConstPointer FixedPointSetConstPointer;
  typedef typename Superclass::MovingImageConstPointer   MovingImageConstPointer;

  typedef typename Superclass::PointIterator     PointIterator;
  typedef typename Superclass::PointDataIterator PointDataIterator;
  typedef typename Superclass::InputPointType    InputPointType;
  typedef typename Superclass::OutputPointType   OutputPointType;

  /** Get the derivatives of the match measure. */
  void GetDerivative(const TransformParametersType & parameters,
                     DerivativeType & Derivative) const ITK_OVERRIDE;

  /**  Get the value for single valued optimizers. */
  MeasureType GetValue(const TransformParametersType & parameters) const ITK_OVERRIDE;

  /**  Get value and derivatives for multiple valued optimizers. */
  void GetValueAndDerivative(const TransformParametersType & parameters,
                             MeasureType & Value, DerivativeType & Derivative) const ITK_OVERRIDE;

  /** Set/Get SubtractMean boolean. If true, the sample mean is subtracted
   * from the sample values in the cross-correlation formula and
   * typically results in narrower valleys in the cost function.
   * Default value is false. */
  itkSetMacro(SubtractMean, bool);
  itkGetConstReferenceMacro(SubtractMean, bool);
  itkBooleanMacro(SubtractMean);

protected:
  NormalizedCorrelationPointSetToImageMetric();
  virtual ~NormalizedCorrelationPointSetToImageMetric() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NormalizedCorrelationPointSetToImageMetric);

  bool m_SubtractMean;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalizedCorrelationPointSetToImageMetric.hxx"
#endif

#endif

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCorrelationCoefficientHistogramImageToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCorrelationCoefficientHistogramImageToImageMetric_h
#define __itkCorrelationCoefficientHistogramImageToImageMetric_h

#include "itkHistogramImageToImageMetric.h"

namespace itk
{
  /** \class CorrelationCoefficientHistogramImageToImageMetric
      \brief Computes correlation coefficient similarity measure between two
      images to be registered.

      This class is templated over the type of the fixed and moving
      images to be compared.

      This metric computes the similarity measure between pixels in the
      moving image and pixels in the fixed images using a histogram.

      \ingroup RegistrationMetrics */
template <class TFixedImage, class TMovingImage>
class ITK_EXPORT CorrelationCoefficientHistogramImageToImageMetric :
public HistogramImageToImageMetric<TFixedImage, TMovingImage>
{
 public:
  /** Standard class typedefs. */
  typedef CorrelationCoefficientHistogramImageToImageMetric Self;
  typedef HistogramImageToImageMetric<TFixedImage, TMovingImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CorrelationCoefficientHistogramImageToImageMetric,
    HistogramImageToImageMetric);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType RealType;
  typedef typename Superclass::TransformType TransformType;
  typedef typename Superclass::TransformPointer TransformPointer;
  typedef typename Superclass::TransformParametersType TransformParametersType;
  typedef typename Superclass::TransformJacobianType TransformJacobianType;
  typedef typename Superclass::GradientPixelType GradientPixelType;

  typedef typename Superclass::MeasureType MeasureType;
  typedef typename Superclass::DerivativeType DerivativeType;
  typedef typename Superclass::FixedImageType FixedImageType;
  typedef typename Superclass::MovingImageType MovingImageType;
  typedef typename Superclass::FixedImageConstPointer FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer MovingImageConstPointer;

  typedef typename Superclass::HistogramType HistogramType;
  typedef typename HistogramType::FrequencyType HistogramFrequencyType;
  typedef typename HistogramType::Iterator HistogramIteratorType;
  typedef typename HistogramType::MeasurementVectorType
    HistogramMeasurementVectorType;

protected:
  /** Constructor is protected to ensure that \c New() function is used to
      create instances. */
  CorrelationCoefficientHistogramImageToImageMetric(){}
  virtual ~CorrelationCoefficientHistogramImageToImageMetric(){}
  
  /** Evaluates the sum of squared differences from the histogram. */
  virtual MeasureType EvaluateMeasure(HistogramType& histogram) const;

private:
  /** Returns the mean in the x-direction. */
  MeasureType MeanX(HistogramType& histogram) const;

  /** Returns the mean in the y-direction. */
  MeasureType MeanY(HistogramType& histogram) const;

  /** Returns the variance in the x-direction. */
  MeasureType VarianceX(HistogramType& histogram) const;

  /** Returns the variance in the y-direction. */
  MeasureType VarianceY(HistogramType& histogram) const;

  /** Returns the variance. */
  MeasureType Covariance(HistogramType& histogram) const;

  // Purposely not implemented.
  CorrelationCoefficientHistogramImageToImageMetric(Self const&); 
  void operator=(Self const&); // Purposely not implemented.
};

} // End namespace itk.

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCorrelationCoefficientHistogramImageToImageMetric.txx"
#endif

#endif // __itkCorrelationCoefficientHistogramImageToImageMetric_h

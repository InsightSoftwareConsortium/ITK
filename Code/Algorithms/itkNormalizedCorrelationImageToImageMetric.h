/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedCorrelationImageToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNormalizedCorrelationImageToImageMetric_h
#define __itkNormalizedCorrelationImageToImageMetric_h

#include "itkImageToImageMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"


namespace itk
{
/** \class NormalizedCorrelationImageToImageMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This Class is templated over the type of the Images to be compared and
 * over the type of transformation and Iterpolator to be used.
 *
 * This metric computes the sum of squared differenced between pixels in
 * the moving image and pixels in the fixed image. The spatial correspondance 
 * between both images is established through a Transform. Pixel values are
 * taken from the Moving image. Their positions are mapped to the Fixed image
 * and result in general in non-grid position on it. Values at these non-grid
 * position of the Fixed image are interpolated using a user-selected Interpolator.
 *
 * \ingroup RegistrationMetrics
 */
template < class TFixedImage, class TMovingImage > 
class ITK_EXPORT NormalizedCorrelationImageToImageMetric : 
public ImageToImageMetric< TFixedImage, TMovingImage>
{
public:

  /** Standard class typedefs. */
  typedef NormalizedCorrelationImageToImageMetric    Self;
  typedef ImageToImageMetric<TFixedImage, TMovingImage >  Superclass;

  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
 
  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedCorrelationImageToImageMetric, Object);

 
  /** Types transferred from the base class */
  typedef typename Superclass::TransformType            TransformType;
  typedef typename Superclass::TransformPointer         TransformPointer;
  typedef typename Superclass::TransformParametersType  TransformParametersType;
  typedef typename Superclass::TransformJacobianType    TransformJacobianType;

  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::FixedImageType           FixedImageType;
  typedef typename Superclass::MovingImageType          MovingImageType;
  typedef typename Superclass::FixedImagePointer        FixedImagePointer;
  typedef typename Superclass::MovingImagePointer       MovingImagePointer;


  /** Get the derivatives of the match measure. */
  const DerivativeType & GetDerivative( 
                              const TransformParametersType & parameters );

  /**  Get the value for single valued optimizers. */
  MeasureType GetValue( const TransformParametersType & parameters );

  /**  Get value and derivatives for multiple valued optimizers. */
  void GetValueAndDerivative( const TransformParametersType & parameters,
                              MeasureType& Value, DerivativeType& Derivative );

protected:
  NormalizedCorrelationImageToImageMetric();
  virtual ~NormalizedCorrelationImageToImageMetric() {};

private:
  NormalizedCorrelationImageToImageMetric(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalizedCorrelationImageToImageMetric.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedCorrelationPointSetToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNormalizedCorrelationPointSetToImageMetric_h
#define __itkNormalizedCorrelationPointSetToImageMetric_h

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
 */
template < class TFixedPointSet, class TMovingImage > 
class ITK_EXPORT NormalizedCorrelationPointSetToImageMetric : 
    public PointSetToImageMetric< TFixedPointSet, TMovingImage>
{
public:

  /** Standard class typedefs. */
  typedef NormalizedCorrelationPointSetToImageMetric    Self;
  typedef PointSetToImageMetric<TFixedPointSet, TMovingImage >  Superclass;

  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
 
  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedCorrelationPointSetToImageMetric, Object);

 
  /** Types transferred from the base class */
  typedef typename Superclass::RealType                   RealType;
  typedef typename Superclass::TransformType              TransformType;
  typedef typename Superclass::TransformPointer           TransformPointer;
  typedef typename Superclass::TransformParametersType    TransformParametersType;
  typedef typename Superclass::TransformJacobianType      TransformJacobianType;
  typedef typename Superclass::GradientPixelType          GradientPixelType;

  typedef typename Superclass::MeasureType                MeasureType;
  typedef typename Superclass::DerivativeType             DerivativeType;
  typedef typename Superclass::FixedPointSetType          FixedPointSetType;
  typedef typename Superclass::MovingImageType            MovingImageType;
  typedef typename Superclass::FixedPointSetConstPointer  FixedPointSetConstPointer;
  typedef typename Superclass::MovingImageConstPointer    MovingImageConstPointer;

  typedef typename Superclass::PointIterator              PointIterator;
  typedef typename Superclass::PointDataIterator          PointDataIterator;


  /** Get the derivatives of the match measure. */
  void GetDerivative( const TransformParametersType & parameters,
                      DerivativeType & Derivative ) const;

  /**  Get the value for single valued optimizers. */
  MeasureType GetValue( const TransformParametersType & parameters ) const;

  /**  Get value and derivatives for multiple valued optimizers. */
  void GetValueAndDerivative( const TransformParametersType & parameters,
                              MeasureType& Value, DerivativeType& Derivative ) const;

protected:
  NormalizedCorrelationPointSetToImageMetric();
  virtual ~NormalizedCorrelationPointSetToImageMetric() {};

private:
  NormalizedCorrelationPointSetToImageMetric(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalizedCorrelationPointSetToImageMetric.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanReciprocalSquareDifferenceImageToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeanReciprocalSquareDifferenceImageToImageMetric_h
#define __itkMeanReciprocalSquareDifferenceImageToImageMetric_h

#include "itkImageToImageMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"


namespace itk
{
/** \class MeanReciprocalSquareDifferenceImageToImageMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This Class is templated over the type of the Images to be compared and
 * over the type of transformation and Iterpolator to be used.
 *
 * This metric computes the sum of squared differences between pixels in
 * the moving image and pixels in the fixed image after passing the squared
 * difference through a function of type \f$ \frac{1}{1+x} \f$.

 * Spatial correspondance between both images is established through a 
 * Transform. Pixel values are taken from the Moving image. Their positions 
 * are mapped to the Fixed image and result in general in non-grid position 
 * on it. Values at these non-grid position of the Fixed image are interpolated 
 * using a user-selected Interpolator.
 *
 * \ingroup RegistrationMetrics
 */
template < class TFixedImage, class TMovingImage > 
class ITK_EXPORT MeanReciprocalSquareDifferenceImageToImageMetric : 
    public ImageToImageMetric< TFixedImage, TMovingImage>
{
public:

  /** Standard class typedefs. */
  typedef MeanReciprocalSquareDifferenceImageToImageMetric    Self;
  typedef ImageToImageMetric<TFixedImage, TMovingImage >  Superclass;

  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
 
  /** Run-time type information (and related methods). */
  itkTypeMacro(MeanReciprocalSquareDifferenceImageToImageMetric, ImageToImageMetric);

 
  /** Types transferred from the base class */
  typedef typename Superclass::TransformType            TransformType;
  typedef typename Superclass::TransformPointer         TransformPointer;
  typedef typename Superclass::TransformParametersType  TransformParametersType;
  typedef typename Superclass::TransformJacobianType    TransformJacobianType;

  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::FixedImageType           FixedImageType;
  typedef typename Superclass::MovingImageType          MovingImageType;
  typedef typename Superclass::FixedImageConstPointer   FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer  MovingImageConstPointer;


  /** Get the derivatives of the match measure. */
  void GetDerivative( const TransformParametersType & parameters,
                      DerivativeType  & derivative ) const;

  /**  Get the value for single valued optimizers. */
  MeasureType GetValue( const TransformParametersType & parameters ) const;

  /**  Get value and derivatives for multiple valued optimizers. */
  void GetValueAndDerivative( const TransformParametersType & parameters,
                              MeasureType& Value, DerivativeType& derivative ) const;

  /** Set/Get Lambda value. This factor regulates the capture radius of 
      this metric */
  itkGetMacro( Lambda, double );
  itkSetMacro( Lambda, double );


protected:
  MeanReciprocalSquareDifferenceImageToImageMetric();
  virtual ~MeanReciprocalSquareDifferenceImageToImageMetric() {};
  void PrintSelf(std::ostream& os, Indent indent) const;


private:
  MeanReciprocalSquareDifferenceImageToImageMetric(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  double m_Lambda;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanReciprocalSquareDifferenceImageToImageMetric.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanReciprocalSquareDifferencePointSetToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeanReciprocalSquareDifferencePointSetToImageMetric_h
#define __itkMeanReciprocalSquareDifferencePointSetToImageMetric_h

#include "itkPointSetToImageMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"


namespace itk
{
  
/** \class MeanReciprocalSquareDifferencePointSetToImageMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This metric computes the correlation between point values in the fixed
 * point-set and pixel values in the moving image. The correlation is
 * normalized by the autocorrelation values of both the point-set and the
 * moving image. The spatial correspondence between the point-set and the image
 * is established through a Transform. Pixel values are taken from the fixed
 * point-set. Their positions are mapped to the moving image and result in
 * general in non-grid position on it.  Values at these non-grid position of
 * the moving image are interpolated using a user-selected Interpolator.
 
 * \ingroup RegistrationMetrics
 */
template < class TFixedPointSet, class TMovingImage > 
class ITK_EXPORT MeanReciprocalSquareDifferencePointSetToImageMetric : 
    public PointSetToImageMetric< TFixedPointSet, TMovingImage>
{
public:

  /** Standard class typedefs. */
  typedef  MeanReciprocalSquareDifferencePointSetToImageMetric    Self;
  typedef PointSetToImageMetric<TFixedPointSet, TMovingImage >  Superclass;

  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
 
  /** Run-time type information (and related methods). */
  itkTypeMacro( MeanReciprocalSquareDifferencePointSetToImageMetric, Object);

 
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

  /**  Set/Get the lambda distance. (controls the capture radius of the metric).  */
  itkSetMacro( Lambda, double );
  itkGetMacro( Lambda, double );
 

protected:
  MeanReciprocalSquareDifferencePointSetToImageMetric();
  virtual ~MeanReciprocalSquareDifferencePointSetToImageMetric() {};
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  MeanReciprocalSquareDifferencePointSetToImageMetric(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  double          m_Lambda;

    
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanReciprocalSquareDifferencePointSetToImageMetric.txx"
#endif

#endif




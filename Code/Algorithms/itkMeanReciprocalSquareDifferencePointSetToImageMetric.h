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
 * \brief Computes similarity between pixel values of a point set and
 * intensity values in an image.
 *
 * This metric computes the average squared difference between pixels in
 * the point set and transformed point set pixels in the moving image 
 * after passing the difference through a function of type 
 * \f$ \frac{1}{1+ \frac{ difference^2 }{ \lambda^2 } }\f$.
 * $\lambda$ controls the capture radius of the metric. The term capture 
 * radius used here is in terms of intensity domain and not in the spatial 
 * domain.
 *
 * Spatial correspondance between both images is established through a 
 * Transform. 
 *
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




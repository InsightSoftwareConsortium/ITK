/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToImageMetric_h
#define __itkImageToImageMetric_h

#include "itkImageBase.h"
#include "itkTransform.h"
#include "itkInterpolateImageFunction.h"
#include "itkSingleValuedCostFunction.h"

namespace itk
{
  
/** \class ImageToImageMetric
 * \brief Computes similarity between regions of two images.
 *
 * This Class is templated over the type of the two input images.
 * It expects a Transform and an Interpolator to be plugged in.
 * This particular class is the base class for a hierarchy of 
 * similarity metrics.
 *
 * This class computes a value that measures the similarity 
 * between the Fixed image and the transformed Moving image.
 * The Interpolator is used to compute intensity values on 
 * non-grid positions resulting from mapping points through 
 * the Transform.
 * 
 *
 * \ingroup RegistrationMetrics
 *
 */

template <class TFixedImage,  class TMovingImage> 
class ITK_EXPORT ImageToImageMetric : public SingleValuedCostFunction 
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageMetric              Self;
  typedef SingleValuedCostFunction        Superclass;
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  /** Type used for representing point components  */
  typedef double        CoordinateRepresentationType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageMetric, SingleValuedCostFunction);

  /**  Type of the moving Image. */
  typedef TMovingImage                               MovingImageType;
  typedef typename MovingImageType::ConstPointer     MovingImagePointer;

  /**  Type of the fixed Image. */
  typedef TFixedImage                                FixedImageType;
  typedef typename FixedImageType::ConstPointer      FixedImagePointer;


  /**  Type of the Transform Base class */
  typedef Transform<CoordinateRepresentationType, 
         ExtractImageDimension<MovingImageType>::ImageDimension,
         ExtractImageDimension<FixedImageType>::ImageDimension > TransformType;

  typedef typename TransformType::Pointer            TransformPointer;
  typedef typename TransformType::InputPointType     InputPointType;
  typedef typename TransformType::OutputPointType    OutputPointType;
  typedef typename TransformType::ParametersType     TransformParametersType;
  typedef typename TransformType::JacobianType       TransformJacobianType;

  /**  Type of the Interpolator Base class */
  typedef InterpolateImageFunction<MovingImageType>  InterpolatorType;
  typedef typename InterpolatorType::Pointer         InterpolatorPointer;

  /**  Type of the measure. */
  typedef Superclass::MeasureType                    MeasureType;

  /**  Type of the derivative. */
  typedef Superclass::DerivativeType                 DerivativeType;

  /**  Type of the parameters. */
  typedef Superclass::ParametersType                 ParametersType;

  /** Connect the Fixed Image.  */
  itkSetConstObjectMacro( FixedImage, FixedImageType );

  /** Get the Fixed Image. */
  itkGetConstObjectMacro( FixedImage, FixedImageType );

  /** Connect the Moving Image.  */
  itkSetConstObjectMacro( MovingImage, MovingImageType );

  /** Get the Moving Image. */
  itkGetConstObjectMacro( MovingImage, MovingImageType );

  /** Connect the Transform. */
  itkSetObjectMacro( Transform, TransformType );

  /** Get a pointer to the Transform.  */
  itkGetObjectMacro( Transform, TransformType );
 
  /** Connect the Interpolator. */
  itkSetObjectMacro( Interpolator, InterpolatorType );

  /** Get a pointer to the Interpolator.  */
  itkGetObjectMacro( Interpolator, InterpolatorType );

  /** Get the match measure value. */
  itkGetMacro( MatchMeasure, MeasureType );

  /** Get the derivatives of the match measure. */
  itkGetMacro( MatchMeasureDerivatives, DerivativeType );

  /** Get the number of pixels considered in the computation. */
  itkGetMacro( NumberOfPixelsCounted, unsigned long );

  /** Set the parameters defining the Transform. */
  void SetTransformParameters( const ParametersType & parameters );

protected:
  ImageToImageMetric();
  virtual ~ImageToImageMetric() {};
  void PrintSelf(std::ostream& os, Indent indent) const;


  MeasureType                 m_MatchMeasure;
  DerivativeType              m_MatchMeasureDerivatives;
  unsigned long               m_NumberOfPixelsCounted;

  FixedImagePointer           m_FixedImage;
  MovingImagePointer          m_MovingImage;

  TransformPointer            m_Transform;
  InterpolatorPointer         m_Interpolator;

private:
  ImageToImageMetric(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageMetric.txx"
#endif

#endif




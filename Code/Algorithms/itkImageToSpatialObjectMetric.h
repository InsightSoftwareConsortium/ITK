/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToSpatialObjectMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToSpatialObjectMetric_h
#define __itkImageToSpatialObjectMetric_h

#include "itkSingleValuedCostFunction.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkLinearInterpolateImageFunction.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk
{
  
/** \class ImageToSpatialObjectMetric
 * \brief Computes similarity between a moving spatial obejct 
 *        and an Image to be registered
 */

template < class TFixedImage, class TMovingSpatialObject> 
class ITK_EXPORT ImageToSpatialObjectMetric 
                           : public SingleValuedCostFunction
{
public:
  /** Standard "Self" typedef. */
  typedef ImageToSpatialObjectMetric  Self;
  /** Standard "Superclass" typedef. */
  typedef SingleValuedCostFunction  Superclass;
  /** Smart pointer typedef support   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Type of the fixed image */
  typedef TFixedImage          FixedImageType;
 
  /** Type of the MovingSpatialObject */
  typedef TMovingSpatialObject  MovingSpatialObjectType;

  /** Type used for representing point components  */
  typedef Superclass::ParametersValueType CoordinateRepresentationType;

  /** Image dimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int, FixedImageType::ImageDimension);
 
  /** Object dimension enumeration. */
  itkStaticConstMacro(ObjectDimension, unsigned int, MovingSpatialObjectType::ObjectDimension);
 
  /**  Type of the Transform Base class */
  typedef Transform<CoordinateRepresentationType, 
    ObjectDimension,
    itkGetStaticConstMacro(ImageDimension) > TransformType;

  typedef typename TransformType::Pointer            TransformPointer;
  typedef typename TransformType::InputPointType     InputPointType;
  typedef typename TransformType::OutputPointType    OutputPointType;
  typedef typename TransformType::ParametersType     TransformParametersType;
  typedef typename TransformType::JacobianType       TransformJacobianType;

  /**  Type of the Interpolator Base class */
  typedef LinearInterpolateImageFunction<
                      TFixedImage,
                      CoordinateRepresentationType > InterpolatorType;

  typedef typename InterpolatorType::Pointer         InterpolatorPointer;

  /** Typede of the vector type to return derivatives */
  typedef vnl_vector_fixed<double,ObjectDimension> VectorType;

  /**  Type of the match measure */
  typedef Superclass::MeasureType          MeasureType;

  /** Type of the derivative of the match measure */
  typedef Superclass::DerivativeType   DerivativeType; 

  /** Pointer type for the FixedImage  */
  typedef typename FixedImageType::Pointer    FixedImagePointer;

  /** Pointer type for the MovingSpatialObject */
  typedef typename MovingSpatialObjectType::Pointer  
                                               MovingSpatialObjectPointer;

  /** Const pointer type for the FixedImage */
  typedef typename FixedImageType::ConstPointer   FixedImageConstPointer;
  
  /** Const pointer type for the MovingSpatialObject */
  typedef typename MovingSpatialObjectType::ConstPointer    
                                           MovingSpatialObjectConstPointer;

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType         ParametersType;

  /** Run-time type information (and related methods).*/
  itkTypeMacro(ImageToSpatialObjectMetric, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Connect the FixedImage. */
  itkSetConstObjectMacro( FixedImage, FixedImageType );
  
  /** Get the FixedImage. */
  itkGetConstObjectMacro( FixedImage, FixedImageType );

  /** Connect the MovingSpatialObject */
  itkSetConstObjectMacro( MovingSpatialObject, MovingSpatialObjectType );
  
  /** Get the MovingSpatialObject. */
  itkGetConstObjectMacro( MovingSpatialObject, MovingSpatialObjectType );

  /** Connect the Interpolator. */
  itkSetObjectMacro( Interpolator, InterpolatorType );
  
  /** Get the Interpolator. */
  itkGetObjectMacro( Interpolator, InterpolatorType );

  /** Get the Derivatives of the Match Measure */
  virtual void GetDerivative( const ParametersType & parameters,
                                    DerivativeType & derivative ) const = 0;

  /** Get the Value for SingleValue Optimizers */
  virtual MeasureType    GetValue( const ParametersType & parameters ) const =0;

  /** Get Value and Derivatives for MultipleValuedOptimizers */
  virtual void GetValueAndDerivative( const ParametersType & parameters,
       MeasureType & Value, DerivativeType  & Derivative ) const =0;

  /** Initialize the metric */
  virtual void Initialize(void) {}
 
  /** Get the last transformation parameters visited by 
   * the optimizer. This function overload the superclass's one */
  itkGetConstReferenceMacro( LastTransformParameters, ParametersType );

  /** Set/Get the Transform. */
  itkSetObjectMacro( Transform, TransformType );

protected:

  ImageToSpatialObjectMetric();
  virtual ~ImageToSpatialObjectMetric() {};
  ImageToSpatialObjectMetric(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  MeasureType                 m_MatchMeasure;
  DerivativeType              m_MatchMeasureDerivatives;
  mutable TransformPointer    m_Transform;
  InterpolatorPointer         m_Interpolator;

  MovingSpatialObjectConstPointer   m_MovingSpatialObject;
  FixedImageConstPointer            m_FixedImage;
  ParametersType                    m_LastTransformParameters;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkImageToSpatialObjectMetric.txx"
#endif

#endif



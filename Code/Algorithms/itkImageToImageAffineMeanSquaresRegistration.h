/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineMeanSquaresRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImageToImageAffineMeanSquaresRegistration_h
#define __itkImageToImageAffineMeanSquaresRegistration_h

#include "itkRegistrationMethod.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"
#include "itkImageMapper.h"
#include "itkAffineRegistrationTransform.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkPoint.h"

namespace itk
{

/**
 * \class ImageToImageAffineMeanSquaresRegistration
 * \brief Base class for registration methods
 *
 * This Class define the generic interface for a registration method.
 * The basic elements of a registration method are:
 *   - Metric to compare the reference and the target
 *   - Transformation used to register the reference against the target
 *   - Optimization method used to search for the best transformation
 * 
 * Registration is not limited to Images, and for this reason
 * this class is templated over the type of the reference object,
 * the target object and the transformation. This types are obtained
 * from the Metric type, to reduce the number of redundant
 * template parameters
 *
 */

template <class TReference, class TTarget>
class ITK_EXPORT ImageToImageAffineMeanSquaresRegistration 
: public RegistrationMethod<TReference,TTarget>
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef ImageToImageAffineMeanSquaresRegistration  Self;


  /**
   * Standard "Superclass" typedef.
   */
   typedef RegistrationMethod  Superclass;


  /** 
   * Smart pointer typedef support 
   */
   typedef SmartPointer<Self>   Pointer;
   typedef SmartPointer<const Self>  ConstPointer;

  /**
   *  Type of the Reference
   */
   typedef TReference  ReferenceType;

  /**
   *  Type of the Target
   */
   typedef TTarget TargetType;

  /**
   * Image Dimensions
   */
   enum {ImageDimension = ReferenceType::ImageDimension,
         ParametersDimension = ImageDimension*(ImageDimension+1) };

  /**
   *  Type of the parameters
   */
   typedef Point<double,ParametersDimension>   ParametersType;

  /**
   *  Type of the Transformation
   */
   typedef AffineRegistrationTransform<
                double, 
                ImageDimension, 
                ParametersType > TransformationType;
	  
  /**
   *  Type of the Mapper
   */
   typedef ImageMapper<ReferenceType,TransformationType>  MapperType;

  /**
   *  Type of the Metric
   */
   typedef MeanSquaresImageToImageMetric<TargetType, MapperType>   MetricType;


  /**
   *  Type of the Optimizer 
   */
   typedef RegularStepGradientDescentOptimizer<MetricType>           OptimizerType;


  /**
   *  Pointer type for the optimizer 
   */
   typedef typename OptimizerType::Pointer     OptimizerPointer;


  /**
   *  Pointer type for the Reference 
   */
   typedef typename ReferenceType::Pointer ReferencePointer;

  
  /**
   *  Pointer type for the Target 
   */
   typedef typename TargetType::Pointer TargetPointer;


  /**
   *  Pointer type for the Transformation
   */
   typedef  typename TransformationType::Pointer TransformationPointer;


  /**
   *  Pointer type for the metric
   */
   typedef  typename MetricType::Pointer        MetricPointer;


  /**
   *  Pointer type for the mapper
   */
   typedef typename MapperType::Pointer        MapperPointer;


  /**
   * Interpolation type
   */
   typedef LinearInterpolateImageFunction<TargetType>  InterpolatorType;

  /** 
   * Run-time type information (and related methods).
   */
   itkTypeMacro(ImageToImageAffineMeanSquaresRegistration, RegistrationMethod);


  /**
   * Method for creation through the object factory.
   */
   itkNewMacro(Self);
  

  /**
   * Method that initiates the registration.
   */
   int StartRegistration(void);


  /**
   * Set the Target
   */
   void SetTarget( TargetType * );

   
  /**
   * Set the Reference
   */
   void SetReference( ReferenceType * );

  /**
   * Get the Reference
   */
   itkGetMacro( Reference, ReferencePointer );
   
  /**
   * Get the Target
   */
   itkGetMacro( Target, TargetPointer );


  /**
   * Get the Transformation
   */
   itkGetMacro( Transformation, TransformationPointer );


  /**
   * Get the Optimizer
   */
   itkGetMacro( Optimizer, OptimizerPointer );


  /**
   * Set Translation Scale
   */
   void SetTranslationScale( const double & scale )
   { m_TranslationScale = scale; }



protected:

  ImageToImageAffineMeanSquaresRegistration();
  virtual ~ImageToImageAffineMeanSquaresRegistration();
  ImageToImageAffineMeanSquaresRegistration(const Self&);
  const Self & operator=(const Self&);
 

private:

  TargetPointer              m_Target;
  ReferencePointer           m_Reference;
  TransformationPointer      m_Transformation;
  MapperPointer              m_Mapper;  
  MetricPointer              m_Metric;
  OptimizerPointer           m_Optimizer;
  ParametersType             m_Parameters;
  double                     m_TranslationScale;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageAffineMeanSquaresRegistration.txx"
#endif

#endif




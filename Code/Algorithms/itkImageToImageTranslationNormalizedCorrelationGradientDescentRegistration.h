/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageTranslationNormalizedCorrelationGradientDescentRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImageToImageTranslationNormalizedCorrelationGradientDescentRegistration_h
#define __itkImageToImageTranslationNormalizedCorrelationGradientDescentRegistration_h

#include "itkRegistrationMethod.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNormalizedCorrelationImageToImageMetric.h"
#include "itkGradientDescentOptimizer.h"
#include "itkImage.h"
#include "itkImageMapper.h"
#include "itkTranslationRegistrationTransform.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkPoint.h"

namespace itk
{

/**
 *  Traits class that defines the different types to be
 *  used by this registration method
 */
template <class TReference, class TTarget>
class ITK_EXPORT ImageToImageTranslationNormalizedCorrelationGradientDescentRegistrationTraits 
{
  
public:

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
         ParametersDimension = ImageDimension };

  /**
   *  Type of the parameters
   */
   typedef Point<double,ParametersDimension>   ParametersType;

  /**
   *  Type of the Transformation
   */
   typedef TranslationRegistrationTransform<
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
   typedef NormalizedCorrelationImageToImageMetric<TargetType, MapperType>   MetricType;


  /**
   *  Type of the Optimizer 
   */
   typedef GradientDescentOptimizer<MetricType>           OptimizerType;


};


/**
 * \class ImageToImageTranslationNormalizedCorrelationGradientDescentRegistration
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
class ITK_EXPORT ImageToImageTranslationNormalizedCorrelationGradientDescentRegistration 
: public RegistrationMethod< 
            ImageToImageTranslationNormalizedCorrelationGradientDescentRegistrationTraits<
               TReference,
               TTarget>  >
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef ImageToImageTranslationNormalizedCorrelationGradientDescentRegistration  Self;


  /**
   * Standard "Superclass" typedef.
   */
   typedef RegistrationMethod< 
            ImageToImageTranslationNormalizedCorrelationGradientDescentRegistrationTraits<
                                       TReference,
                                       TTarget>  >           Superclass;


  /** 
   * Smart pointer typedef support 
   */
   typedef SmartPointer<Self>   Pointer;
   typedef SmartPointer<const Self>  ConstPointer;

  /**
   *  Type of the Reference
   */
   typedef TReference ReferenceType;

  /**
   *  Type of the Target
   */
   typedef TTarget   TargetType;


  /**
   *  Type of the parameters
   */
   typedef typename Superclass::ParametersType ParametersType;


  /**
   *  Type of the Transformation
   */
   typedef typename Superclass::TransformationType TransformationType;
	 
   
  /**
   *  Type of the Mapper
   */
   typedef typename Superclass::MapperType    MapperType;


  /**
   *  Type of the Metric
   */
   typedef typename Superclass::MetricType   MetricType;



   /**
   *  Type of the Optimizer 
   */
   typedef typename Superclass::OptimizerType       OptimizerType;



  /** 
   * Run-time type information (and related methods).
   */
   itkTypeMacro(ImageToImageTranslationNormalizedCorrelationGradientDescentRegistration, RegistrationMethod);


  /**
   * Method for creation through the object factory.
   */
   itkNewMacro(Self);
  

  /**
   * Method that initiates the registration.
   */
   int StartRegistration(void);


  /**
   * Set Translation Scale
   */
   void SetTranslationScale( const double & scale )
   { m_TranslationScale = scale; }


   /** 
    *  Dimension of the images
    */
   enum { ImageDimension = TTarget::ImageDimension,
          ParametersDimension = ImageDimension };



protected:

  ImageToImageTranslationNormalizedCorrelationGradientDescentRegistration();
  virtual ~ImageToImageTranslationNormalizedCorrelationGradientDescentRegistration();
  ImageToImageTranslationNormalizedCorrelationGradientDescentRegistration(const Self&);
  const Self & operator=(const Self&);
 

private:

  ParametersType             m_Parameters;
  double                     m_TranslationScale;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageTranslationNormalizedCorrelationGradientDescentRegistration.txx"
#endif

#endif




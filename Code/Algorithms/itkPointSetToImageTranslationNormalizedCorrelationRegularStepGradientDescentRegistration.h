/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkPointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration_h
#define __itkPointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration_h

#include "itkRegistrationMethod.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNormalizedCorrelationPointSetToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"
#include "itkImageMapper.h"
#include "itkTranslationRegistrationTransform.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkPointSet.h"

namespace itk
{

/**
 *  Traits class that defines the different types to be
 *  used by this registration method
 */
template <class TReference, class TTarget>
class ITK_EXPORT
PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistrationTraits 
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
   typedef NormalizedCorrelationPointSetToImageMetric<TargetType, MapperType>   MetricType;


  /**
   *  Type of the Optimizer 
   */
   typedef RegularStepGradientDescentOptimizer<MetricType>           OptimizerType;


};



/**
 * \class PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration
 * \brief Base class for registration methods
 *
 * This Class define the generic interface for a registration method.
 * The basic elements of a registration method are:
 *   - Metric to compare the reference and the target
 *   - Transformation used to register the reference against the target
 *   - Optimization method used to search for the best transformation
 * 
 * This class registers a PointSet with an Image.
 * the Image is considered the Reference given that is the one that is
 * mapped under the transformation each time a value is required.
 *
 */

template <class TReference, class TTarget>
class ITK_EXPORT PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration 
: public RegistrationMethod< 
            PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistrationTraits<
               TReference,
               TTarget>  >
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration  Self;


  /**
   * Standard "Superclass" typedef.
   */
   typedef
     PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistrationTraits<
        TReference,
        TTarget>  Superclass;


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
   * Image Dimensions
   */
   enum {ImageDimension = ReferenceType::ImageDimension,
         ParametersDimension = ImageDimension };

  /** 
   * Run-time type information (and related methods).
   */
   itkTypeMacro(PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration,
       RegistrationMethod);


  /**
   * Method for creation through the object factory.
   */
   itkNewMacro(Self);
  

  /**
   * Method that initiates the registration.
   */
   int StartRegistration(void);



protected:

  PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration();
  virtual ~PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration();
  PointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration(const Self&);
  const Self & operator=(const Self&);
 

private:

  ParametersType             m_Parameters;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToImageTranslationNormalizedCorrelationRegularStepGradientDescentRegistration.txx"
#endif

#endif




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkPointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration_h
#define __itkPointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration_h

#include "itkRegistrationMethod.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkPatternIntensityPointSetToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"
#include "itkImageMapper.h"
#include "itkRigid3DRegistrationTransform.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPointSet.h"

namespace itk
{

/**
 *  Traits class that defines the different types to be
 *  used by this registration method
 */
template <class TReference, class TTarget>
class ITK_EXPORT
PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistrationTraits 
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
         ParametersDimension = 7 }; // one quaternion + one vector

  /**
   *  Type of the parameters
   */
   typedef Point<double,ParametersDimension>   ParametersType;

  /**
   *  Type of the Transformation
   */
   typedef Rigid3DRegistrationTransform<
                double, 
                ParametersType > TransformationType;
	  
  /**
   *  Type of the Mapper
   */
   typedef ImageMapper<ReferenceType,TransformationType>  MapperType;

  /**
   *  Type of the Metric
   */
   typedef PatternIntensityPointSetToImageMetric<TargetType, MapperType>   MetricType;


  /**
   *  Type of the Optimizer 
   */
   typedef RegularStepGradientDescentOptimizer<MetricType>           OptimizerType;


};



/**
 * \class PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration
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
 * \ingroup PointSetToImageRegistration
 *
 */

template <class TReference, class TTarget>
class ITK_EXPORT PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration 
: public RegistrationMethod< 
            PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistrationTraits<
               TReference,
               TTarget>  >
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration  Self;


  /**
   * Standard "Superclass" typedef.
   */
   typedef
     PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistrationTraits<
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
   itkTypeMacro(PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration,
       RegistrationMethod);


  /**
   * Method for creation through the object factory.
   */
   itkNewMacro(Self);
  

  /**
   * Method that initiates the registration.
   */
   void StartRegistration(void);



protected:

  PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration();
  virtual ~PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration();
  PointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration(const Self&);
  const Self & operator=(const Self&);
 

private:

  ParametersType             m_Parameters;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToImageRigid3DPatternIntensityRegularStepGradientDescentRegistration.txx"
#endif

#endif




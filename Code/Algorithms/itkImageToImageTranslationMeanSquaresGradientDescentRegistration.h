/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageTranslationMeanSquaresGradientDescentRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToImageTranslationMeanSquaresGradientDescentRegistration_h
#define __itkImageToImageTranslationMeanSquaresGradientDescentRegistration_h

#include "itkRegistrationMethod.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkGradientDescentOptimizer.h"
#include "itkImage.h"
#include "itkImageMapper.h"
#include "itkTranslationTransform.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPoint.h"

namespace itk
{

/**
 *  Traits class that defines the different types to be
 *  used by this registration method
 *
 * \deprecated This class was intended to be used with the 
 * Registration framework. It is now longer in use after 
 * the remotion of most of the template parameters in this framework.
 * This class will be removed soon.
 */
template <class TReference, class TTarget>
class ITK_EXPORT ImageToImageTranslationMeanSquaresGradientDescentRegistrationTraits 
{
public:
  /**  Type of the reference. */
  typedef TReference  ReferenceType;

  /**  Type of the target. */
  typedef TTarget TargetType;

  /** Image dimensions. */
  enum { ImageDimension = ReferenceType::ImageDimension }; 

  /**  Type of the transformation. */
  typedef TranslationTransform<double,ImageDimension> TransformationType;

  /** Parameters dimensions. */
  enum { ParametersDimension = TransformationType::ParametersDimension }; 

  /**  Type of the parameters. */
  typedef typename TransformationType::ParametersType  ParametersType;
  
  /**  Type of the mapper. */
  typedef ImageMapper<ReferenceType,TransformationType>  MapperType;

  /**  Type of the metric. */
  typedef MeanSquaresImageToImageMetric<TargetType, MapperType>   MetricType;

  /**  Type of the optimizer.  */
  typedef GradientDescentOptimizer           OptimizerType;
};

/** \class ImageToImageTranslationMeanSquaresGradientDescentRegistration
 * \brief Base class for registration methods
 *
 * \deprecated This class was intended to be used with the 
 * Registration framework. It is now longer in use after 
 * the remotion of most of the template parameters in this framework.
 * This class will be removed soon.
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
 * \ingroup RigidImageRegistration
 */
template <class TReference, class TTarget>
class ITK_EXPORT ImageToImageTranslationMeanSquaresGradientDescentRegistration 
: public RegistrationMethod< 
         ImageToImageTranslationMeanSquaresGradientDescentRegistrationTraits<TReference,TTarget> >
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageTranslationMeanSquaresGradientDescentRegistration  Self;
  typedef RegistrationMethod< 
          ImageToImageTranslationMeanSquaresGradientDescentRegistrationTraits<           TReference,TTarget> >    Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageTranslationMeanSquaresGradientDescentRegistration,
               RegistrationMethod);

  /**  Type of the reference. */
  typedef TReference ReferenceType;

  /**  Type of the target. */
  typedef TTarget   TargetType;

  /**  Type of the parameters. */
  typedef typename Superclass::ParametersType ParametersType;

  /**  Type of the transformation. */
  typedef typename Superclass::TransformationType TransformationType;
   
  /**  Type of the mapper. */
  typedef typename Superclass::MapperType    MapperType;

  /**  Type of the metric. */
  typedef typename Superclass::MetricType   MetricType;

  /**  Type of the optimizer.  */
  typedef typename Superclass::OptimizerType       OptimizerType;

  /** Method that initiates the registration. */
  void StartRegistration(void);

  /** Set the translation scale. */
  void SetTranslationScale( const double & scale )
    { m_TranslationScale = scale; }

  /**  Dimension of the images.  */
  enum { ImageDimension = ReferenceType::ImageDimension,
         ParametersDimension = ImageDimension };

protected:
  ImageToImageTranslationMeanSquaresGradientDescentRegistration();
  virtual ~ImageToImageTranslationMeanSquaresGradientDescentRegistration();

private:
  ImageToImageTranslationMeanSquaresGradientDescentRegistration(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  ParametersType             m_Parameters;
  double                     m_TranslationScale;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageTranslationMeanSquaresGradientDescentRegistration.txx"
#endif

#endif




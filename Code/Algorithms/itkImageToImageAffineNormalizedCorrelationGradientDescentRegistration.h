/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineNormalizedCorrelationGradientDescentRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToImageAffineNormalizedCorrelationGradientDescentRegistration_h
#define __itkImageToImageAffineNormalizedCorrelationGradientDescentRegistration_h

#include "itkRegistrationMethod.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNormalizedCorrelationImageToImageMetric.h"
#include "itkGradientDescentOptimizer.h"
#include "itkImage.h"
#include "itkImageMapper.h"
#include "itkAffineTransform.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPoint.h"

namespace itk
{

/**
 *  Traits class that defines the different types to be
 *  used by this registration method
 */
template <class TReference, class TTarget>
class ITK_EXPORT ImageToImageAffineNormalizedCorrelationGradientDescentRegistrationTraits 
{
public:
  /**  Type of the reference. */
  typedef TReference  ReferenceType;

  /**  Type of the target. */
  typedef TTarget TargetType;

  /** Image dimensions. */
  enum { ImageDimension = ReferenceType::ImageDimension }; 

  /**  Type of the transformation. */
  typedef AffineTransform<double, ImageDimension> TransformationType;
    
  /**  Type of the parameters. */
  typedef typename TransformationType::ParametersType  ParametersType;

  /** Parameters dimensions. */
  enum { ParametersDimension = TransformationType::ParametersDimension }; 
 
  /**  Type of the mapper. */
  typedef ImageMapper<ReferenceType,TransformationType>  MapperType;

  /**  Type of the metric. */
  typedef NormalizedCorrelationImageToImageMetric<TargetType, MapperType>   MetricType;

  /**  Type of the optimizer.  */
  typedef GradientDescentOptimizer<MetricType>           OptimizerType;
};

/** \class ImageToImageAffineNormalizedCorrelationGradientDescentRegistration
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
 *  \ingroup AffineImageRegistration
 */
template <class TReference, class TTarget>
class ITK_EXPORT ImageToImageAffineNormalizedCorrelationGradientDescentRegistration 
: public RegistrationMethod< 
      ImageToImageAffineNormalizedCorrelationGradientDescentRegistrationTraits<
               TReference,TTarget>  >
{
public:
  /** Standard class typedefs. */
  typedef ImageToImageAffineNormalizedCorrelationGradientDescentRegistration  Self;
  typedef RegistrationMethod< 
  ImageToImageAffineNormalizedCorrelationGradientDescentRegistrationTraits<
  TReference,TTarget> >   Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToImageAffineNormalizedCorrelationGradientDescentRegistration, RegistrationMethod);

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

  /** Set translation scale. */
  void SetTranslationScale( const double & scale )
    { m_TranslationScale = scale; }

  /**  Dimension of the images.  */
  enum { ImageDimension = ReferenceType::ImageDimension };

protected:
  ImageToImageAffineNormalizedCorrelationGradientDescentRegistration();
  virtual ~ImageToImageAffineNormalizedCorrelationGradientDescentRegistration();

private:
  ImageToImageAffineNormalizedCorrelationGradientDescentRegistration(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  ParametersType             m_Parameters;
  double                     m_TranslationScale;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageAffineNormalizedCorrelationGradientDescentRegistration.txx"
#endif

#endif




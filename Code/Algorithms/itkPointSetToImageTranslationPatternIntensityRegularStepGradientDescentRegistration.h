/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration_h
#define __itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration_h

#include "itkRegistrationMethod.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkPatternIntensityPointSetToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"
#include "itkImageMapper.h"
#include "itkTranslationTransform.h"
#include "itkPointSet.h"

namespace itk
{

/**
 *  Traits class that defines the different types to be
 *  used by this registration method
 *
 *  It is expected that the reference will be an itk::Image and the
 *  target will be an itk::PointSet or an itk::Mesh class
 */
template <class TReference, class TTarget>
class ITK_EXPORT
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistrationTraits 
{
public:
  /**  Type of the reference. */
  typedef TReference  ReferenceType;

  /**  Type of the target. */
  typedef TTarget TargetType;

  /** Image dimensions and parameters dimension. */
  enum { ImageDimension = ReferenceType::ImageDimension };

  /**  Type of the point used to represent coordinates in space */
  typedef typename TargetType::PointType   PointType;
    
  /**  Type used to represent space coordinates */
  typedef typename PointType::CoordRepType      CoordinatesType;
 
  /**  Type of the transformation. */
  typedef TranslationTransform< CoordinatesType,
                                ImageDimension > TransformationType;

  /** Parameters dimension. */
  enum { ParametersDimension = TransformationType::ParametersDimension };

  /**  Type of the parameters. */
  typedef typename TransformationType::ParametersType   ParametersType;

  /**  Type of the mapper. */
  typedef ImageMapper<ReferenceType,TransformationType>  MapperType;

  /**  Type of the metric. */
  typedef PatternIntensityPointSetToImageMetric<TargetType, MapperType>   MetricType;

  /**  Type of the optimizer.  */
  typedef RegularStepGradientDescentOptimizer  OptimizerType;
};

/** \class PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration
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
 */
template <class TReference, class TTarget>
class ITK_EXPORT PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration 
: public RegistrationMethod< 
            PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistrationTraits<TReference,TTarget>  >
{
public:
  /** Standard class typedefs. */
  typedef PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration  Self;
  typedef PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistrationTraits<TReference,TTarget>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration,  RegistrationMethod);

  /**  Type of the reference. */
  typedef TReference  ReferenceType;

  /**  Type of the target. */
  typedef TTarget TargetType;

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

  /** Image dimensions. */
  enum {ImageDimension = ReferenceType::ImageDimension,
        ParametersDimension = ImageDimension };

  /** Method that initiates the registration. */
  void StartRegistration(void);

protected:
  PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration();
  virtual ~PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration();

private:
  PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  ParametersType             m_Parameters;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration.txx"
#endif

#endif




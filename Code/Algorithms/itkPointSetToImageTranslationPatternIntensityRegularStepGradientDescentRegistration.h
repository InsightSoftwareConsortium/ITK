/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration_h
#define __itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration_h

#include "itkRegistrationMethod.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkPatternIntensityPointSetToImageMetric.h"
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
PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistrationTraits 
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
   typedef PatternIntensityPointSetToImageMetric<TargetType, MapperType>   MetricType;


  /**
   *  Type of the Optimizer 
   */
   typedef RegularStepGradientDescentOptimizer<MetricType>           OptimizerType;


};



/**
 * \class PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration
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
class ITK_EXPORT PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration 
: public RegistrationMethod< 
            PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistrationTraits<
               TReference,
               TTarget>  >
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration  Self;


  /**
   * Standard "Superclass" typedef.
   */
   typedef
     PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistrationTraits<
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
   itkTypeMacro(PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration,
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

  PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration();
  virtual ~PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration();
  PointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration(const Self&);
  const Self & operator=(const Self&);
 

private:

  ParametersType             m_Parameters;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToImageTranslationPatternIntensityRegularStepGradientDescentRegistration.txx"
#endif

#endif




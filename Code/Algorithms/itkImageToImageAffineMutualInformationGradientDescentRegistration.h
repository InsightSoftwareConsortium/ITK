/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineMutualInformationGradientDescentRegistration.h
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
#ifndef __itkImageToImageAffineMutualInformationGradientDescentRegistration_h
#define __itkImageToImageAffineMutualInformationGradientDescentRegistration_h

#include "itkRegistrationMethod.h"
#include "itkMutualInformationImageToImageMetric.h"
#include "itkGradientDescentOptimizer.h"
#include "itkImageMapper.h"
#include "itkAffineTransform.h"
#include "itkPoint.h"

namespace itk
{

/**
 *  Traits class that defines the different types to be
 *  used by this registration method
 */
template <class TReference, class TTarget>
class ITK_EXPORT 
  ImageToImageAffineMutualInformationGradientDescentRegistrationTraits 
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
   *  Type of the Transformation
   */

   typedef AffineTransform<
                            double, 
                            ReferenceType::ImageDimension 
                                            >  TransformationType;

	/**
   *  Type of the parameters
   */
   typedef typename TransformationType::ParametersType  ParametersType;

  /**
   * Image Dimensions
   */
   enum { ImageDimension = ReferenceType::ImageDimension }; 


  /**
   * Parameters Dimensions
   */
   enum { ParametersDimension = TransformationType::ParametersDimension }; 


  /**
   *  Type of the Mapper
   */
   typedef ImageMapper<ReferenceType,TransformationType>  MapperType;

  /**
   *  Type of the Metric
   */
   typedef MutualInformationImageToImageMetric<TargetType, MapperType>   MetricType;


  /**
   *  Type of the Optimizer 
   */
   typedef GradientDescentOptimizer<MetricType>           OptimizerType;


};



/**
 * \class ImageToImageAffineMutualInformationGradientDescentRegistration
 * \brief Register two images using mutual information.
 *
 * ImageToImageAffineMutualInformationGradientDescentRegistration performs an
 * affine registration
 * between a target and reference image using mutual information.
 * It uses the optimization method of Viola and Wells to find the
 * best affine transform to register the reference image onto the target
 * image. The two images are defined via methods SetTarget()
 * and SetReference().
 *
 * The mutual information value and its derivatives are estimated
 * using spatial sampling. The performance
 * of the registration depends on good choices of the parameters
 * used to estimate the mutual information. Refer to the documentation
 * for MutualInformationImageToImageMetric for details on these
 * parameters and how to set them.
 *
 * The registration uses a simple stochastic gradient ascent scheme. Steps
 * are repeatedly taken that are proportional to the approximate
 * deriviative of the mutual information with respect to the affine
 * transform parameters. The stepsize is governed by the LearningRate
 * parameter. The default is 1.0. The LearningRate is set via the method
 * SetLearningRate().
 *
 * Since the parameters of the linear part is different in magnitude
 * to the parameters in the offset part, scaling is required
 * to improve convergence. The scaling can set via SetTranslationScale().
 * The default is 1.0 but should be set to the maximum expected translation.
 *
 * NB: In the Viola and Wells paper, the scaling is specified by
 * using different learning rates for the linear and offset part.
 * The following formula translate their scaling parameters to
 * those used in this framework:
 *
 * LearningRate = lambda_R
 *
 * TranslationScale = sqrt( lambda_T / lambda_R );
 *
 * Optimization performance can be improved by setting the image
 * origin to center of mass of the image.
 *
 * Implementaton of this class is based on:
 * Viola, P. and Wells III, W. (1997).
 * "Alignment by Maximization of Mutual Information"
 * International Journal of Computer Vision, 24(2):137-154
 *
 * This class is templated over the reference image type and the
 * the target image type.
 *
 * \sa MutualInformationImageToImageMetric
 * \sa AffineTransform
 *
 *  \ingroup AffineImageRegistration
 */
template <class TReference, class TTarget>
class ITK_EXPORT 
ImageToImageAffineMutualInformationGradientDescentRegistration 
: public RegistrationMethod< 
   ImageToImageAffineMutualInformationGradientDescentRegistrationTraits
   <TReference, TTarget>  >
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef ImageToImageAffineMutualInformationGradientDescentRegistration  Self;

  /**
   * Standard "Superclass" typedef.
   */
   typedef RegistrationMethod< 
      ImageToImageAffineMutualInformationGradientDescentRegistrationTraits<
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
   typedef TReference  ReferenceType;

  /**
   *  Type of the Target
   */
   typedef TTarget TargetType;

  /**
   * Image Dimensions
   */
   enum {ImageDimension = ReferenceType::ImageDimension};

  /**
   * Parmeters Dimensions
   */
   enum { ParametersDimension = ImageDimension * ( ImageDimension + 1 ) };

  /**
   * Parameters Type
   */
   typedef typename Superclass::ParametersType   ParametersType;

  /**
   *  Type of the Transformation
   */
   typedef typename Superclass::TransformationType    TransformationType;

  /**
   * Point Type
   */
  typedef typename TransformationType::InputPointType  PointType;

  /**
   *  Type of the Mapper
   */
   typedef typename Superclass::MapperType   MapperType;

  /**
   *  Type of the Metric
   */
   typedef typename Superclass::MetricType   MetricType;

  /**
   *  Type of the Optimizer 
   */
   typedef typename  Superclass::OptimizerType  OptimizerType;

  /**
   *  Pointer type for the optimizer 
   */
   typedef typename OptimizerType::Pointer  OptimizerPointer;

  /**
   *  Pointer type for the Reference
   */
   typedef typename ReferenceType::ConstPointer ReferenceConstPointer;

  /**
   *  Pointer type for the Target
   */
   typedef typename TargetType::ConstPointer TargetConstPointer;

  /**
   *  Pointer type for the Transformation
   */
   typedef  typename TransformationType::Pointer TransformationPointer;

  /**
   *  Pointer type for the metric
   */
   typedef  typename MetricType::Pointer  MetricPointer;

  /**
   *  Pointer type for the mapper
   */
   typedef typename MapperType::Pointer  MapperPointer;

  /**
   * Run-time type information (and related methods).
   */
   itkTypeMacro(ImageToImageAffineMutualInformationGradientDescentRegistration,
       RegistrationMethod);

  /**
   * Method for creation through the object factory.
   */
   itkNewMacro(Self);

  /**
   * Method that initiates the registration.
   */
   void StartRegistration( void );


  /**
   * Set the transform parameters
   */
   void SetParameters( const ParametersType&  params )
    { m_Parameters = params; }

  /**
   * Get the transform parameters
   */
  const ParametersType& GetParameters( void ) const
    { return m_Parameters; }

  /**
   * Set the translation scale
   */
  itkSetMacro( TranslationScale, double );

  /**
   * Get the translation scale
   */
  itkGetConstMacro( TranslationScale, double );

  /**
   * Set the learning rate. This is used in the optimization scheme.
   * Typically, the learning rate needs to be annealed (decreased over
   * time). Default is 1.0.
   */
  itkSetClampMacro( LearningRate, double, 0.0,
    NumericTraits<double>::max() );

  /**
   * Get the learning rate.
   */
  itkGetMacro( LearningRate, double );

  /**
   * Set the number of iterations. This determines the number of
   * iterations performed in the steepest descent optimization.
   * Default is 1000.
   */
  itkSetMacro( NumberOfIterations, unsigned int );

  /**
   * Get the number of iterations.
   */
  itkGetMacro( NumberOfIterations, unsigned int );

protected:

  ImageToImageAffineMutualInformationGradientDescentRegistration();
  virtual ~ImageToImageAffineMutualInformationGradientDescentRegistration();
  ImageToImageAffineMutualInformationGradientDescentRegistration(const Self&);
  const Self & operator=(const Self&);

private:

  ParametersType             m_Parameters;

  // -------------------------------
  // Optimization related variables
  // -------------------------------
  double                     m_TranslationScale;
  double                     m_LearningRate;
  unsigned int               m_NumberOfIterations;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToImageAffineMutualInformationGradientDescentRegistration.txx"
#endif

#endif



/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkSimpleImageRegistrationMethod_hxx
#define __itkSimpleImageRegistrationMethod_hxx

#include "itkSimpleImageRegistrationMethod.h"

#include "itkDiscreteGaussianImageFilter.h"
#include "itkGradientDescentObjectOptimizer.h"
#include "itkIterationReporter.h"
#include "itkJointHistogramMutualInformationImageToImageObjectMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegistrationParameterScalesFromShift.h"
#include "itkShrinkImageFilter.h"

namespace itk
{
/**
 * Constructor
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::SimpleImageRegistrationMethod()
{
  this->SetNumberOfRequiredOutputs( 1 );

  this->m_CurrentLevel = 0;

  this->m_FixedImage = NULL;
  this->m_MovingImage = NULL;
  this->m_CompositeTransform = NULL;

  typedef LinearInterpolateImageFunction<FixedImageType, RealType> DefaultFixedInterpolatorType;
  typename DefaultFixedInterpolatorType::Pointer fixedInterpolator = DefaultFixedInterpolatorType::New();
  this->m_FixedInterpolator = fixedInterpolator;

  typedef LinearInterpolateImageFunction<MovingImageType, RealType> DefaultMovingInterpolatorType;
  typename DefaultMovingInterpolatorType::Pointer movingInterpolator = DefaultMovingInterpolatorType::New();
  this->m_MovingInterpolator = movingInterpolator;

  typedef JointHistogramMutualInformationImageToImageObjectMetric<FixedImageType, MovingImageType> MetricForStageOneType;
  typename MetricForStageOneType::Pointer mutualInformationMetric = MetricForStageOneType::New();
  mutualInformationMetric = mutualInformationMetric;
  mutualInformationMetric->SetNumberOfHistogramBins( 20 );
  mutualInformationMetric->SetDoFixedImagePreWarp( true );
  mutualInformationMetric->SetDoMovingImagePreWarp( true );
  mutualInformationMetric->SetUseMovingImageGradientFilter( false );
  mutualInformationMetric->SetUseFixedImageGradientFilter( false );
  mutualInformationMetric->SetUseFixedSampledPointSet( false );
  this->m_Metric = mutualInformationMetric;

  typedef RegistrationParameterScalesFromShift<MetricType> ScalesEstimatorForAffineTransformType;
  typename ScalesEstimatorForAffineTransformType::Pointer scalesEstimator = ScalesEstimatorForAffineTransformType::New();
  scalesEstimator->SetMetric( mutualInformationMetric );
  scalesEstimator->SetTransformForward( true );

  typedef GradientDescentObjectOptimizer DefaultOptimizerType;
  typename DefaultOptimizerType::Pointer optimizer = DefaultOptimizerType::New();
  optimizer->SetLearningRate( 1.0 );
  optimizer->SetNumberOfIterations( 200 );
  optimizer->SetScalesEstimator( scalesEstimator );
  this->m_Optimizer = optimizer;

  TransformOutputPointer transformDecorator = static_cast<TransformOutputType *>( this->MakeOutput( 0 ).GetPointer() );
  this->ProcessObject::SetNthOutput( 0, transformDecorator.GetPointer() );

  // By default we set up an affine transform for a 3-level image registration.

  this->SetNumberOfLevels( 3 );

  this->m_Transform = TransformType::New();

  this->m_ShrinkFactorsPerLevel.SetSize( this->m_NumberOfLevels );
  this->m_ShrinkFactorsPerLevel[0] = 2;
  this->m_ShrinkFactorsPerLevel[1] = 1;
  this->m_ShrinkFactorsPerLevel[2] = 1;

  this->m_SmoothingSigmasPerLevel.SetSize( this->m_NumberOfLevels );
  this->m_SmoothingSigmasPerLevel[0] = 2;
  this->m_SmoothingSigmasPerLevel[1] = 1;
  this->m_SmoothingSigmasPerLevel[2] = 0;

}

template<typename TFixedImage, typename TMovingImage, typename TTransform>
SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::~SimpleImageRegistrationMethod()
{
}

/*
 * Initialize by setting the interconnects between components.
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::InitializeRegistrationAtEachLevel( const SizeValueType level )
{
  // Sanity checks

  if ( !this->m_Optimizer )
    {
    itkExceptionMacro( "The optimizer is not present." );
    }
  if ( !this->m_FixedInterpolator )
    {
    itkExceptionMacro( "The fixed image interpolator is not present." );
    }
  if ( !this->m_MovingInterpolator )
    {
    itkExceptionMacro( "The moving image interpolator is not present." );
    }

  if ( !this->m_Metric )
    {
    itkExceptionMacro( "The image metric is not present." );
    }
  if( !this->m_Transform )
    {
    itkExceptionMacro( "The transform is not present." );
    }

  // For each level, we adapt the current transform.  For many transforms, e.g.
  // affine, the base transform adaptor does not do anything.  However, in the
  // case of other transforms, e.g. the b-spline and displacement field transforms
  // the fixed parameters are changed to reflect an increase in transform resolution.
  // This could involve increasing the mesh size of the B-spline transform or
  // increase the resolution of the displacement field.

  if( this->m_TransformParametersAdaptorsPerLevel[level] )
    {
    this->m_TransformParametersAdaptorsPerLevel[level]->SetTransform( this->m_Transform );
    this->m_TransformParametersAdaptorsPerLevel[level]->AdaptTransformParameters();
    }

  // At each resolution, we can
  //   1. subsample the reference domain (typically the fixed image) and/or
  //   2. smooth the fixed and moving images.

  typedef ShrinkImageFilter<FixedImageType, FixedImageType> ShrinkFilterType;
  typename ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
  shrinkFilter->SetShrinkFactors( this->m_ShrinkFactorsPerLevel[level] );
  shrinkFilter->SetInput( this->m_FixedImage );
  shrinkFilter->Update();

  typedef DiscreteGaussianImageFilter<FixedImageType, FixedImageType> FixedImageSmoothingFilterType;
  typename FixedImageSmoothingFilterType::Pointer fixedImageSmoothingFilter = FixedImageSmoothingFilterType::New();
  fixedImageSmoothingFilter->SetUseImageSpacingOn();
  fixedImageSmoothingFilter->SetVariance( vnl_math_sqr( this->m_SmoothingSigmasPerLevel[level] ) );
  fixedImageSmoothingFilter->SetMaximumError( 0.01 );
  fixedImageSmoothingFilter->SetInput( this->m_FixedImage );
  fixedImageSmoothingFilter->Update();

  typedef DiscreteGaussianImageFilter<MovingImageType, MovingImageType> MovingImageSmoothingFilterType;
  typename MovingImageSmoothingFilterType::Pointer movingImageSmoothingFilter = MovingImageSmoothingFilterType::New();
  movingImageSmoothingFilter->SetUseImageSpacingOn();
  movingImageSmoothingFilter->SetVariance( vnl_math_sqr( this->m_SmoothingSigmasPerLevel[level] ) );
  movingImageSmoothingFilter->SetMaximumError( 0.01 );
  movingImageSmoothingFilter->SetInput( this->m_MovingImage );
  movingImageSmoothingFilter->Update();

  typedef IdentityTransform<RealType, ImageDimension> IdentityTransformType;
  typename IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();

  // Handle the composite transform
  if( level == 0 )
    {
    if( this->m_CompositeTransform )
      {
      // Check for the case where the user added the transform before starting
      // the registration
      if( this->m_Transform.GetPointer() != this->m_CompositeTransform->GetBackTransform() )
        {
        this->m_CompositeTransform->AddTransform( this->m_Transform );
        }
      }
    else
      {
      this->m_CompositeTransform = CompositeTransformType::New();
      this->m_CompositeTransform->AddTransform( this->m_Transform );
      }
    this->m_CompositeTransform->SetOnlyMostRecentTransformToOptimizeOn();
    }

  // Update the image metric
  this->m_Metric->SetFixedTransform( identityTransform );
  this->m_Metric->SetMovingTransform( this->m_CompositeTransform );
  this->m_Metric->SetFixedInterpolator( this->m_FixedInterpolator );
  this->m_Metric->SetMovingInterpolator( this->m_MovingInterpolator );
  this->m_Metric->SetFixedImage( fixedImageSmoothingFilter->GetOutput() );
  this->m_Metric->SetMovingImage( movingImageSmoothingFilter->GetOutput() );
  this->m_Metric->SetVirtualDomainImage( shrinkFilter->GetOutput() );
  this->m_Metric->Initialize();

  // Update the optimizer

  this->m_Optimizer->SetMetric( this->m_Metric );
}

/*
 * Start the registration
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::GenerateData()
{
  TransformOutputType *transformOutput = static_cast<TransformOutputType *>( this->ProcessObject::GetOutput( 0 ) );

  transformOutput->Set( this->m_Transform.GetPointer() );

  for( this->m_CurrentLevel = 0; this->m_CurrentLevel < this->m_NumberOfLevels; this->m_CurrentLevel++ )
    {
    IterationReporter reporter( this, 0, 1 );
    reporter.CompletedStep();

    this->InitializeRegistrationAtEachLevel( this->m_CurrentLevel );

    this->m_Optimizer->StartOptimization();
    }

  TransformOutputPointer transformDecorator = TransformOutputType::New().GetPointer();
  transformDecorator->Set( this->m_Transform );
  this->ProcessObject::SetNthOutput( 0, transformDecorator );
}

/**
 * Set the moving transform adaptors per stage
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::SetTransformParametersAdaptorsPerLevel( TransformParametersAdaptorsContainerType & adaptors )
{
  if( this->m_NumberOfLevels != adaptors.size() )
    {
    itkExceptionMacro( "The number of levels does not equal the number array size." );
    }
  else
    {
    itkDebugMacro( "Setting the transform parameter adaptors." );
    this->m_TransformParametersAdaptorsPerLevel = adaptors;
    this->Modified();
    }
}

/**
 * Get the moving transform adaptors per stage
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
const typename  SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>::TransformParametersAdaptorsContainerType &
SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::GetTransformParametersAdaptorsPerLevel() const
{
  return this->m_TransformParametersAdaptorsPerLevel;
}

/**
 * Set the number of levels
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::SetNumberOfLevels( const SizeValueType numberOfLevels )
{
  if( this->m_NumberOfLevels != numberOfLevels )
    {
    this->m_NumberOfLevels = numberOfLevels;

    // Set default transform adaptors which don't do anything to the input transform
    // Similarly, fill in some default values for the shrink factors, smoothing sigmas,
    // and learning rates.

    this->m_TransformParametersAdaptorsPerLevel.clear();
    for( SizeValueType level = 0; level < this->m_NumberOfLevels; level++ )
      {
      typename TransformParametersAdaptorType::Pointer transformParametersAdaptor = TransformParametersAdaptorType::New();
      this->m_TransformParametersAdaptorsPerLevel.push_back( transformParametersAdaptor.GetPointer() );
      }

    this->m_ShrinkFactorsPerLevel.SetSize( this->m_NumberOfLevels );
    this->m_ShrinkFactorsPerLevel.Fill( 1 );

    this->m_SmoothingSigmasPerLevel.SetSize( this->m_NumberOfLevels );
    this->m_SmoothingSigmasPerLevel.Fill( 1.0 );

    this->Modified();
    }
}

/*
 * PrintSelf
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << "Fixed image: " << this->m_FixedImage << std::endl;
  os << "Moving image: " << this->m_MovingImage << std::endl;

  os << indent << "Moving interpolator:" << std::endl;
  this->m_MovingInterpolator->Print( std::cout, indent );
  os << indent << "Fixed interpolator:" << std::endl;
  this->m_FixedInterpolator->Print( std::cout, indent );

    os << "Number of levels = " << this->m_NumberOfLevels << std::endl;
//    os << indent << "Image metric:" << std::endl;
//    this->m_ImageMetrics[stage]->Print( os, indent );
//
//    os << indent << "Transform:" << std::endl;
//    this->m_CompositeTransform->GetNthTransform( stage )->Print( os, indent );
//    os << indent << "Transform adaptors:" << std::endl;
//    for( SizeValueType level = 0; level < this->m_NumberOfLevels[stage]; level++ )
//      {
//      os << indent << "Level " << level << std::endl;
//      this->m_TransformParametersAdaptors[stage][level]->Print( os, indent );
//      }

  os << indent << "Shrink factors: " << this->m_ShrinkFactorsPerLevel << std::endl;
  os << indent << "Smoothing sigmas: " << this->m_SmoothingSigmasPerLevel << std::endl;
}

/*
 *  Get output composite transform
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
const typename SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>::TransformOutputType *
SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::GetOutput() const
{
  return static_cast<const TransformOutputType *>( this->ProcessObject::GetOutput( 0 ) );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform>
DataObject::Pointer
SimpleImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::MakeOutput( unsigned int output )
{
  switch ( output )
    {
    case 0:
      return static_cast<DataObject *>( TransformOutputType::New().GetPointer() );
      break;
    default:
      itkExceptionMacro("MakeOutput request for an output number larger than the expected number of outputs");
      return 0;
    }
}

} // end namespace itk

#endif

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
#ifndef __itkImageRegistrationMethodv4_hxx
#define __itkImageRegistrationMethodv4_hxx

#include "itkImageRegistrationMethodv4.h"

#include "itkDiscreteGaussianImageFilter.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageToImageMetricv4.h"
#include "itkIterationReporter.h"
#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"

namespace itk
{
/**
 * Constructor
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::ImageRegistrationMethodv4()
{
  ProcessObject::SetNumberOfRequiredOutputs(1);
  Self::SetPrimaryOutputName("Transform");

  // indexed input are alternating fixed and moving images
  Self::SetPrimaryInputName("Fixed");
  Self::AddRequiredInputName("Moving",1);
  ProcessObject::SetNumberOfRequiredInputs(2);

  // optional named inputs
  Self::SetInput("InitialTransform", ITK_NULLPTR);
  Self::SetInput("FixedInitialTransform", ITK_NULLPTR);
  Self::SetInput("MovingInitialTransform", ITK_NULLPTR);


  Self::ReleaseDataBeforeUpdateFlagOff();

  this->m_CurrentLevel = 0;
  this->m_CurrentIteration = 0;
  this->m_CurrentMetricValue = 0.0;
  this->m_CurrentConvergenceValue = 0.0;
  this->m_IsConverged = false;
  this->m_NumberOfFixedImages = 0;
  this->m_NumberOfMovingImages = 0;

  Self::ReleaseDataBeforeUpdateFlagOff();

  this->m_InPlace = true;

  this->m_CompositeTransform = CompositeTransformType::New();

  typedef IdentityTransform<RealType, ImageDimension> IdentityTransformType;

  typedef MattesMutualInformationImageToImageMetricv4<FixedImageType, MovingImageType, VirtualImageType, RealType> DefaultMetricType;
  typename DefaultMetricType::Pointer mutualInformationMetric = DefaultMetricType::New();
  mutualInformationMetric->SetNumberOfHistogramBins( 20 );
  mutualInformationMetric->SetUseMovingImageGradientFilter( false );
  mutualInformationMetric->SetUseFixedImageGradientFilter( false );
  mutualInformationMetric->SetUseFixedSampledPointSet( false );
  this->m_Metric = mutualInformationMetric;

  typedef RegistrationParameterScalesFromPhysicalShift<DefaultMetricType> DefaultScalesEstimatorType;
  typename DefaultScalesEstimatorType::Pointer scalesEstimator = DefaultScalesEstimatorType::New();
  scalesEstimator->SetMetric( mutualInformationMetric );
  scalesEstimator->SetTransformForward( true );

  typedef GradientDescentOptimizerv4Template<RealType> DefaultOptimizerType;
  typename DefaultOptimizerType::Pointer optimizer = DefaultOptimizerType::New();
  optimizer->SetLearningRate( 1.0 );
  optimizer->SetNumberOfIterations( 1000 );
  optimizer->SetScalesEstimator( scalesEstimator );
  this->m_Optimizer = optimizer;

  this->m_OptimizerWeights.SetSize( 0 );
  this->m_OptimizerWeightsAreIdentity = true;


  DecoratedOutputTransformPointer transformDecorator =
        itkDynamicCastInDebugMode< DecoratedOutputTransformType * >( this->MakeOutput(0).GetPointer() );
  this->ProcessObject::SetNthOutput( 0, transformDecorator );
  this->m_OutputTransform = transformDecorator->GetModifiable();

  // By default we set up a 3-level image registration.

  this->m_NumberOfLevels = 0;
  this->SetNumberOfLevels( 3 );

  this->m_ShrinkFactorsPerLevel.resize( this->m_NumberOfLevels );
  ShrinkFactorsPerDimensionContainerType shrinkFactors;
  shrinkFactors.Fill( 2 );
  this->m_ShrinkFactorsPerLevel[0] = shrinkFactors;
  shrinkFactors.Fill( 1 );
  this->m_ShrinkFactorsPerLevel[1] = shrinkFactors;
  shrinkFactors.Fill( 1 );
  this->m_ShrinkFactorsPerLevel[2] = shrinkFactors;

  this->m_SmoothingSigmasPerLevel.SetSize( this->m_NumberOfLevels );
  this->m_SmoothingSigmasPerLevel[0] = 2;
  this->m_SmoothingSigmasPerLevel[1] = 1;
  this->m_SmoothingSigmasPerLevel[2] = 0;

  this->m_SmoothingSigmasAreSpecifiedInPhysicalUnits = true;

  this->m_MetricSamplingStrategy = NONE;
  this->m_MetricSamplingPercentagePerLevel.SetSize( this->m_NumberOfLevels );
  this->m_MetricSamplingPercentagePerLevel.Fill( 1.0 );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::~ImageRegistrationMethodv4()
{
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::SetFixedImage( SizeValueType index, const FixedImageType *image )
{
  itkDebugMacro( "setting fixed image input " << index << " to " << image );
  if( image != static_cast<FixedImageType *>( this->ProcessObject::GetInput( 2 * index ) ) )
    {
    if( !this->ProcessObject::GetInput( 2 * index ) )
      {
      this->m_NumberOfFixedImages++;
      }
    this->ProcessObject::SetNthInput( 2 * index, const_cast<FixedImageType *>( image ) );
    this->Modified();
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
const typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>::FixedImageType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::GetFixedImage( SizeValueType index ) const
{
  itkDebugMacro( "returning fixed image input " << index << " of "
                                    << static_cast<const FixedImageType *>( this->ProcessObject::GetInput( 2 * index ) ) );
  return static_cast<const FixedImageType *>( this->ProcessObject::GetInput( 2 * index ) );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::SetMovingImage( SizeValueType index, const MovingImageType *image )
{
  itkDebugMacro( "setting moving image input " << index << " to " << image );
  if( image != static_cast<MovingImageType *>( this->ProcessObject::GetInput( 2 * index + 1 ) ) )
    {
    if( !this->ProcessObject::GetInput( 2 * index + 1 ) )
      {
      this->m_NumberOfMovingImages++;
      }
    this->ProcessObject::SetNthInput( 2 * index + 1, const_cast<MovingImageType *>( image ) );
    this->Modified();
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
const typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>::MovingImageType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::GetMovingImage( SizeValueType index ) const
{
  itkDebugMacro( "returning moving image input " << index << " of "
                                    << static_cast<const MovingImageType *>( this->ProcessObject::GetInput( 2 * index + 1 ) ) );
  return static_cast<const MovingImageType *>( this->ProcessObject::GetInput( 2 * index + 1 ) );
}

/*
 * Set optimizer weights and do checking for identity.
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::SetOptimizerWeights( OptimizerWeightsType & weights )
{
  if( weights != this->m_OptimizerWeights )
    {
    itkDebugMacro( "setting optimizer weights to " << weights );

    this->m_OptimizerWeights = weights;

    // Check to see if optimizer weights are identity to avoid unnecessary
    // computations.

    this->m_OptimizerWeightsAreIdentity = true;
    if( this->m_OptimizerWeights.Size() > 0 )
      {
      typedef typename OptimizerWeightsType::ValueType OptimizerWeightsValueType;
      OptimizerWeightsValueType tolerance = static_cast<OptimizerWeightsValueType>( 1e-4 );

      for( unsigned int i = 0; i < this->m_OptimizerWeights.Size(); i++ )
        {
        OptimizerWeightsValueType difference =
          std::fabs( NumericTraits<OptimizerWeightsValueType>::OneValue() - this->m_OptimizerWeights[i] );
        if( difference > tolerance  )
          {
          this->m_OptimizerWeightsAreIdentity = false;
          break;
          }
        }
      }
    this->Modified();
    }
}

/*
 * Initialize by setting the interconnects between components.
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::InitializeRegistrationAtEachLevel( const SizeValueType level )
{
  // Sanity checks

  if( this->m_NumberOfFixedImages != this->m_NumberOfMovingImages )
    {
    itkExceptionMacro( "The number of fixed and moving images is not equal." );
    }

  SizeValueType numberOfImagePairs = static_cast<unsigned int>( 0.5 * this->GetNumberOfIndexedInputs() );

  if( numberOfImagePairs == 0 )
    {
    itkExceptionMacro( "There are no input images." );
    }

  if( numberOfImagePairs > 1 )
    {
    // If more than one image pair is set, we assume that the multi-metric
    // is being used.  We check to see if the number of image pairs is equal
    // to the number of metrics in the multi-metric.

    typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
    if( multiMetric )
      {
      SizeValueType numberOfMetrics = multiMetric->GetNumberOfMetrics();
      if( numberOfMetrics != numberOfImagePairs )
        {
        itkExceptionMacro( "Mismatch between number of image pairs and the number of metrics." );
        }
      }
    else
      {
      itkExceptionMacro( "There is more than one image pair.  Need to use a MultiMetricType." );
      }
    }

  if ( !this->m_Optimizer )
    {
    itkExceptionMacro( "The optimizer is not present." );
    }
  if ( !this->m_Metric )
    {
    itkExceptionMacro( "The image metric is not present." );
    }

  InitialTransformType* movingInitialTransform = const_cast<InitialTransformType*>(this->GetMovingInitialTransform());
  InitialTransformType* fixedInitialTransform = const_cast<InitialTransformType*>(this->GetFixedInitialTransform());

  this->m_CurrentIteration = 0;
  this->m_CurrentMetricValue = 0.0;
  this->m_CurrentConvergenceValue = 0.0;
  this->m_IsConverged = false;

  this->InvokeEvent( InitializeEvent() );

  // For each level, we adapt the current transform.  For many transforms, e.g.
  // affine, the base transform adaptor does not do anything.  However, in the
  // case of other transforms, e.g. the b-spline and displacement field transforms
  // the fixed parameters are changed to reflect an increase in transform resolution.
  // This could involve increasing the mesh size of the B-spline transform or
  // increase the resolution of the displacement field.

  if( this->m_TransformParametersAdaptorsPerLevel[level] )
    {
    this->m_TransformParametersAdaptorsPerLevel[level]->SetTransform( this->m_OutputTransform );
    this->m_TransformParametersAdaptorsPerLevel[level]->AdaptTransformParameters();
    }

  // Set-up the composite transform at initialization
  if( level == 0 )
    {
    this->m_CompositeTransform->ClearTransformQueue();

    // Since we cannot instantiate a null object from an abstract class, we need to initialize the moving
    // initial transform as an identity transform.
    // Nevertheless, we do not need add this transform to the composite transform when it is only an
    // identity transform. Simply by not setting that, we can save lots of time in jacobian computations
    // of the composite transform since we can avoid some matrix multiplications.

    // Skip adding an IdentityTransform to the m_CompositeTransform
    if( movingInitialTransform != ITK_NULLPTR &&
      std::string(movingInitialTransform->GetNameOfClass() ) != std::string("IdentityTransform") )
      {
      this->m_CompositeTransform->AddTransform( movingInitialTransform );
      }

    this->m_CompositeTransform->AddTransform( this->m_OutputTransform );
    // If the moving initial transform is a composite transform, unroll
    // it into m_CompositeTransform.
    this->m_CompositeTransform->FlattenTransformQueue();

    if( this->m_OptimizerWeights.Size() > 0 )
      {
      this->m_Optimizer->SetWeights( this->m_OptimizerWeights );
      }
    }
  this->m_CompositeTransform->SetOnlyMostRecentTransformToOptimizeOn();

  // At each resolution and for each image pair, we can
  //   1. subsample the reference domain (typically the fixed image) and/or
  //   2. smooth the fixed and moving images.

  typename ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
  shrinkFilter->SetShrinkFactors( this->m_ShrinkFactorsPerLevel[level] );
  shrinkFilter->SetInput( this->GetFixedImage( 0 ) );
  shrinkFilter->Update();

  typename MultiMetricType::Pointer multiMetric2 = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
  if( multiMetric2 )
    {
    if ( fixedInitialTransform )
      {
      multiMetric2->SetFixedTransform( fixedInitialTransform );
      }
    else
      {
      typedef IdentityTransform<RealType, ImageDimension> IdentityTransformType;
      typename IdentityTransformType::Pointer defaultFixedInitialTransform = IdentityTransformType::New();
      multiMetric2->SetFixedTransform( defaultFixedInitialTransform );
      }
    multiMetric2->SetMovingTransform( this->m_CompositeTransform );
    multiMetric2->SetVirtualDomainFromImage( shrinkFilter->GetOutput() );
    for( unsigned int n = 0; n < multiMetric2->GetNumberOfMetrics(); n++ )
      {
      typename ImageMetricType::Pointer imageMetric = dynamic_cast<ImageMetricType *>( multiMetric2->GetMetricQueue()[n].GetPointer() );
      if( imageMetric.IsNotNull() )
        {
        imageMetric->SetVirtualDomainFromImage( shrinkFilter->GetOutput() );
        }
      else
        {
        itkExceptionMacro("ERROR: Invalid metric conversion.");
        }
      }
    }
  else
    {
    typename ImageMetricType::Pointer imageMetric = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() );
    if( imageMetric.IsNotNull() )
      {
      if ( fixedInitialTransform )
        {
         imageMetric->SetFixedTransform( fixedInitialTransform );
        }
      else
        {
        typedef IdentityTransform<RealType, ImageDimension> IdentityTransformType;
        typename IdentityTransformType::Pointer defaultFixedInitialTransform = IdentityTransformType::New();
        imageMetric->SetFixedTransform( defaultFixedInitialTransform );
        }
      imageMetric->SetMovingTransform( this->m_CompositeTransform );
      imageMetric->SetVirtualDomainFromImage( shrinkFilter->GetOutput() );
      }
    else
      {
      itkExceptionMacro("ERROR: Invalid metric conversion.");
      }
    }

  this->m_FixedSmoothImages.clear();
  this->m_MovingSmoothImages.clear();

  for( unsigned int n = 0; n < numberOfImagePairs; n++ )
    {
    typedef DiscreteGaussianImageFilter<FixedImageType, FixedImageType> FixedImageSmoothingFilterType;
    typename FixedImageSmoothingFilterType::Pointer fixedImageSmoothingFilter = FixedImageSmoothingFilterType::New();
    if( this->m_SmoothingSigmasAreSpecifiedInPhysicalUnits == true )
      {
      fixedImageSmoothingFilter->SetUseImageSpacingOn();
      }
    else
      {
      fixedImageSmoothingFilter->SetUseImageSpacingOff();
      }
    fixedImageSmoothingFilter->SetVariance( vnl_math_sqr( this->m_SmoothingSigmasPerLevel[level] ) );
    fixedImageSmoothingFilter->SetMaximumError( 0.01 );
    fixedImageSmoothingFilter->SetInput( this->GetFixedImage( n ) );

    this->m_FixedSmoothImages.push_back( fixedImageSmoothingFilter->GetOutput() );
    this->m_FixedSmoothImages[n]->Update();
    this->m_FixedSmoothImages[n]->DisconnectPipeline();

    typedef DiscreteGaussianImageFilter<MovingImageType, MovingImageType> MovingImageSmoothingFilterType;
    typename MovingImageSmoothingFilterType::Pointer movingImageSmoothingFilter = MovingImageSmoothingFilterType::New();
    if( this->m_SmoothingSigmasAreSpecifiedInPhysicalUnits == true )
      {
      movingImageSmoothingFilter->SetUseImageSpacingOn();
      }
    else
      {
      movingImageSmoothingFilter->SetUseImageSpacingOff();
      }
    movingImageSmoothingFilter->SetVariance( vnl_math_sqr( this->m_SmoothingSigmasPerLevel[level] ) );
    movingImageSmoothingFilter->SetMaximumError( 0.01 );
    movingImageSmoothingFilter->SetInput( this->GetMovingImage( n ) );

    this->m_MovingSmoothImages.push_back( movingImageSmoothingFilter->GetOutput() );
    this->m_MovingSmoothImages[n]->Update();
    this->m_MovingSmoothImages[n]->DisconnectPipeline();

    // Update the image metric

    typename MultiMetricType::Pointer multiMetric3 = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
    if( multiMetric3 )
      {
      typename ImageMetricType::Pointer metricQueue = dynamic_cast<ImageMetricType *>( multiMetric3->GetMetricQueue()[n].GetPointer() );
      if( metricQueue.IsNotNull() )
        {
        metricQueue->SetFixedImage( this->m_FixedSmoothImages[n] );
        metricQueue->SetMovingImage( this->m_MovingSmoothImages[n] );
        }
      else
        {
        itkExceptionMacro("ERROR: Invalid conversion from the multi metric queue.");
        }
      }
    else
      {
      typename ImageMetricType::Pointer metric = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() );
      if( metric.IsNotNull() )
        {
        metric->SetFixedImage( this->m_FixedSmoothImages[n] );
        metric->SetMovingImage( this->m_MovingSmoothImages[n] );
        }
      else
        {
        itkExceptionMacro("ERROR: Invalid metric conversion.");
        }
      }
    }

  if( this->m_MetricSamplingStrategy != NONE )
    {
    this->SetMetricSamplePoints();
    }

  // Update the optimizer

  this->m_Optimizer->SetMetric( this->m_Metric );

  if( ( this->m_Optimizer->GetScales() ).Size() != this->m_OutputTransform->GetNumberOfLocalParameters() )
    {
    typedef typename OptimizerType::ScalesType ScalesType;
    ScalesType scales;
    scales.SetSize( this->m_OutputTransform->GetNumberOfLocalParameters() );
    scales.Fill( NumericTraits<typename ScalesType::ValueType>::OneValue() );
    this->m_Optimizer->SetScales( scales );
    }
}


template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::AllocateOutputs()
{
  const DecoratedInitialTransformType * decoratedInitialTransform = this->GetInitialTransformInput();
  DecoratedOutputTransformType *decoratedOutputTransform = this->GetOutput();

  if ( decoratedInitialTransform )
    {
    const InitialTransformType * initialTransform = decoratedInitialTransform->Get();

    if ( initialTransform )
      {
      if ( this->GetInPlace() )
        {
        // graft the input to the output which may fail if the types
        // aren't compatible.
        decoratedOutputTransform->Graft( decoratedInitialTransform );

        if ( decoratedOutputTransform->Get() )
          {
          this->m_OutputTransform = decoratedOutputTransform->GetModifiable();

          // This is generally done in the ReleaseInputs methods,
          // however we do not need it again
          const_cast<DecoratedInitialTransformType *>(decoratedInitialTransform)->ReleaseData();

          // successful in-place grafting
          itkDebugMacro("inplace allocation of output transform");
          return;
          }
        }

      const OutputTransformType * initialAsOutputTransform = dynamic_cast<const OutputTransformType*>( initialTransform );

      if ( initialAsOutputTransform )
        {
        // Clone performs a deep copy of the parameters and composition
        this->m_OutputTransform = initialAsOutputTransform->Clone();
        decoratedOutputTransform->Set(this->m_OutputTransform);

        // successful deep copy from initial to output
        itkDebugMacro("clone copy allocation of output transform");
        return;
        }
      else
        {
        itkExceptionMacro("Unable to convert InitialTransform input to the OutputTransform type");
        }

      }
    }

  // fallback allocation and initialization


  // initialize to identity? what happens if we re-run with optimized values?
  itkDebugMacro("fallback allocation of output transform");

  if ( !decoratedOutputTransform->Get() )
    {
    // the output decorated component is null, allocate
    OutputTransformPointer ptr;
    Self::MakeOutputTransform(ptr);
    decoratedOutputTransform->Set(ptr);
    }

  this->m_OutputTransform = this->GetModifiableTransform();

}


/*
 * Start the registration
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::GenerateData()
{
  this->AllocateOutputs();
  for( this->m_CurrentLevel = 0; this->m_CurrentLevel < this->m_NumberOfLevels; this->m_CurrentLevel++ )
    {
    this->InitializeRegistrationAtEachLevel( this->m_CurrentLevel );

    this->m_Metric->Initialize();

    this->m_Optimizer->StartOptimization();
    }
}

/**
 * Set the moving transform adaptors per stage
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
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
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
const typename  ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>::TransformParametersAdaptorsContainerType &
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::GetTransformParametersAdaptorsPerLevel() const
{
  return this->m_TransformParametersAdaptorsPerLevel;
}

/**
 * Set the number of levels
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
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
      this->m_TransformParametersAdaptorsPerLevel.push_back( ITK_NULLPTR );
      }

    for( unsigned int level = 0; level < this->m_NumberOfLevels; ++level )
      {
      ShrinkFactorsPerDimensionContainerType shrinkFactors;
      shrinkFactors.Fill( 1 );
      this->SetShrinkFactorsPerDimension( level, shrinkFactors );
      }

    this->m_SmoothingSigmasPerLevel.SetSize( this->m_NumberOfLevels );
    this->m_SmoothingSigmasPerLevel.Fill( 1.0 );

    this->m_MetricSamplingPercentagePerLevel.SetSize( this->m_NumberOfLevels );
    this->m_MetricSamplingPercentagePerLevel.Fill( 1.0 );

    this->Modified();
    }
}

/**
 * Get the metric samples
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::SetMetricSamplePoints()
{
  SizeValueType numberOfLocalMetrics = 1;

  typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
  if( multiMetric )
    {
    numberOfLocalMetrics = multiMetric->GetNumberOfMetrics();
    }

  typedef typename ImageMetricType::VirtualImageType    VirtualDomainImageType;
  typedef typename VirtualDomainImageType::RegionType   VirtualDomainRegionType;
  const VirtualDomainImageType * virtualImage;
  if( numberOfLocalMetrics == 1 )
    {
    typename ImageMetricType::Pointer metric = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() );
    if( metric.IsNotNull() )
      {
      virtualImage = metric->GetVirtualImage();
      }
    else
      {
      itkExceptionMacro("ERROR: Invalid metric conversion.");
      }
    }
  else
    {
    typename ImageMetricType::Pointer multimetric = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[0].GetPointer() );
    if( multimetric.IsNotNull() )
      {
      virtualImage = multimetric->GetVirtualImage();
      }
    else
      {
      itkExceptionMacro("ERROR: Invalid metric conversion.");
      }
    }
  const VirtualDomainRegionType & virtualDomainRegion = virtualImage->GetRequestedRegion();
  const typename VirtualDomainImageType::SpacingType oneThirdVirtualSpacing = virtualImage->GetSpacing() / 3.0;

  for( unsigned int n = 0; n < numberOfLocalMetrics; n++ )
    {
    typename MetricSamplePointSetType::Pointer samplePointSet = MetricSamplePointSetType::New();
    samplePointSet->Initialize();

    typedef typename MetricSamplePointSetType::PointType SamplePointType;

    typedef typename Statistics::MersenneTwisterRandomVariateGenerator RandomizerType;
    typename RandomizerType::Pointer randomizer = RandomizerType::New();
    randomizer->SetSeed( 1234 );

    unsigned long index = 0;

    switch( this->m_MetricSamplingStrategy )
      {
      case REGULAR:
        {
        const unsigned long sampleCount = static_cast<unsigned long>( std::ceil( 1.0 / this->m_MetricSamplingPercentagePerLevel[this->m_CurrentLevel] ) );
        unsigned long count = sampleCount; //Start at sampleCount to keep behavior backwards identical, using first element.
        ImageRegionConstIteratorWithIndex<VirtualDomainImageType> It( virtualImage, virtualDomainRegion );
        for( It.GoToBegin(); !It.IsAtEnd(); ++It )
          {
          if( count == sampleCount )
            {
            count=0; //Reset counter
            SamplePointType point;
            virtualImage->TransformIndexToPhysicalPoint( It.GetIndex(), point );

            // randomly perturb the point within a voxel (approximately)
            for( unsigned int d = 0; d < ImageDimension; d++ )
              {
              point[d] += randomizer->GetNormalVariate() * oneThirdVirtualSpacing[d];
              }
            samplePointSet->SetPoint( index, point );
            ++index;
            }
          ++count;
          }
        break;
        }
      case RANDOM:
        {
        const unsigned long totalVirtualDomainVoxels = virtualDomainRegion.GetNumberOfPixels();
        const unsigned long sampleCount = static_cast<unsigned long>( static_cast<float>( totalVirtualDomainVoxels ) * this->m_MetricSamplingPercentagePerLevel[this->m_CurrentLevel] );
        ImageRandomConstIteratorWithIndex<VirtualDomainImageType> ItR( virtualImage, virtualDomainRegion );
        ItR.SetNumberOfSamples( sampleCount );
        for( ItR.GoToBegin(); !ItR.IsAtEnd(); ++ItR )
          {
          SamplePointType point;
          virtualImage->TransformIndexToPhysicalPoint( ItR.GetIndex(), point );

          // randomly perturb the point within a voxel (approximately)
          for ( unsigned int d = 0; d < ImageDimension; d++ )
            {
            point[d] += randomizer->GetNormalVariate() * oneThirdVirtualSpacing[d];
            }
          samplePointSet->SetPoint( index, point );
          ++index;
          }
        break;
        }
      default:
        {
        itkExceptionMacro( "Invalid sampling strategy requested." );
        }
      }

    if( numberOfLocalMetrics == 1 )
      {
      dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetFixedSampledPointSet( samplePointSet );
      dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetUseFixedSampledPointSet( true );
      }
    else
      {
      dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->SetFixedSampledPointSet( samplePointSet );
      dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->SetUseFixedSampledPointSet( true );
      }
    }
}

/*
 * PrintSelf
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  Indent indent2 = indent.GetNextIndent();

  os << indent << "Number of levels = " << this->m_NumberOfLevels << std::endl;

  for( unsigned int level = 0; level < this->m_NumberOfLevels; ++level )
    {
    os << indent << "Shrink factors (level " << level << "): "
       << this->m_ShrinkFactorsPerLevel[level] << std::endl;
    }
  os << indent << "Smoothing sigmas: " << this->m_SmoothingSigmasPerLevel << std::endl;

  if( this->m_SmoothingSigmasAreSpecifiedInPhysicalUnits == true )
    {
    os << indent2 << "Smoothing sigmas are specified in physical units." << std::endl;
    }
  else
    {
    os << indent2 << "Smoothing sigmas are specified in voxel units." << std::endl;
    }

  if( this->m_OptimizerWeights.Size() > 0 )
    {
    os << indent << "Optimizers weights: " << this->m_OptimizerWeights << std::endl;
    }

  os << indent << "Metric sampling strategy: " << this->m_MetricSamplingStrategy << std::endl;

  os << indent << "Metric sampling percentage: ";
  for( unsigned int i = 0; i < this->m_NumberOfLevels; i++ )
    {
    os << this->m_MetricSamplingPercentagePerLevel[i] << " ";
    }
  os << std::endl;

  os << indent << "InPlace: " << ( m_InPlace ? "On" : "Off" ) << std::endl;
}

/*
 *  Get output transform
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>::DecoratedOutputTransformType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::GetOutput()
{
  return static_cast<DecoratedOutputTransformType *>( this->ProcessObject::GetOutput( 0 ) );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
const typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>::DecoratedOutputTransformType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::GetOutput() const
{
  return static_cast<const DecoratedOutputTransformType *>( this->ProcessObject::GetOutput( 0 ) );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>::OutputTransformType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::GetModifiableTransform()
{
  DecoratedOutputTransformType * temp = this->GetOutput();
  // required outputs of process object should always exits
  itkAssertInDebugAndIgnoreInReleaseMacro( temp != ITK_NULLPTR );
  return temp->GetModifiable();
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
const typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>::OutputTransformType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::GetTransform() const
{
  const  DecoratedOutputTransformType * temp = this->GetOutput();
  // required outputs of process object should always exits
  itkAssertInDebugAndIgnoreInReleaseMacro( temp != ITK_NULLPTR );
  return temp->Get();
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
DataObject::Pointer
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::MakeOutput( DataObjectPointerArraySizeType output )
{
  switch ( output )
    {
    case 0:
      {
      OutputTransformPointer ptr;
      Self::MakeOutputTransform(ptr);
      DecoratedOutputTransformPointer transformDecorator =  DecoratedOutputTransformType::New();
      transformDecorator->Set( ptr );
      return transformDecorator.GetPointer();
      }
    default:
      itkExceptionMacro("MakeOutput request for an output number larger than the expected number of outputs");
      return ITK_NULLPTR;
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage>
::SetMetricSamplingPercentage( const RealType samplingPercentage )
{
  MetricSamplingPercentageArrayType samplingPercentagePerLevel;
  samplingPercentagePerLevel.SetSize( this->m_NumberOfLevels );
  samplingPercentagePerLevel.Fill( samplingPercentage );
  this->SetMetricSamplingPercentagePerLevel( samplingPercentagePerLevel );
}

} // end namespace itk
#endif

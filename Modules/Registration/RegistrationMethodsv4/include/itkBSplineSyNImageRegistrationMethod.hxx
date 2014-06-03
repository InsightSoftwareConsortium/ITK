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
#ifndef __itkBSplineSyNImageRegistrationMethod_hxx
#define __itkBSplineSyNImageRegistrationMethod_hxx

#include "itkBSplineSyNImageRegistrationMethod.h"

#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"
#include "itkComposeDisplacementFieldsImageFilter.h"
#include "itkImportImageFilter.h"
#include "itkInvertDisplacementFieldImageFilter.h"
#include "itkIterationReporter.h"
#include "itkMultiplyImageFilter.h"
#include "itkWindowConvergenceMonitoringFunction.h"

namespace itk
{
/**
 * Constructor
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform>
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform>
::BSplineSyNImageRegistrationMethod()
{
}

template<typename TFixedImage, typename TMovingImage, typename TOutputTransform>
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform>
::~BSplineSyNImageRegistrationMethod()
{
}

template<typename TFixedImage, typename TMovingImage, typename TOutputTransform>
void
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform>
::InitializeRegistrationAtEachLevel( const SizeValueType level )
{
  Superclass::InitializeRegistrationAtEachLevel( level );

  typedef BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<OutputTransformType> BSplineDisplacementFieldTransformAdaptorType;

  if( level == 0 )
    {
    this->m_FixedToMiddleTransform->SetSplineOrder( this->m_OutputTransform->GetSplineOrder() );
    this->m_FixedToMiddleTransform->SetNumberOfControlPointsForTheUpdateField(
      dynamic_cast<BSplineDisplacementFieldTransformAdaptorType *>( this->m_TransformParametersAdaptorsPerLevel[0].GetPointer() )->GetNumberOfControlPointsForTheUpdateField() );
    this->m_FixedToMiddleTransform->SetNumberOfControlPointsForTheTotalField(
      dynamic_cast<BSplineDisplacementFieldTransformAdaptorType *>( this->m_TransformParametersAdaptorsPerLevel[0].GetPointer() )->GetNumberOfControlPointsForTheTotalField() );

    this->m_MovingToMiddleTransform->SetSplineOrder( this->m_OutputTransform->GetSplineOrder() );
    this->m_MovingToMiddleTransform->SetNumberOfControlPointsForTheUpdateField(
      dynamic_cast<BSplineDisplacementFieldTransformAdaptorType *>( this->m_TransformParametersAdaptorsPerLevel[0].GetPointer() )->GetNumberOfControlPointsForTheUpdateField() );
    this->m_MovingToMiddleTransform->SetNumberOfControlPointsForTheTotalField(
      dynamic_cast<BSplineDisplacementFieldTransformAdaptorType *>( this->m_TransformParametersAdaptorsPerLevel[0].GetPointer() )->GetNumberOfControlPointsForTheTotalField() );
    }
}

/*
 * Start the optimization at each level.  We just do a basic gradient descent operation.
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform>
void
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform>
::StartOptimization()
{
  const DisplacementVectorType zeroVector( 0.0 );
  typedef ImageDuplicator<DisplacementFieldType> DisplacementFieldDuplicatorType;

  typename VirtualImageType::ConstPointer virtualDomainImage;
  typename MovingImageMaskType::ConstPointer movingImageMask;
  typename FixedImageMaskType::ConstPointer fixedImageMask;

  typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
  if( multiMetric )
    {
    typename ImageMetricType::Pointer metricQueue = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[0].GetPointer() );
    if( metricQueue.IsNotNull() )
      {
      virtualDomainImage = metricQueue->GetVirtualImage();
      fixedImageMask = metricQueue->GetFixedImageMask();
      movingImageMask = metricQueue->GetMovingImageMask();
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
      virtualDomainImage = metric->GetVirtualImage();
      fixedImageMask = metric->GetFixedImageMask();
      movingImageMask = metric->GetMovingImageMask();
      }
    else
      {
      itkExceptionMacro("ERROR: Invalid metric conversion.");
      }
    }

  InitialTransformType* fixedInitialTransform = const_cast<InitialTransformType*>(this->GetFixedInitialTransform());

  // Monitor the convergence
  typedef itk::Function::WindowConvergenceMonitoringFunction<RealType> ConvergenceMonitoringType;
  typename ConvergenceMonitoringType::Pointer convergenceMonitoring = ConvergenceMonitoringType::New();
  convergenceMonitoring->SetWindowSize( this->m_ConvergenceWindowSize );

  IterationReporter reporter( this, 0, 1 );

  while( this->m_CurrentIteration++ < this->m_NumberOfIterationsPerLevel[this->m_CurrentLevel] && !this->m_IsConverged )
    {
    typename CompositeTransformType::Pointer fixedComposite = CompositeTransformType::New();
    if ( fixedInitialTransform != ITK_NULLPTR )
      {
      fixedComposite->AddTransform( fixedInitialTransform );
      }
    fixedComposite->AddTransform( this->m_FixedToMiddleTransform->GetInverseTransform() );
    fixedComposite->FlattenTransformQueue();
    fixedComposite->SetOnlyMostRecentTransformToOptimizeOn();

    typename CompositeTransformType::Pointer movingComposite = CompositeTransformType::New();
    movingComposite->AddTransform( this->m_CompositeTransform );
    movingComposite->AddTransform( this->m_MovingToMiddleTransform->GetInverseTransform() );
    movingComposite->FlattenTransformQueue();
    movingComposite->SetOnlyMostRecentTransformToOptimizeOn();

    // Compute the update fields (to both moving and fixed images) and smooth

    MeasureType fixedMetricValue = 0.0;
    MeasureType movingMetricValue = 0.0;

    DisplacementFieldPointer fixedToMiddleSmoothUpdateField = this->ComputeUpdateField(
      this->m_FixedSmoothImages, fixedComposite, this->m_MovingSmoothImages, movingComposite, fixedImageMask, movingMetricValue );
    DisplacementFieldPointer movingToMiddleSmoothUpdateField = this->ComputeUpdateField(
      this->m_MovingSmoothImages, movingComposite, this->m_FixedSmoothImages, fixedComposite, movingImageMask, fixedMetricValue );

    if ( this->m_AverageMidPointGradients )
      {
      ImageRegionIteratorWithIndex<DisplacementFieldType> ItF( fixedToMiddleSmoothUpdateField, fixedToMiddleSmoothUpdateField->GetLargestPossibleRegion() );
      for( ItF.GoToBegin(); !ItF.IsAtEnd(); ++ItF )
        {
        ItF.Set( ItF.Get() - movingToMiddleSmoothUpdateField->GetPixel( ItF.GetIndex() ) );
        movingToMiddleSmoothUpdateField->SetPixel( ItF.GetIndex(), -ItF.Get() );
        }
      }

    // Add the update field to both displacement fields (from fixed/moving to middle image) and then smooth

    typedef ComposeDisplacementFieldsImageFilter<DisplacementFieldType> ComposerType;

    typename ComposerType::Pointer fixedComposer = ComposerType::New();
    fixedComposer->SetDisplacementField( fixedToMiddleSmoothUpdateField );
    fixedComposer->SetWarpingField( this->m_FixedToMiddleTransform->GetDisplacementField() );
    fixedComposer->Update();

    DisplacementFieldPointer fixedToMiddleSmoothTotalFieldTmp = this->BSplineSmoothDisplacementField( fixedComposer->GetOutput(),
      this->m_FixedToMiddleTransform->GetNumberOfControlPointsForTheTotalField(), ITK_NULLPTR );

    typename ComposerType::Pointer movingComposer = ComposerType::New();
    movingComposer->SetDisplacementField( movingToMiddleSmoothUpdateField );
    movingComposer->SetWarpingField( this->m_MovingToMiddleTransform->GetDisplacementField() );
    movingComposer->Update();

    DisplacementFieldPointer movingToMiddleSmoothTotalFieldTmp = this->BSplineSmoothDisplacementField( movingComposer->GetOutput(),
      this->m_MovingToMiddleTransform->GetNumberOfControlPointsForTheTotalField(), ITK_NULLPTR );

    // Iteratively estimate the inverse fields.

    DisplacementFieldPointer fixedToMiddleSmoothTotalFieldInverse = this->InvertDisplacementField( fixedToMiddleSmoothTotalFieldTmp, this->m_FixedToMiddleTransform->GetInverseDisplacementField() );
    DisplacementFieldPointer fixedToMiddleSmoothTotalField = this->InvertDisplacementField( fixedToMiddleSmoothTotalFieldInverse, fixedToMiddleSmoothTotalFieldTmp );

    DisplacementFieldPointer movingToMiddleSmoothTotalFieldInverse = this->InvertDisplacementField( movingToMiddleSmoothTotalFieldTmp, this->m_MovingToMiddleTransform->GetInverseDisplacementField() );
    DisplacementFieldPointer movingToMiddleSmoothTotalField = this->InvertDisplacementField( movingToMiddleSmoothTotalFieldInverse, movingToMiddleSmoothTotalFieldTmp );

    // Assign the displacement fields and their inverses to the proper transforms.
    this->m_FixedToMiddleTransform->SetDisplacementField( fixedToMiddleSmoothTotalField );
    this->m_FixedToMiddleTransform->SetInverseDisplacementField( fixedToMiddleSmoothTotalFieldInverse );

    this->m_MovingToMiddleTransform->SetDisplacementField( movingToMiddleSmoothTotalField );
    this->m_MovingToMiddleTransform->SetInverseDisplacementField( movingToMiddleSmoothTotalFieldInverse );

    this->m_CurrentMetricValue = 0.5 * ( movingMetricValue + fixedMetricValue );

    convergenceMonitoring->AddEnergyValue( this->m_CurrentMetricValue );
    this->m_CurrentConvergenceValue = convergenceMonitoring->GetConvergenceValue();

    if( this->m_CurrentConvergenceValue < this->m_ConvergenceThreshold )
      {
      this->m_IsConverged = true;
      }
    reporter.CompletedStep();
    }
}

template<typename TFixedImage, typename TMovingImage, typename TOutputTransform>
typename BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform>::DisplacementFieldPointer
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform>
::ComputeUpdateField( const FixedImagesContainerType fixedImages, const TransformBaseType * fixedTransform, const MovingImagesContainerType movingImages,
  const TransformBaseType * movingTransform, const FixedImageMaskType * mask, MeasureType & value )
{
  // pre calculate the voxel distance to be used in properly scaling the gradient.

  typename VirtualImageType::ConstPointer virtualDomainImage;
  typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
  if( multiMetric )
    {
    virtualDomainImage = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[0].GetPointer() )->GetVirtualImage();
    }
  else
    {
    virtualDomainImage = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->GetVirtualImage();
    }

  if( !this->m_DownsampleImagesForMetricDerivatives )
    {
    if( multiMetric )
      {
      for( unsigned int n = 0; n < multiMetric->GetNumberOfMetrics(); n++ )
        {
        typename ImageMetricType::Pointer metricQueue = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() );
        if( metricQueue.IsNotNull() )
          {
          metricQueue->SetFixedImage( fixedImages[n] );
          metricQueue->SetMovingImage( movingImages[n] );
          }
        else
          {
          itkExceptionMacro("ERROR: Invalid conversion from the multi metric queue.");
          }
        }
      multiMetric->SetFixedTransform( const_cast<TransformBaseType *>( fixedTransform ) );
      multiMetric->SetMovingTransform( const_cast<TransformBaseType *>( movingTransform ) );
      }
    else
      {
      typename ImageMetricType::Pointer metric = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() );
      if( metric.IsNotNull() )
        {
        metric->SetFixedImage( fixedImages[0] );
        metric->SetFixedTransform( const_cast<TransformBaseType *>( fixedTransform ) );
        metric->SetMovingImage( movingImages[0] );
        metric->SetMovingTransform( const_cast<TransformBaseType *>( movingTransform ) );
        }
      else
        {
        itkExceptionMacro("ERROR: Invalid metric conversion.");
        }
      }
    }
  else
    {
    for( unsigned int n = 0; n < this->m_MovingSmoothImages.size(); n++ )
      {
      typedef ResampleImageFilter<MovingImageType, MovingImageType, typename TOutputTransform::ScalarType> MovingResamplerType;
      typename MovingResamplerType::Pointer movingResampler = MovingResamplerType::New();
      movingResampler->SetTransform( movingTransform );
      movingResampler->SetInput( movingImages[n] );
      movingResampler->SetSize( virtualDomainImage->GetRequestedRegion().GetSize() );
      movingResampler->SetOutputOrigin( virtualDomainImage->GetOrigin() );
      movingResampler->SetOutputSpacing( virtualDomainImage->GetSpacing() );
      movingResampler->SetOutputDirection( virtualDomainImage->GetDirection() );
      movingResampler->SetDefaultPixelValue( 0 );
      movingResampler->Update();

      typedef ResampleImageFilter<FixedImageType, FixedImageType, typename TOutputTransform::ScalarType> FixedResamplerType;
      typename FixedResamplerType::Pointer fixedResampler = FixedResamplerType::New();
      fixedResampler->SetTransform( fixedTransform );
      fixedResampler->SetInput( fixedImages[n] );
      fixedResampler->SetSize( virtualDomainImage->GetRequestedRegion().GetSize() );
      fixedResampler->SetOutputOrigin( virtualDomainImage->GetOrigin() );
      fixedResampler->SetOutputSpacing( virtualDomainImage->GetSpacing() );
      fixedResampler->SetOutputDirection( virtualDomainImage->GetDirection() );
      fixedResampler->SetDefaultPixelValue( 0 );
      fixedResampler->Update();

      if( multiMetric )
        {
        typename ImageMetricType::Pointer metricQueue = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() );
        if( metricQueue.IsNotNull() )
          {
          metricQueue->SetMovingImage( movingResampler->GetOutput() );
          metricQueue->SetFixedImage( fixedResampler->GetOutput() );
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
          metric->SetMovingImage( movingResampler->GetOutput() );
          metric->SetFixedImage( fixedResampler->GetOutput() );
          }
        else
          {
          itkExceptionMacro("ERROR: Invalid metric conversion.");
          }
        }
      }

    const DisplacementVectorType zeroVector( 0.0 );

    typename DisplacementFieldType::Pointer identityField = DisplacementFieldType::New();
    identityField->CopyInformation( virtualDomainImage );
    identityField->SetRegions( virtualDomainImage->GetRequestedRegion() );
    identityField->Allocate();
    identityField->FillBuffer( zeroVector );

    typedef DisplacementFieldTransform<RealType, ImageDimension> DisplacementFieldTransformType;
    typename DisplacementFieldTransformType::Pointer identityDisplacementFieldTransform = DisplacementFieldTransformType::New();
    identityDisplacementFieldTransform->SetDisplacementField( identityField );

    if( multiMetric )
      {
      multiMetric->SetFixedTransform( identityDisplacementFieldTransform );
      multiMetric->SetMovingTransform( identityDisplacementFieldTransform );
      }
    else
      {
      dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetFixedTransform( identityDisplacementFieldTransform );
      dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetMovingTransform( identityDisplacementFieldTransform );
      }
    }
  this->m_Metric->Initialize();

  typedef typename ImageMetricType::DerivativeType MetricDerivativeType;
  const typename MetricDerivativeType::SizeValueType metricDerivativeSize = virtualDomainImage->GetLargestPossibleRegion().GetNumberOfPixels() * ImageDimension;
  MetricDerivativeType metricDerivative( metricDerivativeSize );

  metricDerivative.Fill( NumericTraits<typename MetricDerivativeType::ValueType>::Zero );
  this->m_Metric->GetValueAndDerivative( value, metricDerivative );

  // Ensure that the size of the optimizer weights is the same as the
  // number of local transform parameters (=ImageDimension)

  if( !this->m_OptimizerWeightsAreIdentity && this->m_OptimizerWeights.Size() == ImageDimension )
    {
    typename MetricDerivativeType::iterator it;
    for( it = metricDerivative.begin(); it != metricDerivative.end(); it += ImageDimension )
      {
      for( unsigned int d = 0; d < ImageDimension; d++ )
        {
        *(it + d) *= this->m_OptimizerWeights[d];
        }
      }
    }

  // we rescale the update velocity field at each time point.
  // we first need to convert to a displacement field to look
  // at the max norm of the field.

  const SizeValueType numberOfPixels = static_cast<SizeValueType>( metricDerivative.Size() / ImageDimension );
  const bool importFilterWillReleaseMemory = false;

  // Brad L. says I should feel bad about using a reinterpret_cast.  I do feel bad.

  DisplacementVectorType *metricDerivativeFieldPointer = reinterpret_cast<DisplacementVectorType *>( metricDerivative.data_block() );

  typedef ImportImageFilter<DisplacementVectorType, ImageDimension> ImporterType;
  typename ImporterType::Pointer importer = ImporterType::New();
  importer->SetImportPointer( metricDerivativeFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
  importer->SetRegion( virtualDomainImage->GetBufferedRegion() );
  importer->SetOrigin( virtualDomainImage->GetOrigin() );
  importer->SetSpacing( virtualDomainImage->GetSpacing() );
  importer->SetDirection( virtualDomainImage->GetDirection() );
  importer->Update();

  typename WeightedMaskImageType::Pointer weightedMask = ITK_NULLPTR;

  if( mask )
    {
      // Before using virtualDomainImage as the reference image, it should be cast to the WeightedMaskImageType that always has a type of double.
    typedef itk::CastImageFilter<VirtualImageType, WeightedMaskImageType> CastFilterType;
    typename CastFilterType::Pointer castfilter = CastFilterType::New();
    castfilter->SetInput(virtualDomainImage);
    castfilter->Update();

    typedef ResampleImageFilter<MaskImageType, WeightedMaskImageType, typename TOutputTransform::ScalarType> MaskResamplerType;
    typename MaskResamplerType::Pointer maskResampler = MaskResamplerType::New();
    maskResampler->SetTransform( fixedTransform );
    maskResampler->SetInput( dynamic_cast<ImageMaskSpatialObjectType *>( const_cast<FixedImageMaskType *>( mask ) )->GetImage() );
    maskResampler->UseReferenceImageOn();
    maskResampler->SetReferenceImage( castfilter->GetOutput() );
    maskResampler->SetSize( virtualDomainImage->GetBufferedRegion().GetSize() );
    maskResampler->SetDefaultPixelValue( 0 );

    weightedMask = maskResampler->GetOutput();
    weightedMask->Update();
    weightedMask->DisconnectPipeline();
    }

  DisplacementFieldPointer updateField = this->BSplineSmoothDisplacementField( importer->GetOutput(),
    this->m_FixedToMiddleTransform->GetNumberOfControlPointsForTheUpdateField(), weightedMask );

  typename DisplacementFieldType::SpacingType spacing = updateField->GetSpacing();
  ImageRegionConstIterator<DisplacementFieldType> ItF( updateField, updateField->GetLargestPossibleRegion() );

  RealType maxNorm = NumericTraits<RealType>::NonpositiveMin();
  for( ItF.GoToBegin(); !ItF.IsAtEnd(); ++ItF )
    {
    DisplacementVectorType vector = ItF.Get();

    RealType localNorm = 0;
    for( unsigned int d = 0; d < ImageDimension; d++ )
      {
      localNorm += vnl_math_sqr( vector[d] / spacing[d] );
      }
    localNorm = std::sqrt( localNorm );

    if( localNorm > maxNorm )
      {
      maxNorm = localNorm;
      }
    }

  RealType scale = this->m_LearningRate / maxNorm;

  typedef Image<RealType, ImageDimension> RealImageType;

  typedef MultiplyImageFilter<DisplacementFieldType, RealImageType, DisplacementFieldType> MultiplierType;
  typename MultiplierType::Pointer multiplier = MultiplierType::New();
  multiplier->SetInput( updateField );
  multiplier->SetConstant( scale );

  typename DisplacementFieldType::Pointer scaledUpdateField = multiplier->GetOutput();
  scaledUpdateField->Update();
  scaledUpdateField->DisconnectPipeline();

  return scaledUpdateField;
}

template<typename TFixedImage, typename TMovingImage, typename TOutputTransform>
typename BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform>::DisplacementFieldPointer
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform>
::BSplineSmoothDisplacementField( const DisplacementFieldType * field, const ArrayType & numberOfControlPoints, const WeightedMaskImageType * mask )
{
  typedef ImageDuplicator<DisplacementFieldType> DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( field );
  duplicator->Update();

  DisplacementFieldPointer smoothField = duplicator->GetModifiableOutput();

  for( unsigned int d = 0; d < numberOfControlPoints.Size(); d++ )
    {
    if( numberOfControlPoints[d] <= 0 )
      {
      return smoothField;
      }
    }

  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetDisplacementField( field );
  if( mask )
    {
    bspliner->SetConfidenceImage( mask );
    }
  bspliner->SetNumberOfControlPoints( numberOfControlPoints );
  bspliner->SetSplineOrder( this->m_FixedToMiddleTransform->GetSplineOrder() );
  bspliner->SetNumberOfFittingLevels( 1 );
  bspliner->SetEnforceStationaryBoundary( true );
  bspliner->SetEstimateInverse( false );
  bspliner->Update();

  smoothField = bspliner->GetOutput();

  return smoothField;
}

} // end namespace itk

#endif

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
  typedef DisplacementFieldTransform<RealType, ImageDimension> DisplacementFieldTransformType;
  const DisplacementVectorType zeroVector( 0.0 );
  typedef ImageDuplicator<DisplacementFieldType> DisplacementFieldDuplicatorType;
  typename VirtualImageType::ConstPointer virtualDomainImage = this->m_Metric->GetVirtualDomainImage();

  // Monitor the convergence
  typedef itk::Function::WindowConvergenceMonitoringFunction<double> ConvergenceMonitoringType;
  ConvergenceMonitoringType::Pointer convergenceMonitoring = ConvergenceMonitoringType::New();
  convergenceMonitoring->SetWindowSize( 10 );

  typedef IdentityTransform<RealType, ImageDimension> IdentityTransformType;
  typename IdentityTransformType::Pointer identityTransform;
  identityTransform = IdentityTransformType::New();

  SizeValueType iteration = 0;
  bool isConverged = false;
  while( iteration++ < this->m_NumberOfIterationsPerLevel[this->m_CurrentLevel] && !isConverged )
    {
    std::cout << "    Iteration " << iteration << std::flush;

    typename CompositeTransformType::Pointer fixedComposite = CompositeTransformType::New();
    fixedComposite->AddTransform( this->m_FixedInitialTransform );
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

    DisplacementFieldPointer fixedToMiddleSmoothUpdateField;
    DisplacementFieldPointer movingToMiddleSmoothUpdateField;
    if( this->m_DownsampleImagesForMetricDerivatives )
      {
      typedef ResampleImageFilter<MovingImageType, MovingImageType> MovingResamplerType;
      typename MovingResamplerType::Pointer movingResampler = MovingResamplerType::New();
      movingResampler->SetTransform( movingComposite );
      movingResampler->SetInput( this->m_MovingSmoothImage );
      movingResampler->SetSize( virtualDomainImage->GetRequestedRegion().GetSize() );
      movingResampler->SetOutputOrigin( virtualDomainImage->GetOrigin() );
      movingResampler->SetOutputSpacing( virtualDomainImage->GetSpacing() );
      movingResampler->SetOutputDirection( virtualDomainImage->GetDirection() );
      movingResampler->SetDefaultPixelValue( 0 );
      movingResampler->Update();

      typedef ResampleImageFilter<FixedImageType, FixedImageType> FixedResamplerType;
      typename FixedResamplerType::Pointer fixedResampler = FixedResamplerType::New();
      fixedResampler->SetTransform( fixedComposite );
      fixedResampler->SetInput( this->m_FixedSmoothImage );
      fixedResampler->SetSize( virtualDomainImage->GetRequestedRegion().GetSize() );
      fixedResampler->SetOutputOrigin( virtualDomainImage->GetOrigin() );
      fixedResampler->SetOutputSpacing( virtualDomainImage->GetSpacing() );
      fixedResampler->SetOutputDirection( virtualDomainImage->GetDirection() );
      fixedResampler->SetDefaultPixelValue( 0 );
      fixedResampler->Update();

      typename DisplacementFieldType::Pointer identityField = DisplacementFieldType::New();
      identityField->CopyInformation( virtualDomainImage );
      identityField->SetRegions( virtualDomainImage->GetRequestedRegion() );
      identityField->Allocate();
      identityField->FillBuffer( zeroVector );

      typename DisplacementFieldTransformType::Pointer identityDisplacementFieldTransform = DisplacementFieldTransformType::New();
      identityDisplacementFieldTransform->SetDisplacementField( identityField );

      fixedToMiddleSmoothUpdateField = this->ComputeUpdateField( fixedResampler->GetOutput(), identityTransform,
        movingResampler->GetOutput(), identityDisplacementFieldTransform, movingMetricValue );
      movingToMiddleSmoothUpdateField = this->ComputeUpdateField( movingResampler->GetOutput(), identityTransform,
        fixedResampler->GetOutput(), identityDisplacementFieldTransform, fixedMetricValue );
      }
    else
      {
      fixedToMiddleSmoothUpdateField = this->ComputeUpdateField( this->m_FixedSmoothImage, fixedComposite,
        this->m_MovingSmoothImage, movingComposite, movingMetricValue );
      movingToMiddleSmoothUpdateField = this->ComputeUpdateField( this->m_MovingSmoothImage, movingComposite,
        this->m_FixedSmoothImage, fixedComposite, fixedMetricValue );
      }
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
      this->m_FixedToMiddleTransform->GetNumberOfControlPointsForTheTotalField() );

    typename ComposerType::Pointer movingComposer = ComposerType::New();
    movingComposer->SetDisplacementField( movingToMiddleSmoothUpdateField );
    movingComposer->SetWarpingField( this->m_MovingToMiddleTransform->GetDisplacementField() );
    movingComposer->Update();

    DisplacementFieldPointer movingToMiddleSmoothTotalFieldTmp = this->BSplineSmoothDisplacementField( movingComposer->GetOutput(),
      this->m_MovingToMiddleTransform->GetNumberOfControlPointsForTheTotalField() );

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

    RealType metricValue = 0.5 * ( movingMetricValue + fixedMetricValue );

    convergenceMonitoring->AddEnergyValue( metricValue );
    RealType convergenceValue = convergenceMonitoring->GetConvergenceValue();
    std::cout << ": metric value = " << metricValue << ", convergence value = " << convergenceValue << std::endl;
    if( convergenceValue < this->m_ConvergenceThreshold )
      {
      isConverged = true;
      }
    }
}

template<typename TFixedImage, typename TMovingImage, typename TOutputTransform>
typename BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform>::DisplacementFieldPointer
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform>
::ComputeUpdateField( const FixedImageType * fixedImage, const TransformBaseType * fixedTransform, const MovingImageType * movingImage, const TransformBaseType * movingTransform, MeasureType & value )
{
  typename VirtualImageType::ConstPointer virtualDomainImage = this->m_Metric->GetVirtualDomainImage();

  // pre calculate the voxel distance to be used in properly scaling the gradient.

  this->m_Metric->SetFixedImage( fixedImage );
  this->m_Metric->SetFixedTransform( const_cast<TransformBaseType *>( fixedTransform ) );
  this->m_Metric->SetMovingImage( movingImage );
  this->m_Metric->SetMovingTransform( const_cast<TransformBaseType *>( movingTransform ) );
  this->m_Metric->Initialize();

  typedef typename MetricType::DerivativeType MetricDerivativeType;
  const typename MetricDerivativeType::SizeValueType metricDerivativeSize = virtualDomainImage->GetLargestPossibleRegion().GetNumberOfPixels() * ImageDimension;
  MetricDerivativeType metricDerivative( metricDerivativeSize );

  metricDerivative.Fill( NumericTraits<typename MetricDerivativeType::ValueType>::Zero );
  this->m_Metric->GetValueAndDerivative( value, metricDerivative );

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

  DisplacementFieldPointer updateField = this->BSplineSmoothDisplacementField( importer->GetOutput(), this->m_FixedToMiddleTransform->GetNumberOfControlPointsForTheUpdateField() );

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
    localNorm = vcl_sqrt( localNorm );

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
::BSplineSmoothDisplacementField( const DisplacementFieldType * field, const ArrayType & numberOfControlPoints )
{
  typedef ImageDuplicator<DisplacementFieldType> DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( field );
  duplicator->Update();

  DisplacementFieldPointer smoothField = duplicator->GetOutput();

  for( unsigned int d = 0; d < numberOfControlPoints.Size(); d++ )
    {
    if( numberOfControlPoints[d] <= 0 )
      {
      return smoothField;
      }
    }

  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetDisplacementField( field );
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

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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkImageRegistrationMethodv4.h"
#include "itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod.h"

#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkCompositeTransform.h"
#include "itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor.h"
#include "itkVector.h"

template<class TFilter>
class CommandIterationUpdate : public itk::Command
{
public:
  typedef CommandIterationUpdate   Self;
  typedef itk::Command             Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );
protected:
  CommandIterationUpdate() {};
public:

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
    Execute( (const itk::Object *) caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
    const TFilter * filter =
      dynamic_cast< const TFilter * >( object );
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      { return; }

    unsigned int currentLevel = filter->GetCurrentLevel();
    typename TFilter::ShrinkFactorsArrayType shrinkFactors = filter->GetShrinkFactorsPerLevel();
    typename TFilter::SmoothingSigmasArrayType smoothingSigmas = filter->GetSmoothingSigmasPerLevel();
    typename TFilter::TransformParametersAdaptorsContainerType adaptors = filter->GetTransformParametersAdaptorsPerLevel();

    std::cout << "  Current level = " << currentLevel << std::endl;
    std::cout << "    shrink factor = " << shrinkFactors[currentLevel] << std::endl;
    std::cout << "    smoothing sigma = " << smoothingSigmas[currentLevel] << std::endl;
    std::cout << "    required fixed parameters = " << adaptors[currentLevel]->GetRequiredFixedParameters() << std::endl;
    }
};

template<unsigned int TDimension>
int PerformTimeVaryingBSplineVelocityFieldImageRegistration( int argc, char *argv[] )
{

  int numberOfAffineIterations = 100;
  int numberOfDeformableIterationsLevel0 = 10;
  int numberOfDeformableIterationsLevel1 = 20;
  int numberOfDeformableIterationsLevel2 = 11;
  double learningRate = static_cast<double>(0.5);

  if( argc >= 6 )
    {
    numberOfAffineIterations = atoi( argv[5] );
    }
  if( argc >= 7 )
    {
    numberOfDeformableIterationsLevel0 = atoi( argv[6] );
    }
  if( argc >= 8 )
    {
    numberOfDeformableIterationsLevel1 = atoi( argv[7] );
    }
  if( argc >= 9 )
    {
    numberOfDeformableIterationsLevel2 = atoi( argv[8] );
    }
  if( argc >= 10 )
    {
    learningRate = atof( argv[9] );
    }

  const unsigned int ImageDimension = TDimension;

  typedef float                                 PixelType;
  typedef itk::Image<PixelType, ImageDimension> FixedImageType;
  typedef itk::Image<PixelType, ImageDimension> MovingImageType;

  typedef itk::ImageFileReader<FixedImageType> ImageReaderType;

  typename ImageReaderType::Pointer fixedImageReader = ImageReaderType::New();
  fixedImageReader->SetFileName( argv[2] );
  fixedImageReader->Update();
  typename FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  fixedImage->Update();
  fixedImage->DisconnectPipeline();

  typename ImageReaderType::Pointer movingImageReader = ImageReaderType::New();
  movingImageReader->SetFileName( argv[3] );
  movingImageReader->Update();
  typename MovingImageType::Pointer movingImage = movingImageReader->GetOutput();
  movingImage->Update();
  movingImage->DisconnectPipeline();

  typedef itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType> AffineRegistrationType;
  typename AffineRegistrationType::Pointer affineSimple = AffineRegistrationType::New();
  affineSimple->SetFixedImage( fixedImage );
  affineSimple->SetMovingImage( movingImage );

  // Shrink the virtual domain by specified factors for each level.  See documentation
  // for the itkShrinkImageFilter for more detailed behavior.
  typename AffineRegistrationType::ShrinkFactorsArrayType affineShrinkFactorsPerLevel;
  affineShrinkFactorsPerLevel.SetSize( 3 );
  affineShrinkFactorsPerLevel[0] = 4;
  affineShrinkFactorsPerLevel[1] = 4;
  affineShrinkFactorsPerLevel[2] = 4;
  affineSimple->SetShrinkFactorsPerLevel( affineShrinkFactorsPerLevel );

  // Set the number of iterations
  typedef itk::GradientDescentOptimizerv4 GradientDescentOptimizerType;
  GradientDescentOptimizerType * optimizer = reinterpret_cast<GradientDescentOptimizerType *>(
    const_cast<typename AffineRegistrationType::OptimizerType *>( affineSimple->GetOptimizer() ) );
  optimizer->SetNumberOfIterations( numberOfAffineIterations );
  std::cout << "number of affine iterations: " << numberOfAffineIterations << std::endl;

  typedef CommandIterationUpdate<AffineRegistrationType> AffineCommandType;
  typename AffineCommandType::Pointer affineObserver = AffineCommandType::New();
  affineSimple->AddObserver( itk::IterationEvent(), affineObserver );

  try
    {
    std::cout << "Affine transform" << std::endl;
    affineSimple->StartRegistration();
    }
  catch( itk::ExceptionObject &e )
    {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Now do the displacement field transform with gaussian smoothing using
  // the composite transform.
  //

  typedef typename AffineRegistrationType::RealType RealType;

  typedef itk::CompositeTransform<RealType, ImageDimension> CompositeTransformType;
  typename CompositeTransformType::Pointer compositeTransform = CompositeTransformType::New();
  compositeTransform->AddTransform( const_cast<typename AffineRegistrationType::TransformType *>( affineSimple->GetOutput()->Get() ) );

  typedef itk::ResampleImageFilter<MovingImageType, FixedImageType> AffineResampleFilterType;
  typename AffineResampleFilterType::Pointer affineResampler = AffineResampleFilterType::New();
  affineResampler->SetTransform( compositeTransform );
  affineResampler->SetInput( movingImage );
  affineResampler->SetSize( fixedImage->GetBufferedRegion().GetSize() );
  affineResampler->SetOutputOrigin(  fixedImage->GetOrigin() );
  affineResampler->SetOutputSpacing( fixedImage->GetSpacing() );
  affineResampler->SetOutputDirection( fixedImage->GetDirection() );
  affineResampler->SetDefaultPixelValue( 0 );
  affineResampler->Update();

  std::string affineMovingImageFileName = std::string( argv[4] ) + std::string( "MovingImageAfterAffineTransform.nii.gz" );

  typedef itk::ImageFileWriter<FixedImageType> AffineWriterType;
  typename AffineWriterType::Pointer affineWriter = AffineWriterType::New();
  affineWriter->SetFileName( affineMovingImageFileName.c_str() );
  affineWriter->SetInput( affineResampler->GetOutput() );
  affineWriter->Update();

  typedef itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<FixedImageType, MovingImageType> CorrelationMetricType;
  typename CorrelationMetricType::Pointer correlationMetric = CorrelationMetricType::New();
  typename CorrelationMetricType::RadiusType radius;
  radius.Fill( 4 );
  correlationMetric->SetRadius( radius );
  correlationMetric->SetDoFixedImagePreWarp( true );
  correlationMetric->SetDoMovingImagePreWarp( true );
  correlationMetric->SetUseMovingImageGradientFilter( false );
  correlationMetric->SetUseFixedImageGradientFilter( false );

  typedef itk::TimeVaryingBSplineVelocityFieldImageRegistrationMethod<FixedImageType, MovingImageType> VelocityFieldRegistrationType;
  typename VelocityFieldRegistrationType::Pointer velocityFieldRegistration = VelocityFieldRegistrationType::New();
  velocityFieldRegistration->SetFixedImage( fixedImage );
  velocityFieldRegistration->SetMovingImage( movingImage );
  velocityFieldRegistration->SetNumberOfLevels( 3 );
  velocityFieldRegistration->SetCompositeTransform( compositeTransform );
  velocityFieldRegistration->SetMetric( correlationMetric );
  velocityFieldRegistration->SetLearningRate( learningRate );
  std::cout << "learningRate: " << learningRate << std::endl;
  velocityFieldRegistration->GetTransform()->SetSplineOrder( 3 );
  velocityFieldRegistration->GetTransform()->SetLowerTimeBound( 0.0 );
  velocityFieldRegistration->GetTransform()->SetUpperTimeBound( 1.0 );

  typename VelocityFieldRegistrationType::ShrinkFactorsArrayType numberOfIterationsPerLevel;
  numberOfIterationsPerLevel.SetSize( 3 );
  numberOfIterationsPerLevel[0] = numberOfDeformableIterationsLevel0;
  numberOfIterationsPerLevel[1] = numberOfDeformableIterationsLevel1;
  numberOfIterationsPerLevel[2] = numberOfDeformableIterationsLevel2;
  velocityFieldRegistration->SetNumberOfIterationsPerLevel( numberOfIterationsPerLevel );
  std::cout << "iterations per level: " << numberOfIterationsPerLevel[0] << ", "
            << numberOfIterationsPerLevel[1] << ", " << numberOfIterationsPerLevel[2] << std::endl;

  typename VelocityFieldRegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 3 );
  shrinkFactorsPerLevel[0] = 3;
  shrinkFactorsPerLevel[1] = 2;
  shrinkFactorsPerLevel[2] = 1;
  velocityFieldRegistration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );

  typename VelocityFieldRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 3 );
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 0;
  velocityFieldRegistration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );

  typedef itk::Vector<RealType, ImageDimension> VectorType;
  typedef itk::Image<VectorType, ImageDimension+1> TimeVaryingVelocityFieldControlPointLatticeType;
  typename TimeVaryingVelocityFieldControlPointLatticeType::Pointer velocityFieldLattice = TimeVaryingVelocityFieldControlPointLatticeType::New();

  // Determine the parameters (size, spacing, etc) for the time-varying velocity field

  typename FixedImageType::SizeType fixedImageSize = fixedImage->GetBufferedRegion().GetSize();
  typename FixedImageType::PointType fixedImageOrigin = fixedImage->GetOrigin();
  typename FixedImageType::SpacingType fixedImageSpacing = fixedImage->GetSpacing();
  typename FixedImageType::DirectionType fixedImageDirection = fixedImage->GetDirection();

  typename TimeVaryingVelocityFieldControlPointLatticeType::SizeType transformDomainMeshSize;
  typename TimeVaryingVelocityFieldControlPointLatticeType::PointType transformDomainOrigin;
  typename TimeVaryingVelocityFieldControlPointLatticeType::SpacingType transformDomainPhysicalDimensions;
  typename TimeVaryingVelocityFieldControlPointLatticeType::DirectionType transformDomainDirection;

  transformDomainDirection.SetIdentity();
  transformDomainOrigin.Fill( 0.0 );
  transformDomainMeshSize.Fill( 4 );
  transformDomainPhysicalDimensions.Fill( 1.0 );

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    transformDomainOrigin[i] = fixedImageOrigin[i];
    transformDomainMeshSize[i] = 3;
    transformDomainPhysicalDimensions[i] = static_cast<double>( fixedImageSize[i] - 1 ) * fixedImageSpacing[i];
    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      transformDomainDirection[i][j] = fixedImageDirection[i][j];
      }
    }

  typedef typename VelocityFieldRegistrationType::TransformType TransformType;

  typedef itk::TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TransformType> VelocityFieldTransformAdaptorType;
  typename VelocityFieldTransformAdaptorType::Pointer initialFieldTransformAdaptor = VelocityFieldTransformAdaptorType::New();
  initialFieldTransformAdaptor->SetTransform( velocityFieldRegistration->GetTransform() );
  initialFieldTransformAdaptor->SetRequiredTransformDomainOrigin( transformDomainOrigin );
  initialFieldTransformAdaptor->SetRequiredTransformDomainPhysicalDimensions( transformDomainPhysicalDimensions );
  initialFieldTransformAdaptor->SetRequiredTransformDomainMeshSize( transformDomainMeshSize );
  initialFieldTransformAdaptor->SetRequiredTransformDomainDirection( transformDomainDirection );

  VectorType zeroVector( 0.0 );

  velocityFieldLattice->SetOrigin( initialFieldTransformAdaptor->GetRequiredControlPointLatticeOrigin() );
  velocityFieldLattice->SetSpacing( initialFieldTransformAdaptor->GetRequiredControlPointLatticeSpacing() );
  velocityFieldLattice->SetDirection( initialFieldTransformAdaptor->GetRequiredControlPointLatticeDirection() );
  velocityFieldLattice->SetRegions( initialFieldTransformAdaptor->GetRequiredControlPointLatticeSize() );
  velocityFieldLattice->Allocate();
  velocityFieldLattice->FillBuffer( zeroVector );

  typename TransformType::VelocityFieldPointType        sampledVelocityFieldOrigin;
  typename TransformType::VelocityFieldSpacingType      sampledVelocityFieldSpacing;
  typename TransformType::VelocityFieldSizeType         sampledVelocityFieldSize;
  typename TransformType::VelocityFieldDirectionType    sampledVelocityFieldDirection;

  sampledVelocityFieldOrigin.Fill( 0.0 );
  sampledVelocityFieldSpacing.Fill( 1.0 );
  sampledVelocityFieldSize.Fill( velocityFieldRegistration->GetNumberOfTimePointSamples() );
  sampledVelocityFieldDirection.SetIdentity();

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    sampledVelocityFieldOrigin[i] = fixedImage->GetOrigin()[i];
    sampledVelocityFieldSpacing[i] = fixedImage->GetSpacing()[i];
    sampledVelocityFieldSize[i] = fixedImage->GetRequestedRegion().GetSize()[i];
    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      sampledVelocityFieldDirection[i][j] = fixedImage->GetDirection()[i][j];
      }
    }

  velocityFieldRegistration->GetTransform()->SetTimeVaryingVelocityFieldControlPointLattice( velocityFieldLattice );
  velocityFieldRegistration->GetTransform()->SetVelocityFieldOrigin( sampledVelocityFieldOrigin );
  velocityFieldRegistration->GetTransform()->SetVelocityFieldDirection( sampledVelocityFieldDirection );
  velocityFieldRegistration->GetTransform()->SetVelocityFieldSpacing( sampledVelocityFieldSpacing );
  velocityFieldRegistration->GetTransform()->SetVelocityFieldSize( sampledVelocityFieldSize );
  velocityFieldRegistration->GetTransform()->IntegrateVelocityField();

  typename VelocityFieldRegistrationType::TransformParametersAdaptorsContainerType adaptors;
  for( unsigned int level = 0; level < shrinkFactorsPerLevel.Size(); level++ )
    {

    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      transformDomainMeshSize[i] <<= 1;
      }

    typename VelocityFieldTransformAdaptorType::Pointer fieldTransformAdaptor = VelocityFieldTransformAdaptorType::New();
    fieldTransformAdaptor->SetTransform( velocityFieldRegistration->GetTransform() );
    fieldTransformAdaptor->SetRequiredTransformDomainOrigin( transformDomainOrigin );
    fieldTransformAdaptor->SetRequiredTransformDomainMeshSize( transformDomainMeshSize );
    fieldTransformAdaptor->SetRequiredTransformDomainDirection( transformDomainDirection );
    fieldTransformAdaptor->SetRequiredTransformDomainPhysicalDimensions( transformDomainPhysicalDimensions );

    adaptors.push_back( fieldTransformAdaptor.GetPointer() );
    }
  velocityFieldRegistration->SetTransformParametersAdaptorsPerLevel( adaptors );

  typedef CommandIterationUpdate<VelocityFieldRegistrationType> VelocityFieldRegistrationCommandType;
  typename VelocityFieldRegistrationCommandType::Pointer displacementFieldObserver = VelocityFieldRegistrationCommandType::New();
  velocityFieldRegistration->AddObserver( itk::IterationEvent(), displacementFieldObserver );

  try
    {
    std::cout << "Time-varying B-spline velocity field transform" << std::endl;
    velocityFieldRegistration->StartRegistration();
    }
  catch( itk::ExceptionObject &e )
    {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ResampleImageFilter<MovingImageType, FixedImageType> ResampleFilterType;
  typename ResampleFilterType::Pointer resampler = ResampleFilterType::New();
  resampler->SetTransform( compositeTransform );
  resampler->SetInput( movingImage );
  resampler->SetSize( fixedImage->GetBufferedRegion().GetSize() );
  resampler->SetOutputOrigin( fixedImage->GetOrigin() );
  resampler->SetOutputSpacing( fixedImage->GetSpacing() );
  resampler->SetOutputDirection( fixedImage->GetDirection() );
  resampler->SetDefaultPixelValue( 0 );
  resampler->Update();

  std::string warpedMovingImageFileName = std::string( argv[4] ) + std::string( "MovingImageAfterVelocityFieldTransform.nii.gz" );

  typedef itk::ImageFileWriter<FixedImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( warpedMovingImageFileName.c_str() );
  writer->SetInput( resampler->GetOutput() );
  writer->Update();

  typedef itk::ResampleImageFilter<FixedImageType, MovingImageType> InverseResampleFilterType;
  typename InverseResampleFilterType::Pointer inverseResampler = ResampleFilterType::New();
  inverseResampler->SetTransform( compositeTransform->GetInverseTransform() );
  inverseResampler->SetInput( fixedImage );
  inverseResampler->SetSize( movingImage->GetBufferedRegion().GetSize() );
  inverseResampler->SetOutputOrigin( movingImage->GetOrigin() );
  inverseResampler->SetOutputSpacing( movingImage->GetSpacing() );
  inverseResampler->SetOutputDirection( movingImage->GetDirection() );
  inverseResampler->SetDefaultPixelValue( 0 );
  inverseResampler->Update();

  std::string inverseWarpedFixedImageFileName = std::string( argv[4] ) + std::string( "InverseWarpedFixedImage.nii.gz" );

  typedef itk::ImageFileWriter<MovingImageType> InverseWriterType;
  typename InverseWriterType::Pointer inverseWriter = InverseWriterType::New();
  inverseWriter->SetFileName( inverseWarpedFixedImageFileName.c_str() );
  inverseWriter->SetInput( inverseResampler->GetOutput() );
  inverseWriter->Update();

  std::string velocityFieldLatticeFileName = std::string( argv[4] ) + std::string( "VelocityFieldControlPointLattice.nii.gz" );

  typedef itk::ImageFileWriter<TimeVaryingVelocityFieldControlPointLatticeType> VelocityFieldWriterType;
  typename VelocityFieldWriterType::Pointer velocityFieldLatticeWriter = VelocityFieldWriterType::New();
  velocityFieldLatticeWriter->SetFileName( velocityFieldLatticeFileName.c_str() );
  velocityFieldLatticeWriter->SetInput( velocityFieldRegistration->GetTransform()->GetTimeVaryingVelocityFieldControlPointLattice() );
  velocityFieldLatticeWriter->Update();

  return EXIT_SUCCESS;
}

int itkTimeVaryingBSplineVelocityFieldImageRegistrationTest( int argc, char *argv[] )
{
  if ( argc < 4 )
    {
    std::cout << argv[0] << " imageDimension fixedImage movingImage outputPrefix [numberOfAffineIterations = 100] "
              << "[numberOfDeformableIterationsLevel0 = 10] [numberOfDeformableIterationsLevel1 = 20] [numberOfDeformableIterationsLevel2 = 11 ] [learningRate = 0.5]" << std::endl;
    exit( 1 );
    }

  switch( atoi( argv[1] ) )
   {
   case 2:
     PerformTimeVaryingBSplineVelocityFieldImageRegistration<2>( argc, argv );
     break;
   case 3:
     PerformTimeVaryingBSplineVelocityFieldImageRegistration<3>( argc, argv );
     break;
   default:
      std::cerr << "Unsupported dimension" << std::endl;
      exit( EXIT_FAILURE );
   }
  return EXIT_SUCCESS;
}

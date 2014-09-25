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

#include "itkAffineTransform.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkBSplineTransform.h"
#include "itkBSplineTransformParametersAdaptor.h"

template<typename TFilter>
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

  virtual void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *) caller, event);
    }

  virtual void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    const TFilter * filter =
      dynamic_cast< const TFilter * >( object );
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      { return; }

    unsigned int currentLevel = filter->GetCurrentLevel();
    typename TFilter::ShrinkFactorsPerDimensionContainerType shrinkFactors = filter->GetShrinkFactorsPerDimension( currentLevel );
    typename TFilter::SmoothingSigmasArrayType smoothingSigmas = filter->GetSmoothingSigmasPerLevel();
    typename TFilter::TransformParametersAdaptorsContainerType adaptors = filter->GetTransformParametersAdaptorsPerLevel();

    const itk::ObjectToObjectOptimizerBase * optimizerBase = filter->GetOptimizer();
    typedef itk::GradientDescentOptimizerv4 GradientDescentOptimizerv4Type;
    typename GradientDescentOptimizerv4Type::ConstPointer optimizer = dynamic_cast<const GradientDescentOptimizerv4Type *>(optimizerBase);
    if( !optimizer )
      {
      itkGenericExceptionMacro( "Error dynamic_cast failed" );
      }
    typename GradientDescentOptimizerv4Type::DerivativeType gradient = optimizer->GetGradient();

    //debug:
    std::cout << "  CL Current level:           " << currentLevel << std::endl;
    std::cout << "   SF Shrink factor:          " << shrinkFactors << std::endl;
    std::cout << "   SS Smoothing sigma:        " << smoothingSigmas[currentLevel] << std::endl;
    std::cout << "   RFP Required fixed params: " << adaptors[currentLevel]->GetRequiredFixedParameters() << std::endl;
    std::cout << "   LR Final learning rate:    " << optimizer->GetLearningRate() << std::endl;
    std::cout << "   FM Final metric value:     " << optimizer->GetCurrentMetricValue() << std::endl;
    std::cout << "   SC Optimizer scales:       " << optimizer->GetScales() << std::endl;
    std::cout << "   FG Final metric gradient (sample of values): ";
    if( gradient.GetSize() < 10 )
      {
      std::cout << gradient;
      }
    else
      {
      for( itk::SizeValueType i = 0; i < gradient.GetSize(); i += (gradient.GetSize() / 16) )
        {
        std::cout << gradient[i] << " ";
        }
      }
    std::cout << std::endl;
    }
};

template <unsigned int VImageDimension>
int PerformBSplineImageRegistration( int argc, char *argv[] )
{
  if( argc < 6 )
    {
    std::cout << argv[0] << " imageDimension fixedImage movingImage outputImage numberOfAffineIterations numberOfDeformableIterations" << std::endl;
    exit( 1 );
    }

  typedef double                                 PixelType;
  typedef itk::Image<PixelType, VImageDimension> FixedImageType;
  typedef itk::Image<PixelType, VImageDimension> MovingImageType;

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

  typedef itk::AffineTransform<double, VImageDimension>                                         AffineTransformType;
  typedef itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, AffineTransformType>  AffineRegistrationType;
  typename AffineRegistrationType::Pointer affineSimple = AffineRegistrationType::New();
  affineSimple->SetFixedImage( fixedImage );
  affineSimple->SetMovingImage( movingImage );

  typedef itk::GradientDescentOptimizerv4 GradientDescentOptimizerv4Type;

  // Smooth by specified gaussian sigmas for each level.  These values are specified in
  // physical units. Sigmas of zero cause inconsistency between some platforms.
  {
  typename AffineRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 3 );
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 1; //0;
  affineSimple->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  }

  typedef itk::GradientDescentOptimizerv4 GradientDescentOptimizerv4Type;
  typename GradientDescentOptimizerv4Type::Pointer affineOptimizer =
    dynamic_cast<GradientDescentOptimizerv4Type * >( affineSimple->GetModifiableOptimizer() );
  if( !affineOptimizer )
    {
    itkGenericExceptionMacro( "Error dynamic_cast failed" );
    }
  affineOptimizer->SetNumberOfIterations( atoi( argv[5] ) );
  affineOptimizer->SetDoEstimateLearningRateOnce( false ); //true by default
  affineOptimizer->SetDoEstimateLearningRateAtEachIteration( true );

  typedef CommandIterationUpdate<AffineRegistrationType> AffineCommandType;
  typename AffineCommandType::Pointer affineObserver = AffineCommandType::New();
  affineSimple->AddObserver( itk::IterationEvent(), affineObserver );

  {
  typedef itk::ImageToImageMetricv4<FixedImageType, MovingImageType> ImageMetricType;
  typename ImageMetricType::Pointer imageMetric = dynamic_cast<ImageMetricType*>( affineSimple->GetModifiableMetric() );
  if(imageMetric.IsNull())
    {
    std::cout << "dynamic_cast failed." << std::endl;
    return EXIT_FAILURE;
    }
  imageMetric->SetFloatingPointCorrectionResolution( 1e4 );
  }

  try
    {
    std::cout << "Affine txf:" << std::endl;
    affineSimple->Update();
    }
  catch( itk::ExceptionObject &e )
    {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  {
  typedef itk::ImageToImageMetricv4<FixedImageType, MovingImageType> ImageMetricType;
  typename ImageMetricType::Pointer imageMetric = dynamic_cast<ImageMetricType*>( affineOptimizer->GetModifiableMetric() );
  std::cout << "Affine parameters after registration: " << std::endl
            << affineOptimizer->GetCurrentPosition() << std::endl
            << "Last LearningRate: " << affineOptimizer->GetLearningRate() << std::endl
            << "Use FltPtCorrex: " << imageMetric->GetUseFloatingPointCorrection() << std::endl
            << "FltPtCorrexRes: " << imageMetric->GetFloatingPointCorrectionResolution() << std::endl
            << "Number of threads used: metric: " << imageMetric->GetNumberOfThreadsUsed()
            << std::endl << " optimizer: " << affineOptimizer->GetNumberOfThreads() << std::endl;
  }

  //
  // Now do the b-spline deformable transform with CC metric
  //

  typedef itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<FixedImageType, MovingImageType> CorrelationMetricType;
  typename CorrelationMetricType::Pointer correlationMetric = CorrelationMetricType::New();
  typename CorrelationMetricType::RadiusType radius;
  radius.Fill( 4 );
  correlationMetric->SetRadius( radius );
  correlationMetric->SetUseMovingImageGradientFilter( false );
  correlationMetric->SetUseFixedImageGradientFilter( false );

  typedef itk::RegistrationParameterScalesFromPhysicalShift<CorrelationMetricType> ScalesEstimatorType;
  typename ScalesEstimatorType::Pointer scalesEstimator = ScalesEstimatorType::New();
  scalesEstimator->SetMetric( correlationMetric );
  scalesEstimator->SetTransformForward( true );
  scalesEstimator->SetSmallParameterVariation( 1.0 );

  typename GradientDescentOptimizerv4Type::Pointer optimizer = GradientDescentOptimizerv4Type::New();
  optimizer->SetLearningRate( 1.0 );
  optimizer->SetNumberOfIterations( atoi( argv[6] ) );
  optimizer->SetScalesEstimator( scalesEstimator );
  optimizer->SetDoEstimateLearningRateOnce( false ); //true by default
  optimizer->SetDoEstimateLearningRateAtEachIteration( true );

  typedef typename AffineRegistrationType::RealType RealType;

  typedef itk::CompositeTransform<RealType, VImageDimension> CompositeTransformType;
  typename CompositeTransformType::Pointer compositeTransform = CompositeTransformType::New();
  compositeTransform->AddTransform( affineSimple->GetModifiableTransform() );

  const unsigned int numberOfLevels = 3;
  const unsigned int SplineOrder = 3;
  typedef itk::BSplineTransform<RealType, VImageDimension, SplineOrder> BSplineTransformType;

  typedef itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, BSplineTransformType> BSplineRegistrationType;
  typename BSplineRegistrationType::Pointer bsplineRegistration = BSplineRegistrationType::New();

  // Shrink the virtual domain by specified factors for each level.  See documentation
  // for the itkShrinkImageFilter for more detailed behavior.
  typename BSplineRegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 3 );
  shrinkFactorsPerLevel[0] = 3;
  shrinkFactorsPerLevel[1] = 2;
  shrinkFactorsPerLevel[2] = 1;

  // Smooth by specified gaussian sigmas for each level.  These values are specified in
  // physical units.
  typename BSplineRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 3 );
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 1;

  typename BSplineTransformType::Pointer outputBSplineTransform =  BSplineTransformType::New();

  typename BSplineTransformType::PhysicalDimensionsType physicalDimensions;
  typename BSplineTransformType::MeshSizeType meshSize;
  for( unsigned int d = 0; d < VImageDimension; d++ )
    {
    physicalDimensions[d] = fixedImage->GetSpacing()[d] * static_cast<RealType>( fixedImage->GetLargestPossibleRegion().GetSize()[d] - 1 );
    meshSize[d] = 5;
    }

  // Create the transform adaptors

  typename BSplineRegistrationType::TransformParametersAdaptorsContainerType adaptors;
  // Create the transform adaptors specific to B-splines
  for( unsigned int level = 0; level < numberOfLevels; level++ )
    {
    typedef itk::ShrinkImageFilter<FixedImageType, FixedImageType> ShrinkFilterType;
    typename ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors( shrinkFactorsPerLevel[level] );
    shrinkFilter->SetInput( fixedImage );
    shrinkFilter->Update();

    // A good heuristic is to double the b-spline mesh resolution at each level

    typename BSplineTransformType::MeshSizeType requiredMeshSize;
    for( unsigned int d = 0; d < VImageDimension; d++ )
      {
      requiredMeshSize[d] = meshSize[d] << level;
      }

    typedef itk::BSplineTransformParametersAdaptor<BSplineTransformType> BSplineAdaptorType;
    typename BSplineAdaptorType::Pointer bsplineAdaptor = BSplineAdaptorType::New();
    bsplineAdaptor->SetTransform( outputBSplineTransform );
    bsplineAdaptor->SetRequiredTransformDomainMeshSize( requiredMeshSize );
    bsplineAdaptor->SetRequiredTransformDomainOrigin( shrinkFilter->GetOutput()->GetOrigin() );
    bsplineAdaptor->SetRequiredTransformDomainDirection( shrinkFilter->GetOutput()->GetDirection() );
    bsplineAdaptor->SetRequiredTransformDomainPhysicalDimensions( physicalDimensions );

    adaptors.push_back( bsplineAdaptor.GetPointer() );
    }

  bsplineRegistration->SetFixedImage( 0, fixedImage );
  bsplineRegistration->SetMovingImage( 0, movingImage );
  bsplineRegistration->SetMetric( correlationMetric );
  bsplineRegistration->SetNumberOfLevels( numberOfLevels );
  bsplineRegistration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  bsplineRegistration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );
  bsplineRegistration->SetOptimizer( optimizer );
  bsplineRegistration->SetMovingInitialTransform( compositeTransform );
  bsplineRegistration->SetTransformParametersAdaptorsPerLevel( adaptors );

  outputBSplineTransform->SetTransformDomainOrigin( fixedImage->GetOrigin() );
  outputBSplineTransform->SetTransformDomainPhysicalDimensions( physicalDimensions );
  outputBSplineTransform->SetTransformDomainMeshSize( meshSize );
  outputBSplineTransform->SetTransformDomainDirection( fixedImage->GetDirection() );
  outputBSplineTransform->SetIdentity();

  bsplineRegistration->SetInitialTransform( outputBSplineTransform );
  bsplineRegistration->InPlaceOn();

  typedef CommandIterationUpdate<BSplineRegistrationType> BSplineRegistrationCommandType;
  typename BSplineRegistrationCommandType::Pointer bsplineObserver = BSplineRegistrationCommandType::New();
  bsplineRegistration->AddObserver( itk::IterationEvent(), bsplineObserver );

  try
    {
    std::cout << "BSpline. txf - bspline update" << std::endl;
    bsplineRegistration->Update();
    }
  catch( itk::ExceptionObject &e )
    {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  compositeTransform->AddTransform( bsplineRegistration->GetModifiableTransform() );

  std::cout << "After displacement registration: " << std::endl
            << "Last LearningRate: " << optimizer->GetLearningRate() << std::endl
            << "Use FltPtCorrex: " << correlationMetric->GetUseFloatingPointCorrection() << std::endl
            << "FltPtCorrexRes: " << correlationMetric->GetFloatingPointCorrectionResolution() << std::endl
            << "Number of threads used: metric: " << correlationMetric->GetNumberOfThreadsUsed()
            << "Number of threads used: metric: " << correlationMetric->GetNumberOfThreadsUsed()
            << " optimizer: " << bsplineRegistration->GetOptimizer()->GetNumberOfThreads() << std::endl;

  typedef itk::ResampleImageFilter<MovingImageType, FixedImageType> ResampleFilterType;
  typename ResampleFilterType::Pointer resampler = ResampleFilterType::New();
  resampler->SetTransform( compositeTransform );
  resampler->SetInput( movingImage );
  resampler->SetSize( fixedImage->GetLargestPossibleRegion().GetSize() );
  resampler->SetOutputOrigin(  fixedImage->GetOrigin() );
  resampler->SetOutputSpacing( fixedImage->GetSpacing() );
  resampler->SetOutputDirection( fixedImage->GetDirection() );
  resampler->SetDefaultPixelValue( 0 );
  resampler->Update();

  typedef itk::ImageFileWriter<FixedImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[4] );
  writer->SetInput( resampler->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}

int itkBSplineImageRegistrationTest( int argc, char *argv[] )
{
  if( argc < 6 )
    {
    std::cout << argv[0] << " imageDimension fixedImage movingImage outputImage numberOfAffineIterations numberOfDeformableIterations" << std::endl;
    exit( 1 );
    }

  switch( atoi( argv[1] ) )
   {
   case 2:
     PerformBSplineImageRegistration<2>( argc, argv );
     break;
   case 3:
     PerformBSplineImageRegistration<3>( argc, argv );
     break;
   default:
      std::cerr << "Unsupported dimension" << std::endl;
      exit( EXIT_FAILURE );
   }
  return EXIT_SUCCESS;
}

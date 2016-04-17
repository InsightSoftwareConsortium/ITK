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
#include "itkCompositeTransform.h"
#include "itkEuler2DTransform.h"
#include "itkEuler3DTransform.h"
#include "itkCorrelationImageToImageMetricv4.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkObjectToObjectMultiMetricv4.h"

template <unsigned int TImageDimension>
class RigidTransformTraits
{
public:
  typedef itk::AffineTransform<double, TImageDimension> TransformType;
};

template <>
class RigidTransformTraits<2>
{
public:
  typedef itk::Euler2DTransform<double> TransformType;
};

template <>
class RigidTransformTraits<3>
{
public:
  typedef itk::Euler3DTransform<double> TransformType;
};


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

    const itk::ObjectToObjectOptimizerBase* optimizerBase = filter->GetOptimizer();
    typedef itk::GradientDescentOptimizerv4 GradientDescentOptimizerv4Type;
    typename GradientDescentOptimizerv4Type::ConstPointer optimizer = dynamic_cast<const GradientDescentOptimizerv4Type *>(optimizerBase);
    if( !optimizer )
      {
      itkGenericExceptionMacro( "Error dynamic_cast failed" );
      }
    typename GradientDescentOptimizerv4Type::DerivativeType gradient = optimizer->GetGradient();

    /* orig
    std::cout << "  Current level = " << currentLevel << std::endl;
    std::cout << "    shrink factor = " << shrinkFactors[currentLevel] << std::endl;
    std::cout << "    smoothing sigma = " << smoothingSigmas[currentLevel] << std::endl;
    std::cout << "    required fixed parameters = " << adaptors[currentLevel]->GetRequiredFixedParameters() << std::endl;
    */

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
int PerformSimpleImageRegistration2( int argc, char *argv[] )
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

  // Set up MI metric
  typedef itk::JointHistogramMutualInformationImageToImageMetricv4<FixedImageType, MovingImageType> MIMetricType;
  typename MIMetricType::Pointer mutualInformationMetric = MIMetricType::New();
  mutualInformationMetric->SetNumberOfHistogramBins( 20 );
  mutualInformationMetric->SetUseMovingImageGradientFilter( false );
  mutualInformationMetric->SetUseFixedImageGradientFilter( false );
  mutualInformationMetric->SetUseFixedSampledPointSet( false );

  // Set up CC metric
  typedef itk::CorrelationImageToImageMetricv4<FixedImageType, MovingImageType> GlobalCorrelationMetricType;
  typename GlobalCorrelationMetricType::Pointer gCorrelationMetric = GlobalCorrelationMetricType::New();


  // Stage1: Rigid registration
  //
  typedef itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType> RegistrationType;
  typename RegistrationType::Pointer rigidRegistration = RegistrationType::New();
  rigidRegistration->SetObjectName("RigidSimple");
  // Set up rigid multi metric: It only has one metric component
  typedef itk::ObjectToObjectMultiMetricv4<VImageDimension, VImageDimension> MultiMetricType;

  typename MultiMetricType::Pointer rigidMultiMetric = MultiMetricType::New();
  rigidMultiMetric->AddMetric( mutualInformationMetric );
  rigidRegistration->SetMetric( rigidMultiMetric );

  rigidRegistration->SetFixedImage( fixedImage );
  rigidRegistration->SetMovingImage( movingImage );

  // Rigid transform that is set to be optimized
  typedef typename RigidTransformTraits<VImageDimension>::TransformType   RigidTransformType;
  typename RigidTransformType::Pointer rigidTransform = RigidTransformType::New();
  rigidRegistration->SetInitialTransform( rigidTransform );
  rigidRegistration->InPlaceOn();

  typename RegistrationType::ShrinkFactorsArrayType rigidShrinkFactorsPerLevel;
  rigidShrinkFactorsPerLevel.SetSize( 3 );
  rigidShrinkFactorsPerLevel[0] = 4;
  rigidShrinkFactorsPerLevel[1] = 4;
  rigidShrinkFactorsPerLevel[2] = 4;
  rigidRegistration->SetShrinkFactorsPerLevel( rigidShrinkFactorsPerLevel );

  typename RegistrationType::MetricSamplingStrategyType rigidSamplingStrategy = RegistrationType::RANDOM;
  double rigidSamplingPercentage = 0.20;
  rigidRegistration->SetMetricSamplingStrategy( rigidSamplingStrategy );
  rigidRegistration->SetMetricSamplingPercentage( rigidSamplingPercentage );

  typedef itk::RegistrationParameterScalesFromPhysicalShift<MIMetricType> RigidScalesEstimatorType;
  typename RigidScalesEstimatorType::Pointer rigidScalesEstimator = RigidScalesEstimatorType::New();
  rigidScalesEstimator->SetMetric( mutualInformationMetric );
  rigidScalesEstimator->SetTransformForward( true );

  typedef itk::GradientDescentOptimizerv4 GradientDescentOptimizerv4Type;
  GradientDescentOptimizerv4Type * rigidOptimizer = dynamic_cast<GradientDescentOptimizerv4Type *>( rigidRegistration->GetModifiableOptimizer() );
  if( !rigidOptimizer )
    {
    itkGenericExceptionMacro( "Error dynamic_cast failed" );
    }
  rigidOptimizer->SetLearningRate( 0.1 );
#ifdef NDEBUG
  rigidOptimizer->SetNumberOfIterations( atoi( argv[5] ) );
#else
  rigidOptimizer->SetNumberOfIterations( 1 );
#endif
  rigidOptimizer->SetDoEstimateLearningRateOnce( false ); //true by default
  rigidOptimizer->SetDoEstimateLearningRateAtEachIteration( true );
  rigidOptimizer->SetScalesEstimator( rigidScalesEstimator );

  typedef CommandIterationUpdate<RegistrationType> CommandType;
  typename CommandType::Pointer rigidObserver = CommandType::New();
  rigidRegistration->AddObserver( itk::MultiResolutionIterationEvent(), rigidObserver );


  // Stage2: Affine registration
  //
  typename RegistrationType::Pointer affineSimple = RegistrationType::New();
  affineSimple->SetObjectName("affineSimple");
  // Ensuring code coverage for boolean macros
  affineSimple->SmoothingSigmasAreSpecifiedInPhysicalUnitsOff();
  affineSimple->SetSmoothingSigmasAreSpecifiedInPhysicalUnits( false );
  if( affineSimple->GetSmoothingSigmasAreSpecifiedInPhysicalUnits() != false )
    {
    std::cerr << "Returned unexpected value of TRUE." << std::endl;
    return EXIT_FAILURE;
    }

  // Set up affine multi metric: It has two metric components
  typename MultiMetricType::Pointer affineMultiMetric = MultiMetricType::New();
  affineMultiMetric->AddMetric( mutualInformationMetric );
  affineMultiMetric->AddMetric( gCorrelationMetric );
  affineSimple->SetMetric( affineMultiMetric );

  affineSimple->SetFixedImage( 0, fixedImage );
  affineSimple->SetMovingImage( 0, movingImage );
  affineSimple->SetFixedImage( 1, fixedImage );
  affineSimple->SetMovingImage( 1, movingImage );

  typedef itk::AffineTransform<double, VImageDimension> AffineTransformType;
  typename AffineTransformType::Pointer   affineTransform  = AffineTransformType::New();
  affineSimple->SetInitialTransform(  affineTransform  );
  affineSimple->InPlaceOn();

  affineSimple->SetMovingInitialTransformInput( rigidRegistration->GetTransformOutput() );

  typedef itk::RegistrationParameterScalesFromPhysicalShift<MIMetricType> AffineScalesEstimatorType;
  typename AffineScalesEstimatorType::Pointer scalesEstimator1 = AffineScalesEstimatorType::New();
  scalesEstimator1->SetMetric( mutualInformationMetric );
  scalesEstimator1->SetTransformForward( true );

  affineSimple->SmoothingSigmasAreSpecifiedInPhysicalUnitsOn();
  affineSimple->SetSmoothingSigmasAreSpecifiedInPhysicalUnits( true );
  if( affineSimple->GetSmoothingSigmasAreSpecifiedInPhysicalUnits() != true )
    {
    std::cerr << "Returned unexpected value of FALSE." << std::endl;
    return EXIT_FAILURE;
    }

  // Smooth by specified gaussian sigmas for each level.  These values are specified in
  // physical units. Sigmas of zero cause inconsistency between some platforms.
  {
  typename RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 3 );
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 1; //0;
  affineSimple->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  }

  typename GradientDescentOptimizerv4Type::Pointer affineOptimizer =
    dynamic_cast<GradientDescentOptimizerv4Type * >( affineSimple->GetModifiableOptimizer() );
  if( !affineOptimizer )
    {
    itkGenericExceptionMacro( "Error dynamic_cast failed" );
    }

#ifdef NDEBUG
  affineOptimizer->SetNumberOfIterations( atoi( argv[5] ) );
#else
  affineOptimizer->SetNumberOfIterations( 1 );
#endif
  affineOptimizer->SetDoEstimateLearningRateOnce( false ); //true by default
  affineOptimizer->SetDoEstimateLearningRateAtEachIteration( true );
  affineOptimizer->SetScalesEstimator( scalesEstimator1 );

  typename CommandType::Pointer affineObserver = CommandType::New();
  affineSimple->AddObserver( itk::IterationEvent(), affineObserver );

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
  std::cout << "Affine parameters after registration: " << std::endl
            << affineOptimizer->GetCurrentPosition() << std::endl
            << "Last LearningRate: " << affineOptimizer->GetLearningRate() << std::endl
            << std::endl << " optimizer: " << affineOptimizer->GetNumberOfThreads() << std::endl;
  }

  typedef itk::CompositeTransform< double, VImageDimension >  CompositeTransformType;
  typename CompositeTransformType::Pointer compositeTransform  = CompositeTransformType::New();
  compositeTransform->AddTransform( rigidTransform );
  compositeTransform->AddTransform( affineTransform );

  typedef itk::ResampleImageFilter<MovingImageType, FixedImageType> ResampleFilterType;
  typename ResampleFilterType::Pointer resampler = ResampleFilterType::New();
  resampler->SetTransform( compositeTransform );
  resampler->SetInput( movingImage );
  resampler->SetSize( fixedImage->GetLargestPossibleRegion().GetSize() );
  resampler->SetOutputOrigin( fixedImage->GetOrigin() );
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

int itkSimpleImageRegistrationTest2( int argc, char *argv[] )
{
  if( argc < 6 )
    {
    std::cout << argv[0] << " imageDimension fixedImage movingImage outputImage numberOfAffineIterations" << std::endl;
    exit( 1 );
    }

  switch( atoi( argv[1] ) )
   {
   case 2:
     return PerformSimpleImageRegistration2<2>( argc, argv );

   case 3:
     return PerformSimpleImageRegistration2<3>( argc, argv );

   default:
      std::cerr << "Unsupported dimension" << std::endl;
      exit( EXIT_FAILURE );
   }
  return EXIT_SUCCESS;
}

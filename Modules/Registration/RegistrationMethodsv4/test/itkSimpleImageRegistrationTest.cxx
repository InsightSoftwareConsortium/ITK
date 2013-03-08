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
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"

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

    typename itk::ObjectToObjectOptimizerBase::Pointer optimizerBase = (const_cast<TFilter*>(filter))->GetModifiableOptimizer();
    typedef itk::GradientDescentOptimizerv4 GradientDescentOptimizerv4Type;
    typename GradientDescentOptimizerv4Type::Pointer optimizer = dynamic_cast<GradientDescentOptimizerv4Type *>(optimizerBase.GetPointer());
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
    std::cout << "   SF Shrink factor:          " << shrinkFactors[currentLevel] << std::endl;
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
int PerformSimpleImageRegistration( int argc, char *argv[] )
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

  typedef itk::AffineTransform<double, VImageDimension> AffineTransformType;
  typedef itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, AffineTransformType> AffineRegistrationType;
  typedef itk::GradientDescentOptimizerv4 GradientDescentOptimizerv4Type;
  typename AffineRegistrationType::Pointer affineSimple = AffineRegistrationType::New();
  affineSimple->SetFixedImage( fixedImage );
  affineSimple->SetMovingImage( movingImage );

  // Ensuring code coverage for boolean macros
  affineSimple->SmoothingSigmasAreSpecifiedInPhysicalUnitsOff();
  affineSimple->SetSmoothingSigmasAreSpecifiedInPhysicalUnits( false );
  if( affineSimple->GetSmoothingSigmasAreSpecifiedInPhysicalUnits() != false )
    {
    std::cerr << "Returned unexpected value of TRUE." << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::JointHistogramMutualInformationImageToImageMetricv4<FixedImageType, MovingImageType> MIMetricType;
  typename MIMetricType::Pointer mutualInformationMetric = MIMetricType::New();
  mutualInformationMetric->SetNumberOfHistogramBins( 20 );
  mutualInformationMetric->SetUseMovingImageGradientFilter( false );
  mutualInformationMetric->SetUseFixedImageGradientFilter( false );
  mutualInformationMetric->SetUseFixedSampledPointSet( false );
  affineSimple->SetMetric( mutualInformationMetric );

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
  affineOptimizer->SetScalesEstimator( scalesEstimator1 );

  typedef CommandIterationUpdate<AffineRegistrationType> AffineCommandType;
  typename AffineCommandType::Pointer affineObserver = AffineCommandType::New();
  affineSimple->AddObserver( itk::IterationEvent(), affineObserver );

  {
  typedef itk::ImageToImageMetricv4<FixedImageType, MovingImageType> ImageMetricType;
  typename ImageMetricType::Pointer imageMetric = dynamic_cast<ImageMetricType*>( affineSimple->GetModifiableMetric() );
  //imageMetric->SetUseFloatingPointCorrection(true);
  imageMetric->SetFloatingPointCorrectionResolution(1e4);
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
  // Now do the displacement field transform with gaussian smoothing using
  // the composite transform.
  //

  typedef typename AffineRegistrationType::RealType RealType;

  typedef itk::CompositeTransform<RealType, VImageDimension> CompositeTransformType;
  typename CompositeTransformType::Pointer compositeTransform = CompositeTransformType::New();
  compositeTransform->AddTransform( const_cast<typename AffineRegistrationType::OutputTransformType *>( affineSimple->GetOutput()->Get() ) );

  typedef itk::Vector<RealType, VImageDimension> VectorType;
  VectorType zeroVector( 0.0 );
  typedef itk::Image<VectorType, VImageDimension> DisplacementFieldType;
  typename DisplacementFieldType::Pointer displacementField = DisplacementFieldType::New();
  displacementField->CopyInformation( fixedImage );
  displacementField->SetRegions( fixedImage->GetBufferedRegion() );
  displacementField->Allocate();
  displacementField->FillBuffer( zeroVector );

  typedef itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<RealType, VImageDimension> DisplacementFieldTransformType;

  typedef itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, DisplacementFieldTransformType> DisplacementFieldRegistrationType;
  typename DisplacementFieldRegistrationType::Pointer displacementFieldSimple = DisplacementFieldRegistrationType::New();

  typename DisplacementFieldTransformType::Pointer fieldTransform = const_cast<DisplacementFieldTransformType *>( displacementFieldSimple->GetOutput()->Get() );
  fieldTransform->SetGaussianSmoothingVarianceForTheUpdateField( 0 );
  fieldTransform->SetGaussianSmoothingVarianceForTheTotalField( 1.5 );
  fieldTransform->SetDisplacementField( displacementField );

  typedef itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<FixedImageType, MovingImageType> CorrelationMetricType;
  typename CorrelationMetricType::Pointer correlationMetric = CorrelationMetricType::New();
  typename CorrelationMetricType::RadiusType radius;
  radius.Fill( 4 );
  correlationMetric->SetRadius( radius );
  correlationMetric->SetUseMovingImageGradientFilter( false );
  correlationMetric->SetUseFixedImageGradientFilter( false );

  //correlationMetric->SetUseFloatingPointCorrection(true);
  //correlationMetric->SetFloatingPointCorrectionResolution(1e4);

  typedef itk::RegistrationParameterScalesFromPhysicalShift<CorrelationMetricType> ScalesEstimatorType;
  typename ScalesEstimatorType::Pointer scalesEstimator = ScalesEstimatorType::New();
  scalesEstimator->SetMetric( correlationMetric );
  scalesEstimator->SetTransformForward( true );

  typename GradientDescentOptimizerv4Type::Pointer optimizer = GradientDescentOptimizerv4Type::New();
  optimizer->SetLearningRate( 1.0 );
  optimizer->SetNumberOfIterations( atoi( argv[6] ) );
  optimizer->SetScalesEstimator( scalesEstimator );
  optimizer->SetDoEstimateLearningRateOnce( false ); //true by default
  optimizer->SetDoEstimateLearningRateAtEachIteration( true );

  displacementFieldSimple->SetFixedImage( fixedImage );
  displacementFieldSimple->SetMovingImage( movingImage );
  displacementFieldSimple->SetNumberOfLevels( 3 );
  displacementFieldSimple->SetMovingInitialTransform( compositeTransform );
  displacementFieldSimple->SetMetric( correlationMetric );
  displacementFieldSimple->SetOptimizer( optimizer );

  // Shrink the virtual domain by specified factors for each level.  See documentation
  // for the itkShrinkImageFilter for more detailed behavior.
  typename DisplacementFieldRegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 3 );
  shrinkFactorsPerLevel[0] = 3;
  shrinkFactorsPerLevel[1] = 2;
  shrinkFactorsPerLevel[2] = 1;
  displacementFieldSimple->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );

  // Smooth by specified gaussian sigmas for each level.  These values are specified in
  // physical units.
  typename DisplacementFieldRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 3 );
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 1;
  displacementFieldSimple->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );

  typedef itk::GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<DisplacementFieldTransformType> DisplacementFieldTransformAdaptorType;

  typename DisplacementFieldRegistrationType::TransformParametersAdaptorsContainerType adaptors;

  for( unsigned int level = 0; level < shrinkFactorsPerLevel.Size(); level++ )
    {
    // We use the shrink image filter to calculate the fixed parameters of the virtual
    // domain at each level.  To speed up calculation and avoid unnecessary memory
    // usage, we could calculate these fixed parameters directly.

    typedef itk::ShrinkImageFilter<DisplacementFieldType, DisplacementFieldType> ShrinkFilterType;
    typename ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors( shrinkFactorsPerLevel[level] );
    shrinkFilter->SetInput( displacementField );
    shrinkFilter->Update();

    typename DisplacementFieldTransformAdaptorType::Pointer fieldTransformAdaptor = DisplacementFieldTransformAdaptorType::New();
    fieldTransformAdaptor->SetRequiredSpacing( shrinkFilter->GetOutput()->GetSpacing() );
    fieldTransformAdaptor->SetRequiredSize( shrinkFilter->GetOutput()->GetBufferedRegion().GetSize() );
    fieldTransformAdaptor->SetRequiredDirection( shrinkFilter->GetOutput()->GetDirection() );
    fieldTransformAdaptor->SetRequiredOrigin( shrinkFilter->GetOutput()->GetOrigin() );

    adaptors.push_back( fieldTransformAdaptor.GetPointer() );
    }
  displacementFieldSimple->SetTransformParametersAdaptorsPerLevel( adaptors );

  typedef CommandIterationUpdate<DisplacementFieldRegistrationType> DisplacementFieldRegistrationCommandType;
  typename DisplacementFieldRegistrationCommandType::Pointer displacementFieldObserver = DisplacementFieldRegistrationCommandType::New();
  displacementFieldSimple->AddObserver( itk::IterationEvent(), displacementFieldObserver );

  try
    {
    std::cout << "Displ. txf - gauss update" << std::endl;
    displacementFieldSimple->Update();
    }
  catch( itk::ExceptionObject &e )
    {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  compositeTransform->AddTransform( const_cast<DisplacementFieldTransformType *>( displacementFieldSimple->GetOutput()->Get() ) );

  std::cout << "After displacement registration: " << std::endl
            << "Last LearningRate: " << optimizer->GetLearningRate() << std::endl
            << "Use FltPtCorrex: " << correlationMetric->GetUseFloatingPointCorrection() << std::endl
            << "FltPtCorrexRes: " << correlationMetric->GetFloatingPointCorrectionResolution() << std::endl
            << "Number of threads used: metric: " << correlationMetric->GetNumberOfThreadsUsed()
            << "Number of threads used: metric: " << correlationMetric->GetNumberOfThreadsUsed()
            << " optimizer: " << displacementFieldSimple->GetOptimizer()->GetNumberOfThreads() << std::endl;

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

int itkSimpleImageRegistrationTest( int argc, char *argv[] )
{
  if( argc < 6 )
    {
    std::cout << argv[0] << " imageDimension fixedImage movingImage outputImage numberOfAffineIterations numberOfDeformableIterations" << std::endl;
    exit( 1 );
    }

  switch( atoi( argv[1] ) )
   {
   case 2:
     PerformSimpleImageRegistration<2>( argc, argv );
     break;
   case 3:
     PerformSimpleImageRegistration<3>( argc, argv );
     break;
   default:
      std::cerr << "Unsupported dimension" << std::endl;
      exit( EXIT_FAILURE );
   }
  return EXIT_SUCCESS;
}

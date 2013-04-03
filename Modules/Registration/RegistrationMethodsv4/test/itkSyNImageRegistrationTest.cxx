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
#include "itkSyNImageRegistrationMethod.h"

#include "itkAffineTransform.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkCompositeTransform.h"
#include "itkDisplacementFieldTransformParametersAdaptor.h"
#include "itkVector.h"

template<class TFilter>
class CommandIterationUpdate : public itk::Command
{
public:
  typedef CommandIterationUpdate                                          Self;
  typedef itk::Command                                                    Superclass;
  typedef itk::SmartPointer<Self>                                         Pointer;
  itkNewMacro( Self );

  typedef typename TFilter::FixedImageType                                FixedImageType;
  itkStaticConstMacro( ImageDimension, unsigned int, FixedImageType::ImageDimension ); /** ImageDimension constants */

  typedef itk::ShrinkImageFilter<FixedImageType, FixedImageType>          ShrinkFilterType;
  typedef typename TFilter::OutputTransformType::ScalarType               RealType;
  typedef itk::DisplacementFieldTransform<RealType, ImageDimension>       DisplacementFieldTransformType;
  typedef typename DisplacementFieldTransformType::DisplacementFieldType  DisplacementFieldType;

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
    std::cout << "    smoothing variance = " << smoothingSigmas[currentLevel] << std::endl;
    std::cout << "    required fixed parameters = " << adaptors[currentLevel]->GetRequiredFixedParameters() << std::endl;

    /*
    testing "itkGetConstObjectMacro" at each iteration
    */
    typename ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors( shrinkFactors[currentLevel] );
    shrinkFilter->SetInput( filter->GetFixedImage() );
    shrinkFilter->Update();

    const typename FixedImageType::SizeType ImageSize = shrinkFilter->GetOutput()->GetBufferedRegion().GetSize();

    const typename DisplacementFieldType::SizeType FixedDisplacementFieldSize =
      const_cast<DisplacementFieldTransformType *>( filter->GetFixedToMiddleTransform() )->GetDisplacementField()->GetBufferedRegion().GetSize();

    const typename DisplacementFieldType::SizeType MovingDisplacementFieldSize =
      const_cast<DisplacementFieldTransformType *>( filter->GetMovingToMiddleTransform() )->GetDisplacementField()->GetBufferedRegion().GetSize();

    if( ( FixedDisplacementFieldSize == ImageSize ) && ( MovingDisplacementFieldSize == ImageSize ) )
      {
      std::cout << " *Filter returns its internal transforms properly*" << std::endl;
      }
    else
      {
      itkExceptionMacro( "Internal transforms should be consistent with input image size at each iteration." );
      }
    }
};

template<unsigned int TDimension>
int PerformDisplacementFieldImageRegistration( int itkNotUsed( argc ), char *argv[] )
{
  const unsigned int ImageDimension = TDimension;

  typedef double                                PixelType;
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

  typedef itk::AffineTransform<double, ImageDimension> AffineTransformType;
  typedef itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, AffineTransformType> AffineRegistrationType;
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
  typedef itk::GradientDescentOptimizerv4 GradientDescentOptimizerv4Type;
  GradientDescentOptimizerv4Type * optimizer = reinterpret_cast<GradientDescentOptimizerv4Type *>(
    const_cast<typename AffineRegistrationType::OptimizerType *>( affineSimple->GetOptimizer() ) );
  optimizer->SetNumberOfIterations( 100 );

  try
    {
    std::cout << "Affine transform" << std::endl;
    affineSimple->Update();
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
  compositeTransform->AddTransform( const_cast<typename AffineRegistrationType::OutputTransformType *>( affineSimple->GetOutput()->Get() ) );

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

  typedef itk::Vector<RealType, ImageDimension> VectorType;
  VectorType zeroVector( 0.0 );

  // Create the SyN deformable registration method

  typedef itk::Image<VectorType, ImageDimension> DisplacementFieldType;
  typename DisplacementFieldType::Pointer displacementField = DisplacementFieldType::New();
  displacementField->CopyInformation( fixedImage );
  displacementField->SetRegions( fixedImage->GetBufferedRegion() );
  displacementField->Allocate();
  displacementField->FillBuffer( zeroVector );

  typename DisplacementFieldType::Pointer inverseDisplacementField = DisplacementFieldType::New();
  inverseDisplacementField->CopyInformation( fixedImage );
  inverseDisplacementField->SetRegions( fixedImage->GetBufferedRegion() );
  inverseDisplacementField->Allocate();
  inverseDisplacementField->FillBuffer( zeroVector );

  typedef itk::SyNImageRegistrationMethod<FixedImageType, MovingImageType> DisplacementFieldRegistrationType;
  typename DisplacementFieldRegistrationType::Pointer displacementFieldRegistration = DisplacementFieldRegistrationType::New();

  typedef typename DisplacementFieldRegistrationType::OutputTransformType OutputTransformType;
  typename OutputTransformType::Pointer outputTransform = const_cast<OutputTransformType *>( displacementFieldRegistration->GetOutput()->Get() );
  outputTransform->SetDisplacementField( displacementField );
  outputTransform->SetInverseDisplacementField( inverseDisplacementField );

  // Create the transform adaptors

  typedef itk::DisplacementFieldTransformParametersAdaptor<OutputTransformType> DisplacementFieldTransformAdaptorType;
  typename DisplacementFieldRegistrationType::TransformParametersAdaptorsContainerType adaptors;

  // Create the transform adaptors
  // For the gaussian displacement field, the specified variances are in image spacing terms
  // and, in normal practice, we typically don't change these values at each level.  However,
  // if the user wishes to add that option, they can use the class
  // GaussianSmoothingOnUpdateDisplacementFieldTransformAdaptor

  unsigned int numberOfLevels = 3;

  typename DisplacementFieldRegistrationType::NumberOfIterationsArrayType numberOfIterationsPerLevel;
  numberOfIterationsPerLevel.SetSize( 3 );
  numberOfIterationsPerLevel[0] = atoi( argv[5] );
  numberOfIterationsPerLevel[1] = 2;
  numberOfIterationsPerLevel[2] = 1;

  RealType varianceForUpdateField = 1.75;
  RealType varianceForTotalField = 0.5;

  typename DisplacementFieldRegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 3 );
  shrinkFactorsPerLevel[0] = 3;
  shrinkFactorsPerLevel[1] = 2;
  shrinkFactorsPerLevel[2] = 1;

  typename DisplacementFieldRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 3 );
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 0;


  for( unsigned int level = 0; level < numberOfLevels; level++ )
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
    fieldTransformAdaptor->SetTransform( outputTransform );

    adaptors.push_back( fieldTransformAdaptor.GetPointer() );
    }

  typedef itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<FixedImageType, MovingImageType> CorrelationMetricType;
  typename CorrelationMetricType::Pointer correlationMetric = CorrelationMetricType::New();
  typename CorrelationMetricType::RadiusType radius;
  radius.Fill( 4 );
  correlationMetric->SetRadius( radius );
  correlationMetric->SetUseMovingImageGradientFilter( false );
  correlationMetric->SetUseFixedImageGradientFilter( false );

  displacementFieldRegistration->SetFixedImage( fixedImage );
  displacementFieldRegistration->SetMovingImage( movingImage );
  displacementFieldRegistration->SetNumberOfLevels( 3 );
  displacementFieldRegistration->SetMovingInitialTransform( compositeTransform );
  displacementFieldRegistration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );
  displacementFieldRegistration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  displacementFieldRegistration->SetMetric( correlationMetric );
  displacementFieldRegistration->SetLearningRate( atof( argv[6] ) );
  displacementFieldRegistration->SetNumberOfIterationsPerLevel( numberOfIterationsPerLevel );
  displacementFieldRegistration->SetTransformParametersAdaptorsPerLevel( adaptors );
  displacementFieldRegistration->SetGaussianSmoothingVarianceForTheUpdateField( varianceForUpdateField );
  displacementFieldRegistration->SetGaussianSmoothingVarianceForTheTotalField( varianceForTotalField );

  typedef CommandIterationUpdate<DisplacementFieldRegistrationType> DisplacementFieldCommandType;
  typename DisplacementFieldCommandType::Pointer DisplacementFieldObserver = DisplacementFieldCommandType::New();
  displacementFieldRegistration->AddObserver( itk::IterationEvent(), DisplacementFieldObserver );

  try
    {
    std::cout << "SyN registration" << std::endl;
    displacementFieldRegistration->Update();
    }
  catch( itk::ExceptionObject &e )
    {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  compositeTransform->AddTransform( outputTransform );

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

  std::string warpedMovingImageFileName = std::string( argv[4] ) + std::string( "MovingImageAfterSyN.nii.gz" );

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

  std::string displacementFieldFileName = std::string( argv[4] ) + std::string( "DisplacementField.nii.gz" );

  typedef itk::ImageFileWriter<DisplacementFieldType> DisplacementFieldWriterType;
  typename DisplacementFieldWriterType::Pointer displacementFieldWriter = DisplacementFieldWriterType::New();
  displacementFieldWriter->SetFileName( displacementFieldFileName.c_str() );
  displacementFieldWriter->SetInput( outputTransform->GetDisplacementField() );
  displacementFieldWriter->Update();

  return EXIT_SUCCESS;
}

int itkSyNImageRegistrationTest( int argc, char *argv[] )
{
  if ( argc < 5 )
    {
    std::cout << argv[0] << " imageDimension fixedImage movingImage outputPrefix numberOfDeformableIterations learningRate" << std::endl;
    exit( 1 );
    }

  switch( atoi( argv[1] ) )
   {
   case 2:
     PerformDisplacementFieldImageRegistration<2>( argc, argv );
     break;
   case 3:
     PerformDisplacementFieldImageRegistration<3>( argc, argv );
     break;
   default:
      std::cerr << "Unsupported dimension" << std::endl;
      exit( EXIT_FAILURE );
   }
  return EXIT_SUCCESS;
}

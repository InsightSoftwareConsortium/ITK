/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DeformableRegistration15.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
//
// This example illustrates a realistic pipeline for solving a full deformable registration problem.
// 
// First the two images are roughly aligned by using a transform
// initialization, then they are registered using a rigid transform, that in
// turn, is used to initialize a registration with an affine transform. The
// transform resulting from the affine registration is used as the bulk
// transform of a BSplineDeformableTransform. The deformable registration is
// computed, and finally the resulting transform is used to resample the moving
// image.
//
// Software Guide : EndLatex 

#include "itkImageRegistrationMethod.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkOrientedImage.h"

#include "itkTimeProbesCollectorBase.h"

#ifdef ITK_USE_REVIEW
#include "itkMemoryProbesCollectorBase.h"
#define itkProbesCreate()  \
  itk::TimeProbesCollectorBase chronometer; \
  itk::MemoryProbesCollectorBase memorymeter
#define itkProbesStart( text ) memorymeter.Start( text ); chronometer.Start( text )
#define itkProbesStop( text )  chronometer.Stop( text ); memorymeter.Stop( text  )
#define itkProbesReport( stream )  chronometer.Report( stream ); memorymeter.Report( stream  )
#else
#define itkProbesCreate()  \
  itk::TimeProbesCollectorBase chronometer
#define itkProbesStart( text ) chronometer.Start( text )
#define itkProbesStop( text )  chronometer.Stop( text )
#define itkProbesReport( stream )  chronometer.Report( stream )
#endif

//  Software Guide : BeginLatex
//  
//  The following are the most relevant headers to this example.
//
//  \index{itk::VersorRigid3DTransform!header}
//  \index{itk::AffineTransform!header}
//  \index{itk::BSplineDeformableTransform!header}
//  \index{itk::RegularStepGradientDescentOptimizer!header}
// 
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCenteredTransformInitializer.h"
#include "itkVersorRigid3DTransform.h"
#include "itkAffineTransform.h"
#include "itkBSplineDeformableTransform.h"
#include "itkRegularStepGradientDescentOptimizer.h"
// Software Guide : EndCodeSnippet

#include "itkBSplineResampleImageFunction.h"
#include "itkBSplineDecompositionImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"


#include "itkTransformFileReader.h"

//  The following section of code implements a Command observer
//  used to monitor the evolution of the registration process.
//
#include "itkCommand.h"
class CommandIterationUpdate : public itk::Command 
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef itk::SmartPointer<Self>   Pointer;
  itkNewMacro( Self );
protected:
  CommandIterationUpdate() {};
public:
  typedef itk::RegularStepGradientDescentOptimizer  OptimizerType;
  typedef   const OptimizerType *                   OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
    OptimizerPointer optimizer = 
      dynamic_cast< OptimizerPointer >( object );
    if( !(itk::IterationEvent().CheckEvent( &event )) )
      {
      return;
      }
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
    std::cout << std::endl;
    }
};

int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile outputImagefile  ";
    std::cerr << " [differenceOutputfile] [differenceBeforeRegistration] ";
    std::cerr << " [deformationField] ";
    std::cerr << " [useExplicitPDFderivatives ] [useCachingBSplineWeights ] ";
    std::cerr << " [filenameForFinalTransformParameters] ";
    std::cerr << " [numberOfGridNodesInsideImageInOneDimensionCoarse] ";
    std::cerr << " [numberOfGridNodesInsideImageInOneDimensionFine] ";
    std::cerr << " [maximumStepLength] [maximumNumberOfIterations]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }
  
  const    unsigned int    ImageDimension = 3;
  typedef  signed short    PixelType;

  typedef itk::OrientedImage< PixelType, ImageDimension >  FixedImageType;
  typedef itk::OrientedImage< PixelType, ImageDimension >  MovingImageType;


  const unsigned int SpaceDimension = ImageDimension;

  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;

  typedef itk::VersorRigid3DTransform< double > RigidTransformType;

  typedef itk::AffineTransform< double, SpaceDimension > AffineTransformType;

  typedef itk::BSplineDeformableTransform<
                            CoordinateRepType,
                            SpaceDimension,
                            SplineOrder >     DeformableTransformType;

  typedef itk::CenteredTransformInitializer< RigidTransformType, 
                                             FixedImageType, 
                                             MovingImageType 
                                                 >  TransformInitializerType;


  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;


  typedef itk::MattesMutualInformationImageToImageMetric< 
                                    FixedImageType, 
                                    MovingImageType >    MetricType;

  typedef itk:: LinearInterpolateImageFunction< 
                                    MovingImageType,
                                    double          >    InterpolatorType;

  typedef itk::ImageRegistrationMethod< 
                                    FixedImageType, 
                                    MovingImageType >    RegistrationType;

  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  

  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetInterpolator(  interpolator  );

  // Auxiliary identity transform.
  typedef itk::IdentityTransform<double,SpaceDimension> IdentityTransformType;
  IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();


  //
  //   Read the Fixed and Moving images.
  // 
  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  try
    {
    fixedImageReader->Update();
    movingImageReader->Update();
    }
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return EXIT_FAILURE;
    } 

  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();

  registration->SetFixedImage(  fixedImage   );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  //
  // Add a time and memory probes collector for profiling the computation time
  // of every stage.
  //
  itkProbesCreate();


  // 
  // Setup the metric parameters
  //
  metric->SetNumberOfHistogramBins( 50 );
  
  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
  
  const unsigned int numberOfPixels = fixedRegion.GetNumberOfPixels();

  metric->ReinitializeSeed( 76926294 );


  if( argc > 7 )
    {
    // Define whether to calculate the metric derivative by explicitly
    // computing the derivatives of the joint PDF with respect to the Transform
    // parameters, or doing it by progressively accumulating contributions from
    // each bin in the joint PDF.
    metric->SetUseExplicitPDFDerivatives( atoi( argv[7] ) );
    }

  if( argc > 8 )
    {
    // Define whether to cache the BSpline weights and indexes corresponding to
    // each one of the samples used to compute the metric. Enabling caching will
    // make the algorithm run faster but it will have a cost on the amount of memory
    // that needs to be allocated. This option is only relevant when using the 
    // BSplineDeformableTransform.
    metric->SetUseCachingOfBSplineWeights( atoi( argv[8] ) );
    }


  //
  //  Initialize a rigid transform by using Image Intensity Moments
  //
  TransformInitializerType::Pointer initializer = TransformInitializerType::New();

  RigidTransformType::Pointer  rigidTransform = RigidTransformType::New();

  initializer->SetTransform(   rigidTransform );
  initializer->SetFixedImage(  fixedImageReader->GetOutput() );
  initializer->SetMovingImage( movingImageReader->GetOutput() );
  initializer->MomentsOn();


  std::cout << "Starting Rigid Transform Initialization " << std::endl;
  itkProbesStart( "Rigid Initialization");
  initializer->InitializeTransform();
  itkProbesStop( "Rigid Initialization");
  std::cout << "Rigid Transform Initialization completed" << std::endl;
  std::cout << std::endl;

  registration->SetFixedImageRegion( fixedRegion );
  registration->SetInitialTransformParameters( rigidTransform->GetParameters() );

  registration->SetTransform( rigidTransform );

  //
  //  Define optimizer normaliztion to compensate for different dynamic range
  //  of rotations and translations.
  //
  typedef OptimizerType::ScalesType       OptimizerScalesType;
  OptimizerScalesType optimizerScales( rigidTransform->GetNumberOfParameters() );
  const double translationScale = 1.0 / 1000.0;

  optimizerScales[0] = 1.0;
  optimizerScales[1] = 1.0;
  optimizerScales[2] = 1.0;
  optimizerScales[3] = translationScale;
  optimizerScales[4] = translationScale;
  optimizerScales[5] = translationScale;

  optimizer->SetScales( optimizerScales );

  optimizer->SetMaximumStepLength( 0.2000  ); 
  optimizer->SetMinimumStepLength( 0.0001 );

  optimizer->SetNumberOfIterations( 200 );

  //
  // The rigid transform has 6 parameters we use therefore a few samples to run
  // this stage.
  //
  // Regulating the number of samples in the Metric is equivalent to performing
  // multi-resolution registration because it is indeed a sub-sampling of the
  // image.
  metric->SetNumberOfSpatialSamples( 10000L );

  //
  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );


  std::cout << "Starting Rigid Registration " << std::endl;

  try 
    { 
    itkProbesStart( "Rigid Registration" );
    registration->StartRegistration(); 
    itkProbesStop( "Rigid Registration" );
    std::cout << "Optimizer stop condition = "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return EXIT_FAILURE;
    } 

  std::cout << "Rigid Registration completed" << std::endl;
  std::cout << std::endl;

  rigidTransform->SetParameters( registration->GetLastTransformParameters() );


  //
  //  Perform Affine Registration
  // 
  AffineTransformType::Pointer  affineTransform = AffineTransformType::New();

  affineTransform->SetCenter( rigidTransform->GetCenter() );
  affineTransform->SetTranslation( rigidTransform->GetTranslation() );
  affineTransform->SetMatrix( rigidTransform->GetMatrix() );

  registration->SetTransform( affineTransform );
  registration->SetInitialTransformParameters( affineTransform->GetParameters() );

  optimizerScales = OptimizerScalesType( affineTransform->GetNumberOfParameters() );

  optimizerScales[0] = 1.0;
  optimizerScales[1] = 1.0;
  optimizerScales[2] = 1.0;
  optimizerScales[3] = 1.0;
  optimizerScales[4] = 1.0;
  optimizerScales[5] = 1.0;
  optimizerScales[6] = 1.0;
  optimizerScales[7] = 1.0;
  optimizerScales[8] = 1.0;

  optimizerScales[9]  = translationScale;
  optimizerScales[10] = translationScale;
  optimizerScales[11] = translationScale;

  optimizer->SetScales( optimizerScales );

  optimizer->SetMaximumStepLength( 0.2000  ); 
  optimizer->SetMinimumStepLength( 0.0001 );

  optimizer->SetNumberOfIterations( 200 );

  //
  // The Affine transform has 12 parameters we use therefore a more samples to run
  // this stage.
  //
  // Regulating the number of samples in the Metric is equivalent to performing
  // multi-resolution registration because it is indeed a sub-sampling of the
  // image.
  metric->SetNumberOfSpatialSamples( 50000L );


  std::cout << "Starting Affine Registration " << std::endl;

  try 
    { 
    itkProbesStart( "Affine Registration" );
    registration->StartRegistration(); 
    itkProbesStop( "Affine Registration" );
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return EXIT_FAILURE;
    } 

  std::cout << "Affine Registration completed" << std::endl;
  std::cout << std::endl;

  affineTransform->SetParameters( registration->GetLastTransformParameters() );


  //
  //  Perform Deformable Registration
  // 
  DeformableTransformType::Pointer  bsplineTransformCoarse = DeformableTransformType::New();

  unsigned int numberOfGridNodesInOneDimensionCoarse = 5;

  if( argc > 10 )
    {
    numberOfGridNodesInOneDimensionCoarse = atoi( argv[10] );
    }


  typedef DeformableTransformType::RegionType RegionType;
  RegionType bsplineRegion;
  RegionType::SizeType   gridSizeOnImage;
  RegionType::SizeType   gridBorderSize;
  RegionType::SizeType   totalGridSize;

  gridSizeOnImage.Fill( numberOfGridNodesInOneDimensionCoarse );
  gridBorderSize.Fill( SplineOrder );    // Border for spline order = 3 ( 1 lower, 2 upper )
  totalGridSize = gridSizeOnImage + gridBorderSize;

  bsplineRegion.SetSize( totalGridSize );

  typedef DeformableTransformType::SpacingType SpacingType;
  SpacingType spacing = fixedImage->GetSpacing();

  typedef DeformableTransformType::OriginType OriginType;
  OriginType origin = fixedImage->GetOrigin();

  FixedImageType::SizeType fixedImageSize = fixedRegion.GetSize();

  for(unsigned int r=0; r<ImageDimension; r++)
    {
    spacing[r] *= static_cast<double>(fixedImageSize[r] - 1)  / 
                  static_cast<double>(gridSizeOnImage[r] - 1);
    }

  FixedImageType::DirectionType gridDirection = fixedImage->GetDirection();
  SpacingType gridOriginOffset = gridDirection * spacing;

  OriginType gridOrigin = origin - gridOriginOffset; 

  bsplineTransformCoarse->SetGridSpacing( spacing );
  bsplineTransformCoarse->SetGridOrigin( gridOrigin );
  bsplineTransformCoarse->SetGridRegion( bsplineRegion );
  bsplineTransformCoarse->SetGridDirection( gridDirection );
  
  bsplineTransformCoarse->SetBulkTransform( affineTransform );

  typedef DeformableTransformType::ParametersType     ParametersType;

  unsigned int numberOfBSplineParameters = bsplineTransformCoarse->GetNumberOfParameters();


  optimizerScales = OptimizerScalesType( numberOfBSplineParameters );
  optimizerScales.Fill( 1.0 );

  optimizer->SetScales( optimizerScales );


  ParametersType initialDeformableTransformParameters( numberOfBSplineParameters );
  initialDeformableTransformParameters.Fill( 0.0 );

  bsplineTransformCoarse->SetParameters( initialDeformableTransformParameters );

  registration->SetInitialTransformParameters( bsplineTransformCoarse->GetParameters() );
  registration->SetTransform( bsplineTransformCoarse );

  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  Next we set the parameters of the RegularStepGradientDescentOptimizer object. 
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumStepLength( 10.0 );
  optimizer->SetMinimumStepLength(  0.01 );

  optimizer->SetRelaxationFactor( 0.7 );
  optimizer->SetNumberOfIterations( 50 );
  // Software Guide : EndCodeSnippet


  // Optionally, get the step length from the command line arguments
  if( argc > 11 )
    {
    optimizer->SetMaximumStepLength( atof( argv[12] ) );
    }

  // Optionally, get the number of iterations from the command line arguments
  if( argc > 12 )
    {
    optimizer->SetNumberOfIterations( atoi( argv[13] ) );
    }


  //
  // The BSpline transform has a large number of parameters, we use therefore a
  // much larger number of samples to run this stage.
  //
  // Regulating the number of samples in the Metric is equivalent to performing
  // multi-resolution registration because it is indeed a sub-sampling of the
  // image.
  metric->SetNumberOfSpatialSamples( numberOfBSplineParameters * 100 );

  std::cout << std::endl << "Starting Deformable Registration Coarse Grid" << std::endl;

  try 
    { 
    itkProbesStart( "Deformable Registration Coarse" );
    registration->StartRegistration(); 
    itkProbesStop( "Deformable Registration Coarse" );
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return EXIT_FAILURE;
    } 
  
  std::cout << "Deformable Registration Coarse Grid completed" << std::endl;
  std::cout << std::endl;

  OptimizerType::ParametersType finalParameters = 
                    registration->GetLastTransformParameters();

  bsplineTransformCoarse->SetParameters( finalParameters );

  //  Software Guide : BeginLatex
  //  
  //  Once the registration has finished with the low resolution grid, we
  //  proceed to instantiate a higher resolution
  //  \code{BSplineDeformableTransform}.
  //  
  //  Software Guide : EndLatex 

  DeformableTransformType::Pointer  bsplineTransformFine = DeformableTransformType::New();

  unsigned int numberOfGridNodesInOneDimensionFine = 20;

  if( argc > 11 )
    {
    numberOfGridNodesInOneDimensionFine = atoi( argv[11] );
    }

  RegionType::SizeType   gridHighSizeOnImage;
  gridHighSizeOnImage.Fill( numberOfGridNodesInOneDimensionFine );
  totalGridSize = gridHighSizeOnImage + gridBorderSize;

  bsplineRegion.SetSize( totalGridSize );

  SpacingType spacingHigh = fixedImage->GetSpacing();
  OriginType  originHigh  = fixedImage->GetOrigin();

  for(unsigned int rh=0; rh<ImageDimension; rh++)
    {
    spacingHigh[rh] *= static_cast<double>(fixedImageSize[rh] - 1)  / 
      static_cast<double>(gridHighSizeOnImage[rh] - 1);
    originHigh[rh] -= spacingHigh[rh]; 
    }

  SpacingType gridOriginOffsetHigh = gridDirection * spacingHigh;

  OriginType gridOriginHigh = origin - gridOriginOffsetHigh; 


  bsplineTransformFine->SetGridSpacing( spacingHigh );
  bsplineTransformFine->SetGridOrigin( gridOriginHigh );
  bsplineTransformFine->SetGridRegion( bsplineRegion );
  bsplineTransformFine->SetGridDirection( gridDirection );

  bsplineTransformFine->SetBulkTransform( affineTransform );

  numberOfBSplineParameters = bsplineTransformFine->GetNumberOfParameters();

  ParametersType parametersHigh( numberOfBSplineParameters );
  parametersHigh.Fill( 0.0 );

  //  Software Guide : BeginLatex
  //  
  //  Now we need to initialize the BSpline coefficients of the higher resolution
  //  transform. This is done by first computing the actual deformation field 
  //  at the higher resolution from the lower resolution BSpline coefficients. 
  //  Then a BSpline decomposition is done to obtain the BSpline coefficient of 
  //  the higher resolution transform.
  //  
  //  Software Guide : EndLatex 

  unsigned int counter = 0;

  for ( unsigned int k = 0; k < SpaceDimension; k++ )
    {
    typedef DeformableTransformType::ImageType ParametersImageType;
    typedef itk::ResampleImageFilter<ParametersImageType,ParametersImageType> ResamplerType;
    ResamplerType::Pointer upsampler = ResamplerType::New();

    typedef itk::BSplineResampleImageFunction<ParametersImageType,double> FunctionType;
    FunctionType::Pointer function = FunctionType::New();

    upsampler->SetInput( bsplineTransformCoarse->GetCoefficientImage()[k] );
    upsampler->SetInterpolator( function );
    upsampler->SetTransform( identityTransform );
    upsampler->SetSize( bsplineTransformFine->GetGridRegion().GetSize() );
    upsampler->SetOutputSpacing( bsplineTransformFine->GetGridSpacing() );
    upsampler->SetOutputOrigin( bsplineTransformFine->GetGridOrigin() );

    typedef itk::BSplineDecompositionImageFilter<ParametersImageType,ParametersImageType>
      DecompositionType;
    DecompositionType::Pointer decomposition = DecompositionType::New();

    decomposition->SetSplineOrder( SplineOrder );
    decomposition->SetInput( upsampler->GetOutput() );
    decomposition->Update();

    ParametersImageType::Pointer newCoefficients = decomposition->GetOutput();

    // copy the coefficients into the parameter array
    typedef itk::ImageRegionIterator<ParametersImageType> Iterator;
    Iterator it( newCoefficients, bsplineTransformFine->GetGridRegion() );
    while ( !it.IsAtEnd() )
      {
      parametersHigh[ counter++ ] = it.Get();
      ++it;
      }

    }


  optimizerScales = OptimizerScalesType( numberOfBSplineParameters );
  optimizerScales.Fill( 1.0 );

  optimizer->SetScales( optimizerScales );

  bsplineTransformFine->SetParameters( parametersHigh );

  //  Software Guide : BeginLatex
  //  
  //  We now pass the parameters of the high resolution transform as the initial
  //  parameters to be used in a second stage of the registration process.
  //
  //  Software Guide : EndLatex 

  std::cout << "Starting Registration with high resolution transform" << std::endl;

  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransformParameters( bsplineTransformFine->GetParameters() );
  registration->SetTransform( bsplineTransformFine );

  //
  // The BSpline transform at fine scale has a very large number of parameters,
  // we use therefore a much larger number of samples to run this stage. In this
  // case, however, the number of transform parameters is closer to the number
  // of pixels in the image. Therefore we use the geometric mean of the two numbers
  // to ensure that the number of samples is larger than the number of transform
  // parameters and smaller than the number of samples.
  //
  // Regulating the number of samples in the Metric is equivalent to performing
  // multi-resolution registration because it is indeed a sub-sampling of the
  // image.
  const unsigned long numberOfSamples = 
     static_cast<unsigned long>(
       vcl_sqrt( static_cast<double>( numberOfBSplineParameters ) *
                 static_cast<double>( numberOfPixels ) ) );

  metric->SetNumberOfSpatialSamples( numberOfSamples );


  try 
    { 
    itkProbesStart( "Deformable Registration Fine" );
    registration->StartRegistration(); 
    itkProbesStop( "Deformable Registration Fine" );
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return EXIT_FAILURE;
    } 
  // Software Guide : EndCodeSnippet


  std::cout << "Deformable Registration Fine Grid completed" << std::endl;
  std::cout << std::endl;


  // Report the time and memory taken by the registration
  itkProbesReport( std::cout );

  finalParameters = registration->GetLastTransformParameters();

  bsplineTransformFine->SetParameters( finalParameters );


  typedef itk::ResampleImageFilter< 
                            MovingImageType, 
                            FixedImageType >    ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( bsplineTransformFine );
  resample->SetInput( movingImageReader->GetOutput() );

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );

  // This value is set to zero in order to make easier to perform
  // regression testing in this example. However, for didactic 
  // exercise it will be better to set it to a medium gray value
  // such as 100 or 128.
  resample->SetDefaultPixelValue( 0 );
  
  typedef  signed short  OutputPixelType;

  typedef itk::Image< OutputPixelType, ImageDimension > OutputImageType;
  
  typedef itk::CastImageFilter< 
                        FixedImageType,
                        OutputImageType > CastFilterType;
                    
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;


  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();


  writer->SetFileName( argv[3] );


  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput()   );

  std::cout << "Writing resampled moving image...";

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return EXIT_FAILURE;
    } 
 
  std::cout << " Done!" << std::endl;


  typedef itk::SquaredDifferenceImageFilter< 
                                  FixedImageType, 
                                  FixedImageType, 
                                  OutputImageType > DifferenceFilterType;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( difference->GetOutput() );  
  

  // Compute the difference image between the 
  // fixed and resampled moving image.
  if( argc > 4 )
    {
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( resample->GetOutput() );
    writer2->SetFileName( argv[4] );

    std::cout << "Writing difference image after registration...";

    try
      {
      writer2->Update();
      }
    catch( itk::ExceptionObject & err ) 
      { 
      std::cerr << "ExceptionObject caught !" << std::endl; 
      std::cerr << err << std::endl; 
      return EXIT_FAILURE;
      } 

    std::cout << " Done!" << std::endl;
    }


  // Compute the difference image between the 
  // fixed and moving image before registration.
  if( argc > 5 )
    {
    writer2->SetFileName( argv[5] );
    difference->SetInput1( fixedImageReader->GetOutput() );
    resample->SetTransform( identityTransform );

    std::cout << "Writing difference image before registration...";

    try
      {
      writer2->Update();
      }
    catch( itk::ExceptionObject & err ) 
      { 
      std::cerr << "ExceptionObject caught !" << std::endl; 
      std::cerr << err << std::endl; 
      return EXIT_FAILURE;
      } 

    std::cout << " Done!" << std::endl;
    }

  // Generate the explicit deformation field resulting from 
  // the registration.
  if( argc > 6 )
    {

    typedef itk::Vector< float, ImageDimension >      VectorType;
    typedef itk::Image< VectorType, ImageDimension >  DeformationFieldType;

    DeformationFieldType::Pointer field = DeformationFieldType::New();
    field->SetRegions( fixedRegion );
    field->SetOrigin( fixedImage->GetOrigin() );
    field->SetSpacing( fixedImage->GetSpacing() );
    field->SetDirection( fixedImage->GetDirection() );
    field->Allocate();

    typedef itk::ImageRegionIterator< DeformationFieldType > FieldIterator;
    FieldIterator fi( field, fixedRegion );

    fi.GoToBegin();

    DeformableTransformType::InputPointType  fixedPoint;
    DeformableTransformType::OutputPointType movingPoint;
    DeformationFieldType::IndexType index;

    VectorType displacement;

    while( ! fi.IsAtEnd() )
      {
      index = fi.GetIndex();
      field->TransformIndexToPhysicalPoint( index, fixedPoint );
      movingPoint = bsplineTransformFine->TransformPoint( fixedPoint );
      displacement = movingPoint - fixedPoint;
      fi.Set( displacement );
      ++fi;
      }

    typedef itk::ImageFileWriter< DeformationFieldType >  FieldWriterType;
    FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

    fieldWriter->SetInput( field );

    fieldWriter->SetFileName( argv[6] );

    std::cout << "Writing deformation field ...";

    try
      {
      fieldWriter->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << " Done!" << std::endl;
    }

  // Optionally, save the transform parameters in a file
  if( argc > 9 )
    {
    std::cout << "Writing transform parameter file ...";
    std::ofstream parametersFile;
    parametersFile.open( argv[9] );
    parametersFile << finalParameters << std::endl;
    parametersFile.close();
    std::cout << " Done!" << std::endl;
    }

  return EXIT_SUCCESS;
}

#undef itkProbesCreate
#undef itkProbesStart
#undef itkProbesStop
#undef itkProbesReport

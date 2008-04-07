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
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );
protected:
  CommandIterationUpdate() {};
public:
  typedef itk::RegularStepGradientDescentOptimizer     OptimizerType;
  typedef   const OptimizerType   *    OptimizerPointer;

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
    std::cerr << " [numberOfGridNodesInsideImageInOneDimension] ";
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

  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
  
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
  DeformableTransformType::Pointer  bsplineTransform = DeformableTransformType::New();

  unsigned int numberOfGridNodesInOneDimension = 5;

  if( argc > 10 )
    {
    numberOfGridNodesInOneDimension = atoi( argv[10] );
    }


  typedef DeformableTransformType::RegionType RegionType;
  RegionType bsplineRegion;
  RegionType::SizeType   gridSizeOnImage;
  RegionType::SizeType   gridBorderSize;
  RegionType::SizeType   totalGridSize;

  gridSizeOnImage.Fill( numberOfGridNodesInOneDimension );
  gridBorderSize.Fill( SplineOrder );    // Border for spline order = 3 ( 1 lower, 2 upper )
  totalGridSize = gridSizeOnImage + gridBorderSize;

  bsplineRegion.SetSize( totalGridSize );

  typedef DeformableTransformType::SpacingType SpacingType;
  SpacingType spacing = fixedImage->GetSpacing();

  typedef DeformableTransformType::OriginType OriginType;
  OriginType origin = fixedImage->GetOrigin();;

  FixedImageType::SizeType fixedImageSize = fixedRegion.GetSize();

  for(unsigned int r=0; r<ImageDimension; r++)
    {
    spacing[r] *= floor( static_cast<double>(fixedImageSize[r] - 1)  / 
                  static_cast<double>(gridSizeOnImage[r] - 1) );
    origin[r]  -=  spacing[r]; 
    }

  bsplineTransform->SetGridSpacing( spacing );
  bsplineTransform->SetGridOrigin( origin );
  bsplineTransform->SetGridRegion( bsplineRegion );
  
  bsplineTransform->SetBulkTransform( affineTransform );

  typedef DeformableTransformType::ParametersType     ParametersType;

  const unsigned int numberOfParameters = bsplineTransform->GetNumberOfParameters();


  optimizerScales = OptimizerScalesType( numberOfParameters );
  optimizerScales.Fill( 1.0 );

  optimizer->SetScales( optimizerScales );


  ParametersType initialDeformableTransformParameters( numberOfParameters );
  initialDeformableTransformParameters.Fill( 0.0 );

  bsplineTransform->SetParameters( initialDeformableTransformParameters );

  registration->SetInitialTransformParameters( bsplineTransform->GetParameters() );
  registration->SetTransform( bsplineTransform );

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
    optimizer->SetMaximumStepLength( atof( argv[11] ) );
    }

  // Optionally, get the number of iterations from the command line arguments
  if( argc > 12 )
    {
    optimizer->SetNumberOfIterations( atoi( argv[12] ) );
    }


  metric->SetNumberOfHistogramBins( 50 );
  
  const unsigned int numberOfSamples = 
    static_cast<unsigned int>( fixedRegion.GetNumberOfPixels() * 20.0 / 100.0 );

  metric->SetNumberOfSpatialSamples( numberOfSamples );
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


  std::cout << std::endl << "Starting Deformable Registration" << std::endl;

  try 
    { 
    itkProbesStart( "Deformable Registration" );
    registration->StartRegistration(); 
    itkProbesStop( "Deformable Registration" );
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return EXIT_FAILURE;
    } 
  
  OptimizerType::ParametersType finalParameters = 
                    registration->GetLastTransformParameters();


  // Report the time and memory taken by the registration
  itkProbesReport( std::cout );

  bsplineTransform->SetParameters( finalParameters );


  typedef itk::ResampleImageFilter< 
                            MovingImageType, 
                            FixedImageType >    ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( bsplineTransform );
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
    }


  // Compute the difference image between the 
  // fixed and moving image before registration.
  if( argc > 5 )
    {
    writer2->SetFileName( argv[5] );
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( movingImageReader->GetOutput() );
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
    }



  // Generate the explicit deformation field resulting from 
  // the registration.
  if( argc > 6 )
    {

    typedef itk::Vector< float, ImageDimension >  VectorType;
    typedef itk::Image< VectorType, ImageDimension >  DeformationFieldType;

    DeformationFieldType::Pointer field = DeformationFieldType::New();
    field->SetRegions( fixedRegion );
    field->SetOrigin( fixedImage->GetOrigin() );
    field->SetSpacing( fixedImage->GetSpacing() );
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
      movingPoint = bsplineTransform->TransformPoint( fixedPoint );
      displacement = movingPoint - fixedPoint;
      fi.Set( displacement );
      ++fi;
      }

    typedef itk::ImageFileWriter< DeformationFieldType >  FieldWriterType;
    FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

    fieldWriter->SetInput( field );

    fieldWriter->SetFileName( argv[6] );
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
    }

  // Optionally, save the transform parameters in a file
  if( argc > 9 )
    {
    std::ofstream parametersFile;
    parametersFile.open( argv[9] );
    parametersFile << finalParameters << std::endl;
    parametersFile.close();
    }

  return EXIT_SUCCESS;
}


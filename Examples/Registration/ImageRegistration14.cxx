/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration14.cxx
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
//  This example illustrates how to do registration with a 2D Rigid Transform
//  and with the Normalized Mutual Information metric.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethod.h"

#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredTransformInitializer.h"

#include "itkNormalizedMutualInformationHistogramImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkNormalVariateGenerator.h" 

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"


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
  CommandIterationUpdate() {m_LastMetricValue = 0;}
public:
  typedef itk::OnePlusOneEvolutionaryOptimizer     OptimizerType;
  typedef   const OptimizerType   *    OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
      Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
      OptimizerPointer optimizer = 
        dynamic_cast< OptimizerPointer >( object );
      if( typeid( event ) != typeid( itk::IterationEvent ) )
        {
        return;
        }
      double currentValue = optimizer->GetValue();
      // Only print out when the Metric value changes
      if( fabs( m_LastMetricValue - currentValue ) > 1e-7 )
        { 
        std::cout << optimizer->GetCurrentIteration() << "   ";
        std::cout << currentValue << "   ";
        std::cout << optimizer->GetFrobeniusNorm() << "   ";
        std::cout << optimizer->GetCurrentPosition() << std::endl;
        m_LastMetricValue = currentValue;
        }
    }
private:
  double m_LastMetricValue;
};


int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile [numberOfHistogramBins] ";
    std::cerr << "[initialRadius] [epsilon]" << std::endl;
    std::cerr << "[initialAngle(radians)] [initialTx] [initialTy]" << std::endl;
    return 1;
    }
  
  const    unsigned int    Dimension = 2;
  typedef  unsigned char   PixelType;
  
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::CenteredRigid2DTransform< double > TransformType;

  typedef itk::OnePlusOneEvolutionaryOptimizer       OptimizerType;
  typedef itk::LinearInterpolateImageFunction< 
                                    MovingImageType,
                                    double             > InterpolatorType;
  typedef itk::ImageRegistrationMethod< 
                                    FixedImageType, 
                                    MovingImageType    > RegistrationType;


  typedef itk::NormalizedMutualInformationHistogramImageToImageMetric< 
                                          FixedImageType, 
                                          MovingImageType >    MetricType;



  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );
  


  MetricType::Pointer metric = MetricType::New();
  registration->SetMetric( metric  );


  unsigned int numberOfHistogramBins = 32;
  if( argc > 4 )
    {
    numberOfHistogramBins = atoi( argv[4] );
    std::cout << "Using " << numberOfHistogramBins << " Histogram bins" << std::endl;
    }
  MetricType::HistogramType::SizeType histogramSize;
  histogramSize[0] = numberOfHistogramBins;
  histogramSize[1] = numberOfHistogramBins;
  metric->SetHistogramSize( histogramSize );
 

  const unsigned int numberOfParameters = transform->GetNumberOfParameters();

  typedef MetricType::ScalesType ScalesType;
  ScalesType scales( numberOfParameters );

  scales.Fill( 1.0 );
    
  metric->SetDerivativeStepLengthScales(scales);




  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  fixedImageReader->Update();
  movingImageReader->Update();

  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();

  registration->SetFixedImageRegion( fixedImage->GetBufferedRegion() );


  typedef itk::CenteredTransformInitializer< 
                                    TransformType, 
                                    FixedImageType, 
                                    MovingImageType >  TransformInitializerType;
  TransformInitializerType::Pointer initializer = TransformInitializerType::New();

  initializer->SetTransform(   transform );
  initializer->SetFixedImage(  fixedImageReader->GetOutput() );
  initializer->SetMovingImage( movingImageReader->GetOutput() );
  initializer->GeometryOn();
  initializer->InitializeTransform();


  typedef RegistrationType::ParametersType ParametersType;
  

  double initialAngle = 0.0;

  if( argc > 7 )
    {
    initialAngle = atof( argv[7] );
    }

  transform->SetAngle( initialAngle );

  TransformType::OutputVectorType initialTranslation;

  initialTranslation = transform->GetTranslation();

  if( argc > 9 )
    {
    initialTranslation[0] += atof( argv[8] );
    initialTranslation[1] += atof( argv[9] );
    }

  transform->SetTranslation( initialTranslation );


  ParametersType initialParameters =  transform->GetParameters();

  registration->SetInitialTransformParameters( initialParameters );

  std::cout << "Initial transform parameters = ";
  std::cout << initialParameters << std::endl;

  typedef OptimizerType::ScalesType       OptimizerScalesType;
  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );

  FixedImageType::RegionType region = fixedImage->GetLargestPossibleRegion();

  FixedImageType::SizeType size = region.GetSize();

  FixedImageType::SpacingType spacing = fixedImage->GetSpacing();

  optimizerScales[0] = 1.0 / 0.1;  // make angle move slowly
  optimizerScales[1] = 10000.0;    // prevent the center from moving
  optimizerScales[2] = 10000.0;    // prevent the center from moving
  optimizerScales[3] = 1.0 / ( 0.1 * size[0] * spacing[0] );
  optimizerScales[4] = 1.0 / ( 0.1 * size[1] * spacing[1] );

  std::cout << "optimizerScales = " << optimizerScales << std::endl;
    
  optimizer->SetScales( optimizerScales );


  typedef itk::Statistics::NormalVariateGenerator  GeneratorType;

  GeneratorType::Pointer generator = GeneratorType::New();
  generator->Initialize(12345);

  optimizer->MaximizeOn();

  optimizer->SetNormalVariateGenerator( generator );

  double initialRadius = 0.05;

  if( argc > 5 )
    {
    initialRadius = atof( argv[5] );
    std::cout << "Using initial radius = " << initialRadius << std::endl;
    }
  optimizer->Initialize( initialRadius );

  double epsilon = 0.001;

  if( argc > 6 )
    {
    epsilon = atof( argv[6] );
    std::cout << "Using epsilon = " << epsilon << std::endl;
    }
  optimizer->SetEpsilon( epsilon );

  optimizer->SetMaximumIteration( 2000 );


  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );


  try 
    { 
    registration->StartRegistration(); 
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
    } 

  typedef RegistrationType::ParametersType ParametersType;

  ParametersType finalParameters = registration->GetLastTransformParameters();
  
  const double finalAngle           = finalParameters[0];
  const double finalRotationCenterX = finalParameters[1];
  const double finalRotationCenterY = finalParameters[2];
  const double finalTranslationX    = finalParameters[3];
  const double finalTranslationY    = finalParameters[4];

  unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  
  double bestValue = optimizer->GetValue();


  // Print out results
  //

  const double finalAngleInDegrees = finalAngle * 45.0 / atan(1.0);

  std::cout << "Result = " << std::endl;
  std::cout << " Angle (radians) " << finalAngle  << std::endl;
  std::cout << " Angle (degrees) " << finalAngleInDegrees  << std::endl;
  std::cout << " Center X      = " << finalRotationCenterX  << std::endl;
  std::cout << " Center Y      = " << finalRotationCenterY  << std::endl;
  std::cout << " Translation X = " << finalTranslationX  << std::endl;
  std::cout << " Translation Y = " << finalTranslationY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;


  typedef itk::ResampleImageFilter< 
                            MovingImageType, 
                            FixedImageType >    ResampleFilterType;

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters( finalParameters );

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( finalTransform );
  resample->SetInput( movingImageReader->GetOutput() );

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetDefaultPixelValue( 100 );


  typedef itk::Image< PixelType, Dimension > OutputImageType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer      writer =  WriterType::New();

  writer->SetFileName( argv[3] );

  writer->SetInput( resample->GetOutput() );
  writer->Update();

// Software Guide : EndCodeSnippet



  return 0;
}


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

// Software Guide : BeginLatex
//
//  This example illustrates how to do registration with a 2D Rigid Transform
//  and with MutualInformation metric.
//
// Software Guide : EndLatex


#include "itkImageRegistrationMethodv4.h"

#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredTransformInitializer.h"

// Software Guide : BeginCodeSnippet
#include "itkMattesMutualInformationImageToImageMetricv4.h"
// Software Guide : EndCodeSnippet

#include "itkRegularStepGradientDescentOptimizerv4.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"


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
  typedef itk::SmartPointer<Self>   Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate() {};

public:
  typedef itk::RegularStepGradientDescentOptimizerv4<double>  OptimizerType;
  typedef   const OptimizerType *                             OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );
    if( ! itk::IterationEvent().CheckEvent( &event ) )
      {
      return;
      }
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
    std::cout << optimizer->GetCurrentPosition() << std::endl;
    }
};


int main( int argc, char *argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile " << std::endl;
    return EXIT_FAILURE;
    }

  const    unsigned int    Dimension = 2;
  typedef  float           PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  // Software Guide : BeginLatex
  //
  // The CenteredRigid2DTransform applies a rigid transform in 2D space.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredRigid2DTransform< double >  TransformType;
  // Software Guide : EndCodeSnippet

  typedef itk::RegularStepGradientDescentOptimizerv4<double> OptimizerType;

  typedef itk::ImageRegistrationMethodv4<
                                    FixedImageType,
                                    MovingImageType,
                                    TransformType >           RegistrationType;


  // Software Guide : BeginCodeSnippet
  typedef itk::MattesMutualInformationImageToImageMetricv4<
                                          FixedImageType,
                                          MovingImageType >   MetricType;
  // Software Guide : EndCodeSnippet

  TransformType::Pointer      transform     = TransformType::New();
  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  registration->SetOptimizer(     optimizer     );
  registration->SetMetric( metric  );

  // For consistent results when regression testing.
  registration->MetricSamplingReinitializeSeed(121212);

  // Software Guide : BeginCodeSnippet
  metric->SetNumberOfHistogramBins( 20 );


  double samplingPercentage = 0.20;
  registration->SetMetricSamplingPercentage( samplingPercentage );

  RegistrationType::MetricSamplingStrategyType  samplingStrategy  =
                                                      RegistrationType::RANDOM;
  registration->SetMetricSamplingStrategy( samplingStrategy );
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  fixedImageReader->Update();

  // Software Guide : BeginLatex
  //
  // The \doxygen{CenteredRigid2DTransform} is initialized with 5 parameters,
  // indicating the angle of rotation, the center coordinates and the
  // translation to be applied after rotation. The initialization is done
  // by the \doxygen{CenteredTransformInitializer}.
  // The transform can operate in two modes, the first of which assumes that the
  // anatomical objects to be registered are centered in their respective
  // images. Hence the best initial guess for the registration is the one
  // that superimposes those two centers.
  // This second approach assumes that the moments of the anatomical
  // objects are similar for both images and hence the best initial guess
  // for registration is to superimpose both mass centers. The center of
  // mass is computed from the moments obtained from the gray level values.
  // Here we adopt the first approach. The \code{GeometryOn()} method
  // toggles between the approaches.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredTransformInitializer<
    TransformType,
    FixedImageType,
    MovingImageType > TransformInitializerType;
  TransformInitializerType::Pointer initializer
    = TransformInitializerType::New();
  initializer->SetTransform(   transform );

  initializer->SetFixedImage(  fixedImageReader->GetOutput() );
  initializer->SetMovingImage( movingImageReader->GetOutput() );
  initializer->GeometryOn();
  initializer->InitializeTransform();
  // Software Guide : EndCodeSnippet

  transform->SetAngle( 0.0 );

  registration->SetInitialTransform( transform );
  registration->InPlaceOn();

  // Software Guide : BeginLatex
  //
  // The optimizer scales the metrics (the gradient in this case) by the
  // scales during each iteration. Therefore, a large value of the center scale
  // will prevent movement along the center during optimization. Here we
  // assume that the fixed and moving images are likely to be related by
  // a translation.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef OptimizerType::ScalesType       OptimizerScalesType;
  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );

  const double translationScale = 1.0 / 128.0;
  const double centerScale      = 1000.0; // prevents it from moving
                                            // during the optimization
  optimizerScales[0] = 1.0;
  optimizerScales[1] = centerScale;
  optimizerScales[2] = centerScale;
  optimizerScales[3] = translationScale;
  optimizerScales[4] = translationScale;

  optimizer->SetScales( optimizerScales );

  optimizer->SetLearningRate( 0.5 );
  optimizer->SetMinimumStepLength( 0.0001 );
  optimizer->SetNumberOfIterations( 400 );
  // Software Guide : EndCodeSnippet

  // One level registration process without shrinking and smoothing.
  //
  const unsigned int numberOfLevels = 1;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 1 );
  shrinkFactorsPerLevel[0] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 1 );
  smoothingSigmasPerLevel[0] = 0;

  registration->SetNumberOfLevels ( numberOfLevels );
  registration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  registration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );


  try
    {
    registration->Update();
    std::cout << "Optimizer stop condition = "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  typedef TransformType::ParametersType ParametersType;
  ParametersType finalParameters = transform->GetParameters();

  const double finalAngle           = finalParameters[0];
  const double finalRotationCenterX = finalParameters[1];
  const double finalRotationCenterY = finalParameters[2];
  const double finalTranslationX    = finalParameters[3];
  const double finalTranslationY    = finalParameters[4];

  unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  double bestValue = optimizer->GetValue();

  // Print out results
  //

  const double finalAngleInDegrees = finalAngle * 180 / itk::Math::pi;

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

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( transform );
  resample->SetInput( movingImageReader->GetOutput() );

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 100 );

  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::CastImageFilter<
                        FixedImageType,
                        OutputImageType > CastFilterType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();

  writer->SetFileName( argv[3] );

  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();

  return EXIT_SUCCESS;
}

//  Software Guide : BeginLatex
//
//  Let's execute this example over some of the images provided in
//  \code{Examples/Data}, for example:
//
//  \begin{itemize}
//  \item \code{BrainProtonDensitySlice.png}
//  \item \code{BrainProtonDensitySliceR10X13Y17.png}
//  \end{itemize}
//
//  The second image is the result of intentionally rotating the first
//  image by $10$ degrees and shifting it $13mm$ in $X$ and $17mm$ in
//  $Y$. Both images have unit-spacing and are shown in Figure
//  \ref{fig:FixedMovingImageRegistration5}. The example
//  yielded the following results.
//
//  \begin{verbatim}
//
//  Angle (radians) 0.174585
//  Angle (degrees) 10.003
//  Center X      = 110
//  Center Y      = 128
//  Translation X = 13.09
//  Translation Y = 15.91
//
//  \end{verbatim}
//
//  These values match the true misalignment introduced in the moving image.
//
//  Software Guide : EndLatex

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


#include "itkImageRegistrationMethod.h"

#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredTransformInitializer.h"

// Software Guide : BeginCodeSnippet
#include "itkMattesMutualInformationImageToImageMetric.h"
// Software Guide : EndCodeSnippet

#include "itkRegularStepGradientDescentOptimizer.h"
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
  typedef itk::RegularStepGradientDescentOptimizer  OptimizerType;
  typedef   const OptimizerType *                   OptimizerPointer;

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
    std::cerr << "outputImagefile ";
    std::cerr << "[useExplicitPDFderivatives ] ";
    std::cerr << "[useCachingBSplineWeights ] " << std::endl;
    return EXIT_FAILURE;
    }

  const    unsigned int    Dimension = 2;
  typedef  unsigned char   PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  // Software Guide : BeginLatex
  // The CenteredRigid2DTransform applies a rigid transform in 2D space.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredRigid2DTransform< double >  TransformType;
  typedef itk::RegularStepGradientDescentOptimizer OptimizerType;
  // Software Guide : EndCodeSnippet

  typedef itk::LinearInterpolateImageFunction<
                                    MovingImageType,
                                    double             > InterpolatorType;
  typedef itk::ImageRegistrationMethod<
                                    FixedImageType,
                                    MovingImageType    > RegistrationType;


  // Software Guide : BeginCodeSnippet
  typedef itk::MattesMutualInformationImageToImageMetric<
                                          FixedImageType,
                                          MovingImageType >    MetricType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  // Software Guide : EndCodeSnippet
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );


  MetricType::Pointer metric = MetricType::New();
  registration->SetMetric( metric  );


  metric->SetNumberOfHistogramBins( 20 );
  metric->SetNumberOfSpatialSamples( 10000 );

  // For consistent results when regression testing.
  metric->ReinitializeSeed( 121212 );


  if( argc > 4 )
    {
    // Define whether to calculate the metric derivative by explicitly
    // computing the derivatives of the joint PDF with respect to the Transform
    // parameters, or doing it by progressively accumulating contributions from
    // each bin in the joint PDF.
    metric->SetUseExplicitPDFDerivatives( atoi( argv[4] ) );
    }

  if( argc > 5 )
    {
    // Define whether to cache the BSpline weights and indexes corresponding to
    // each one of the samples used to compute the metric. Enabling caching will
    // make the algorithm run faster but it will have a cost on the amount of memory
    // that needs to be allocated. This option is only relevant when using the
    // BSplineTransform.
    metric->SetUseCachingOfBSplineWeights( atoi( argv[5] ) );
    }


  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  fixedImageReader->Update();

  registration->SetFixedImageRegion(
       fixedImageReader->GetOutput()->GetBufferedRegion() );

  // Software Guide : BeginLatex
  // The \doxygen{CenteredRigid2DTransform} is initialized by 5 parameters,
  // indicating the angle of rotation, the center coordinates and the
  // translation to be applied after rotation. The initialization is done
  // by the \doxygen{CenteredTransformInitializer}.
  // The transform can operate in two modes, one assumes that the
  // anatomical objects to be registered are centered in their respective
  // images. Hence the best initial guess for the registration is the one
  // that superimposes those two centers.
  // This second approach assumes that the moments of the anatomical
  // objects are similar for both images and hence the best initial guess
  // for registration is to superimpose both mass centers. The center of
  // mass is computed from the moments obtained from the gray level values.
  // Here we adopt the first approach. The \code{GeometryOn()} method
  // toggles between the approaches.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredTransformInitializer<
            TransformType, FixedImageType,
            MovingImageType >  TransformInitializerType;
  TransformInitializerType::Pointer initializer
                                            = TransformInitializerType::New();
  initializer->SetTransform(   transform );

  initializer->SetFixedImage(  fixedImageReader->GetOutput() );
  initializer->SetMovingImage( movingImageReader->GetOutput() );
  initializer->GeometryOn();
  initializer->InitializeTransform();
  // Software Guide : EndCodeSnippet

  transform->SetAngle( 0.0 );


  registration->SetInitialTransformParameters( transform->GetParameters() );

  // Software Guide : BeginLatex
  // The optimizer scales the metrics (the gradient in this case) by the
  // scales during each iteration. Hence a large value of the center scale
  // will prevent movement along the center during optimization. Here we
  // assume that the fixed and moving images are likely to be related by
  // a translation.
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

  optimizer->SetMaximumStepLength( 0.5   );
  optimizer->SetMinimumStepLength( 0.0001 );
  optimizer->SetNumberOfIterations( 400 );
  // Software Guide : EndCodeSnippet

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

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters( finalParameters );
  finalTransform->SetFixedParameters( transform->GetFixedParameters() );

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( finalTransform );
  resample->SetInput( movingImageReader->GetOutput() );

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 100 );


  typedef itk::Image< PixelType, Dimension > OutputImageType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer      writer =  WriterType::New();

  writer->SetFileName( argv[3] );

  writer->SetInput( resample->GetOutput() );
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
//  \item \code{BrainProtonDensitySliceBorder20.png}
//  \end{itemize}
//
//  The second image is the result of intentionally shifting the first
//  image by $20mm$ in $X$ and $20mm$ in
//  $Y$. Both images have unit-spacing and are shown in Figure
//  \ref{fig:FixedMovingImageRegistration1}. The example
//  yielded the following results.
//
//  \begin{verbatim}
//  Translation X = 20
//  Translation Y = 20
//  \end{verbatim}
//  These values match the true misalignment introduced in the moving image.
//  Software Guide : EndLatex

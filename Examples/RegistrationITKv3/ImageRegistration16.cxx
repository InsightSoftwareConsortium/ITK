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
//  This example illustrates how to do registration with a 2D Translation Transform,
//  the Normalized Mutual Information metric and the Amoeba optimizer.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethod.h"

#include "itkTranslationTransform.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkAmoebaOptimizer.h"
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
  CommandIterationUpdate()
    {
    m_IterationNumber=0;
    }

public:
  typedef itk::AmoebaOptimizer         OptimizerType;
  typedef   const OptimizerType   *    OptimizerPointer;

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
    std::cout << m_IterationNumber++ << "   ";
    std::cout << optimizer->GetCachedValue() << "   ";
    std::cout << optimizer->GetCachedCurrentPosition() << std::endl;
    }

private:
  unsigned long m_IterationNumber;
};

int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << " outputImagefile ";
    std::cerr << " [initialTx] [initialTy]";
    std::cerr << "[useExplicitPDFderivatives ] " << std::endl;
    return EXIT_FAILURE;
    }

  const    unsigned int    Dimension = 2;
  typedef  unsigned char   PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::TranslationTransform< double, Dimension > TransformType;

  typedef itk::AmoebaOptimizer                           OptimizerType;
  typedef itk::LinearInterpolateImageFunction<
                                    MovingImageType,
                                    double             > InterpolatorType;
  typedef itk::ImageRegistrationMethod<
                                    FixedImageType,
                                    MovingImageType    > RegistrationType;


  typedef itk::MattesMutualInformationImageToImageMetric<
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


  //  Software Guide : BeginLatex
  //
  //  The metric requires two parameters to be selected: the number
  //  of bins used to compute the entropy and the number of spatial samples
  //  used to compute the density estimates. In typical application, 50
  //  histogram bins are sufficient and the metric is relatively insensitive
  //  to changes in the number of bins. The number of spatial samples
  //  to be used depends on the content of the image. If the images are
  //  smooth and do not contain much detail, then using approximately
  //  $1$ percent of the pixels will do. On the other hand, if the images
  //  are detailed, it may be necessary to use a much higher proportion,
  //  such as $20$ percent.
  //
  //  \index{itk::Mattes\-Mutual\-Information\-Image\-To\-Image\-Metric!SetNumberOfHistogramBins()}
  //  \index{itk::Mattes\-Mutual\-Information\-Image\-To\-Image\-Metric!SetNumberOfSpatialSamples()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  metric->SetNumberOfHistogramBins( 20 );
  metric->SetNumberOfSpatialSamples( 10000 );
  // Software Guide : EndCodeSnippet

  // For consistent results when regression testing.
  metric->ReinitializeSeed( 121213 );

  if( argc > 6 )
    {
    // Define whether to calculate the metric derivative by explicitly
    // computing the derivatives of the joint PDF with respect to the Transform
    // parameters, or doing it by progressively accumulating contributions from
    // each bin in the joint PDF.
    metric->SetUseExplicitPDFDerivatives( atoi( argv[6] ) );
    }


  const unsigned int numberOfParameters = transform->GetNumberOfParameters();


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


  transform->SetIdentity();

  typedef RegistrationType::ParametersType ParametersType;

  ParametersType initialParameters =  transform->GetParameters();

  initialParameters[0] = 0.0;
  initialParameters[1] = 0.0;

  if( argc > 5 )
    {
    initialParameters[0] = atof( argv[4] );
    initialParameters[1] = atof( argv[5] );
    }

  registration->SetInitialTransformParameters( initialParameters  );

  std::cout << "Initial transform parameters = ";
  std::cout << initialParameters << std::endl;


  //  Software Guide : BeginLatex
  //
  //  The AmoebaOptimizer moves a simplex around the cost surface.  Here we set
  //  the initial size of the simplex (5 units in each of the parameters)
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  OptimizerType::ParametersType simplexDelta( numberOfParameters );
  simplexDelta.Fill( 5.0 );

  optimizer->AutomaticInitialSimplexOff();
  optimizer->SetInitialSimplexDelta( simplexDelta );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We also adjust the tolerances on the optimizer to define convergence.
  //  Here, we used a tolerance on the parameters of 0.1 (which will be one
  //  tenth of image unit, in this case pixels). We also set the tolerance on
  //  the cost function value to define convergence.  The metric we are using
  //  returns the value of Mutual Information.  So we set the function
  //  convergence to be 0.001 bits (bits are the appropriate units for
  //  measuring Information).
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetParametersConvergenceTolerance( 0.1 );  // 1/10th pixel
  optimizer->SetFunctionConvergenceTolerance(0.001);    // 0.001 bits
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  In the case where the optimizer never succeeds in reaching the desired
  //  precision tolerance, it is prudent to establish a limit on the number of
  //  iterations to be performed. This maximum number is defined with the
  //  method \code{SetMaximumNumberOfIterations()}.
  //
  //  \index{itk::Amoeba\-Optimizer!SetMaximumNumberOfIterations()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumNumberOfIterations( 200 );
  // Software Guide : EndCodeSnippet


  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );


  try
    {
    registration->Update();
    std::cout << "Optimizer stop condition: "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }


  ParametersType finalParameters = registration->GetLastTransformParameters();

  const double finalTranslationX    = finalParameters[0];
  const double finalTranslationY    = finalParameters[1];

  double bestValue = optimizer->GetValue();


  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << finalTranslationX  << std::endl;
  std::cout << " Translation Y = " << finalTranslationY  << std::endl;
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

  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}

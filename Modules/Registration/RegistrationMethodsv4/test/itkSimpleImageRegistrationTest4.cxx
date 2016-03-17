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


#include "itkEuler2DTransform.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkTestingMacros.h"
#include "itkImageRegistrationMethodv4.h"
#include "itkConjugateGradientLineSearchOptimizerv4.h"

#include <iomanip>

namespace
{
template<typename TOptimizer>
class CommandIterationUpdate : public itk::Command
{
public:
  typedef CommandIterationUpdate                                          Self;
  typedef itk::Command                                                    Superclass;
  typedef itk::SmartPointer<Self>                                         Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate()
    {
      // mark used to avoid warnings
      (void) &Self::Clone;
    };

public:

  virtual void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *) caller, event);
    }

  virtual void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {

    const TOptimizer *optimizer = dynamic_cast< const TOptimizer * > (object);

    if( typeid( event ) != typeid( itk::IterationEvent ) || !optimizer )
      { return; }

    // stash the stream state
    std::ios  state(NULL);
    state.copyfmt(std::cout);
    std::cout << std::fixed << std::setfill(' ') << std::setprecision( 5 );
    std::cout << std::setw(3) << optimizer->GetCurrentIteration();
    std::cout << " = " << std::setw(10) << optimizer->GetCurrentMetricValue();
    std::cout << " : " << optimizer->GetCurrentPosition() << std::endl;
    std::cout << "\nScales: " << optimizer->GetScales() << std::endl;

    }
};

template<unsigned int TDimension>
int ImageRegistration( int itkNotUsed( argc ), char *argv[] )
{
  const unsigned int ImageDimension = TDimension;


  typedef float                                 PixelType;
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

  // Set up the centered transform initializer
  typedef itk::Euler2DTransform<double> TransformType;
  typename TransformType::Pointer initialTransform = TransformType::New();


  typedef itk::MeanSquaresImageToImageMetricv4<FixedImageType, MovingImageType> MetricType;
  typename MetricType::Pointer metric = MetricType::New();

  typedef itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, TransformType> RegistrationType;
  typename RegistrationType::Pointer registration = RegistrationType::New();

  registration->SetFixedImage( fixedImage );
  registration->SetMovingImage( movingImage );
  registration->SetMetric( metric );
  registration->SetMovingInitialTransform( initialTransform );
  registration->SetNumberOfLevels(1);

  typedef itk::ConjugateGradientLineSearchOptimizerv4 Optimizerv4Type;
  typename Optimizerv4Type::Pointer optimizer = Optimizerv4Type::New();

  optimizer->SetLearningRate( 1.0 );
  optimizer->SetNumberOfIterations( 100 );
  optimizer->SetMinimumConvergenceValue(1e-5);
  optimizer->SetConvergenceWindowSize(2);

  double scaleData[] = {200000,1.0,1.0};
  typename Optimizerv4Type::ScalesType::Superclass scales( scaleData, 3);
  optimizer->SetScales( scales );

  registration->SetOptimizer(optimizer);

  typedef CommandIterationUpdate<Optimizerv4Type> CommandType;
  typename CommandType::Pointer observer = CommandType::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );

  try
    {
    registration->Update();
    }
  catch( itk::ExceptionObject &e )
    {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  registration->GetTransform()->Print(std::cout);
  std::cout << optimizer->GetStopConditionDescription() << std::endl;
  typename TransformType::ParametersType results = registration->GetTransform()->GetParameters();

  std::cout << "Expecting close (+/- 0.3) to: ( 0.0, -2.8, 9.5 )" << std::endl;
  std::cout << "Parameters: " << results << std::endl;

  std::cout << "Number Of Iterations: " << optimizer->GetCurrentIteration();
  TEST_EXPECT_TRUE( optimizer->GetCurrentIteration() > 5 );

  return EXIT_SUCCESS;
}
}

int itkSimpleImageRegistrationTest4( int argc, char *argv[] )
{
  if ( argc < 4 )
    {
    std::cout << argv[0] << " imageDimension fixedImage movingImage" << std::endl;
    exit( 1 );
    }

  switch( atoi( argv[1] ) )
    {
    case 2:
      return ImageRegistration<2>( argc, argv );

    default:
      std::cerr << "Unsupported dimension" << std::endl;
      exit( EXIT_FAILURE );
    }
  return EXIT_SUCCESS;
}

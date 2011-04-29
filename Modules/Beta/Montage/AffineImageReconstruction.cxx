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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkCenteredTransformInitializer.h"
#include "itkAffineTransform.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkNumericSeriesFileNames.h"
#include "itkTimeProbesCollectorBase.h"

//
//  The following piece of code implements an observer
//  that will monitor the evolution of the registration process.
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
  typedef itk::RegularStepGradientDescentOptimizer OptimizerType;
  typedef   const OptimizerType *                  OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
    OptimizerPointer optimizer =
                      dynamic_cast< OptimizerPointer >( object );
    if( ! itk::IterationEvent().CheckEvent( &event ) )
      {
      return;
      }
      std::cout << optimizer->GetCurrentIteration() << "   ";
      std::cout << optimizer->GetValue() << "   ";
      std::cout << optimizer->GetCurrentPosition();

      // Print the angle for the trace plot
      vnl_matrix<double> p(2, 2);
      p[0][0] = (double) optimizer->GetCurrentPosition()[0];
      p[0][1] = (double) optimizer->GetCurrentPosition()[1];
      p[1][0] = (double) optimizer->GetCurrentPosition()[2];
      p[1][1] = (double) optimizer->GetCurrentPosition()[3];
      vnl_svd<double> svd(p);
      vnl_matrix<double> r(2, 2);
      r = svd.U() * vnl_transpose(svd.V());
      double angle = vcl_asin(r[1][0]);
      std::cout << " AffineAngle: " << angle * 180.0 / vnl_math::pi << std::endl;
    }
};

class AffineRegistration
{
public:

  typedef itk::AffineTransform< double, 2 >  TransformType;

  AffineRegistration()
    {
    this->m_OutputInterSliceTransform = TransformType::New();
    }

  ~AffineRegistration() {}

  void SetFixedImageFileName( const std::string & name )
    {
    this->m_FixedImageFilename = name;
    }

  void SetMovingImageFileName( const std::string & name )
    {
    this->m_MovingImageFilename = name;
    }

  void SetRegisteredImageFileName( const std::string & name )
    {
    this->m_RegisteredImageFileName = name;
    }

  const TransformType * GetOutputInterSliceTransform() const
   {
   return this->m_OutputInterSliceTransform.GetPointer();
   }

  void Execute();

private:

  std::string  m_FixedImageFilename;
  std::string  m_MovingImageFilename;
  std::string  m_RegisteredImageFileName;

  TransformType::Pointer  m_OutputInterSliceTransform;

};


void AffineRegistration::Execute()
{

  std::cout << "AffineRegistration of " << std::endl;
  std::cout << this->m_FixedImageFilename  << std::endl;
  std::cout << this->m_MovingImageFilename << std::endl;
  std::cout << std::endl;

  itk::TimeProbesCollectorBase  chronometer;

  const    unsigned int    Dimension = 2;
  typedef  unsigned char   PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;

  typedef itk::MeanSquaresImageToImageMetric<
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

  registration->SetTransform( this->m_OutputInterSliceTransform );

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  this->m_FixedImageFilename );
  movingImageReader->SetFileName( this->m_MovingImageFilename );

  chronometer.Start("Reading");
  fixedImageReader->Update();
  movingImageReader->Update();
  chronometer.Stop("Reading");

  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  registration->SetFixedImageRegion(
     fixedImageReader->GetOutput()->GetBufferedRegion() );

  typedef itk::CenteredTransformInitializer<
                                    TransformType,
                                    FixedImageType,
                                    MovingImageType >  TransformInitializerType;

  TransformInitializerType::Pointer initializer = TransformInitializerType::New();

  initializer->SetTransform( this->m_OutputInterSliceTransform );
  initializer->SetFixedImage(  fixedImageReader->GetOutput() );
  initializer->SetMovingImage( movingImageReader->GetOutput() );
  initializer->GeometryOn();

  chronometer.Start("Initialization");
  initializer->InitializeTransform();
  chronometer.Stop("Initialization");

  registration->SetInitialTransformParameters( this->m_OutputInterSliceTransform->GetParameters() );

  double translationScale = 1.0 / 1000.0;

  typedef OptimizerType::ScalesType       OptimizerScalesType;
  OptimizerScalesType optimizerScales( this->m_OutputInterSliceTransform->GetNumberOfParameters() );

  optimizerScales[0] =  1.0;
  optimizerScales[1] =  1.0;
  optimizerScales[2] =  1.0;
  optimizerScales[3] =  1.0;
  optimizerScales[4] =  translationScale;
  optimizerScales[5] =  translationScale;

  optimizer->SetScales( optimizerScales );

  double steplength = 0.1;

  unsigned int maxNumberOfIterations = 300;

  optimizer->SetMaximumStepLength( steplength );
  optimizer->SetMinimumStepLength( 0.0001 );
  optimizer->SetNumberOfIterations( maxNumberOfIterations );

  optimizer->MinimizeOn();

  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );

  try
    {
    chronometer.Start("Registration");
    registration->StartRegistration();
    chronometer.Stop("Registration");
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return;
    }

  std::cout << "Optimizer stop condition: "
            << registration->GetOptimizer()->GetStopConditionDescription()
            << std::endl;

  OptimizerType::ParametersType finalParameters =
                    registration->GetLastTransformParameters();

  const double finalRotationCenterX = this->m_OutputInterSliceTransform->GetCenter()[0];
  const double finalRotationCenterY = this->m_OutputInterSliceTransform->GetCenter()[1];
  const double finalTranslationX    = finalParameters[4];
  const double finalTranslationY    = finalParameters[5];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  const double bestValue = optimizer->GetValue();

  std::cout << "Result = " << std::endl;
  std::cout << " Center X      = " << finalRotationCenterX  << std::endl;
  std::cout << " Center Y      = " << finalRotationCenterY  << std::endl;
  std::cout << " Translation X = " << finalTranslationX  << std::endl;
  std::cout << " Translation Y = " << finalTranslationY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;

  vnl_matrix<double> p(2, 2);
  p[0][0] = (double) finalParameters[0];
  p[0][1] = (double) finalParameters[1];
  p[1][0] = (double) finalParameters[2];
  p[1][1] = (double) finalParameters[3];
  vnl_svd<double> svd(p);
  vnl_matrix<double> r(2, 2);
  r = svd.U() * vnl_transpose(svd.V());
  double angle = vcl_asin(r[1][0]);

  const double angleInDegrees = angle * 180.0 / vnl_math::pi;

  std::cout << " Scale 1         = " << svd.W(0)        << std::endl;
  std::cout << " Scale 2         = " << svd.W(1)        << std::endl;
  std::cout << " Angle (degrees) = " << angleInDegrees  << std::endl;

  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters( finalParameters );
  finalTransform->SetFixedParameters( this->m_OutputInterSliceTransform->GetFixedParameters() );

  ResampleFilterType::Pointer resampler = ResampleFilterType::New();

  resampler->SetTransform( finalTransform );
  resampler->SetInput( movingImageReader->GetOutput() );

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resampler->SetSize( fixedImage->GetLargestPossibleRegion().GetSize() );
  resampler->SetOutputOrigin(  fixedImage->GetOrigin() );
  resampler->SetOutputSpacing( fixedImage->GetSpacing() );
  resampler->SetOutputDirection( fixedImage->GetDirection() );

  resampler->SetDefaultPixelValue( 100 );

  chronometer.Start("Resampling");
  resampler->Update();
  chronometer.Stop("Resampling");

  typedef  unsigned char  OutputPixelType;

  typedef itk::ImageFileWriter< FixedImageType >  WriterType;

  WriterType::Pointer      writer =  WriterType::New();

  writer->SetFileName( m_RegisteredImageFileName );

  writer->SetInput( resampler->GetOutput() );

  chronometer.Start("Writing");
  writer->Update();
  chronometer.Stop("Writing");

  chronometer.Report( std::cout );
}


int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << "   fixedImageFile  movingImageFile " << std::endl;
    std::cerr << "   outputImagefile  [differenceBeforeRegistration] " << std::endl;
    std::cerr << "   [differenceAfterRegistration] " << std::endl;
    std::cerr << "   [stepLength] [maxNumberOfIterations] "<< std::endl;
    return EXIT_FAILURE;
    }

  typedef AffineRegistration::TransformType   TransformType;

  typedef itk::NumericSeriesFileNames    NameGeneratorType;

  NameGeneratorType::Pointer nameGenerator = NameGeneratorType::New();

  nameGenerator->SetSeriesFormat( argv[1] );
  nameGenerator->SetStartIndex( atoi( argv[2] ) );
  nameGenerator->SetEndIndex( atoi( argv[3] ) );
  nameGenerator->SetIncrementIndex( 1 );

  typedef std::vector< std::string > FileNamesType;

  const FileNamesType & nameList = nameGenerator->GetFileNames();

  FileNamesType::const_iterator nameFixed  = nameList.begin();
  FileNamesType::const_iterator nameMoving = nameList.begin();
  FileNamesType::const_iterator nameEnd    = nameList.end();

  nameMoving++;

  while( nameMoving != nameEnd )
    {
    AffineRegistration registration;

    registration.SetFixedImageFileName( *nameFixed );
    registration.SetMovingImageFileName( *nameMoving );
    registration.SetRegisteredImageFileName("registered.png");

    registration.Execute();

    const TransformType * intersliceTransform =
      registration.GetOutputInterSliceTransform();

    intersliceTransform->Print( std::cout );

    nameFixed++;
    nameMoving++;
    }

}


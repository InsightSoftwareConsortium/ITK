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

#include "itkMutualInformationImageToImageMetric.h"

#include "itkTextOutput.h"
#include "itkSimpleMultiResolutionImageRegistrationUI.h"
namespace
{


double F( itk::Vector<double,3> & v );

}

/**
 *  This program test the
 *  itk::MultiResolutionImageRegistrationMethod class
 *
 *  This file tests the combination of:
 *   - MutualInformation
 *   - AffineTransform
 *   - GradientDescentOptimizer
 *   - LinearInterpolateImageFunction
 *   - RecursiveMultiResolutionPyramidImageFilter
 *
 *  The test image pattern consists of a 3D gaussian in the middle
 *  with some directional pattern on the outside.
 *  One image is scaled and shifted relative to the other.
 *
 *  This program runs two registration tests. The first test
 *  uses SetNumberOfLevels() method to specify the number of computation levels
 *  in the pyramid. The second test uses a user defined multi-resolution schedule.
 *  The final transform of the registration runs are compared with the
 *  true parameters.
 *
 *
 * Notes:
 * =====
 * This example performs an affine
 * registration between a moving image and a fixed image using
 * mutual information and a multi-resolution strategy.
 *
 * See notes for itkImageRegistrationMethodTest_13.cxx for more
 * detailed information on the algorithm.
 *
 * A simple user-interface, allows the user to define the number
 * of iteration and learning rate at each resolution level.
 *
 * In addition, several exceptions are exercised for testing and code
 * coverage purpose.
 *
 *
 */

int itkMultiResolutionImageRegistrationMethodTest_1(int, char* [] )
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  bool pass = true;

  const unsigned int dimension = 3;
  unsigned int j;

  typedef float  PixelType;

  // Fixed Image Type
  typedef itk::Image<PixelType,dimension>               FixedImageType;

  // Moving Image Type
  typedef itk::Image<PixelType,dimension>               MovingImageType;

  // Transform Type
  typedef itk::AffineTransform< double,dimension >  TransformType;

  // Optimizer Type
  typedef itk::GradientDescentOptimizer             OptimizerType;

  // Metric Type
  typedef itk::MutualInformationImageToImageMetric<
                                    FixedImageType,
                                    MovingImageType >    MetricType;

  // Interpolation technique
  typedef itk:: LinearInterpolateImageFunction<
                                    MovingImageType,
                                    double          >    InterpolatorType;

  // Fixed Image Pyramid Type
  typedef itk::RecursiveMultiResolutionPyramidImageFilter<
                                    FixedImageType,
                                    FixedImageType  >    FixedImagePyramidType;

  // Moving Image Pyramid Type
  typedef itk::RecursiveMultiResolutionPyramidImageFilter<
                                    MovingImageType,
                                    MovingImageType  >   MovingImagePyramidType;


  // Registration Method
  typedef itk::MultiResolutionImageRegistrationMethod<
                                    FixedImageType,
                                    MovingImageType >    RegistrationType;
  /*********************************************************
   * Set up the two input images.
   * One image scaled and shifted with respect to the other.
   **********************************************************/
  FixedImageType::Pointer     fixedImage    = FixedImageType::New();
  MovingImageType::Pointer    movingImage   = MovingImageType::New();

  double displacement[dimension] = {7,3,2};
  double scale[dimension] = { 0.80, 1.0, 1.0 };

  FixedImageType::SizeType size = {{100,100,40}};
  FixedImageType::IndexType index = {{0,0,0}};
  FixedImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  fixedImage->SetLargestPossibleRegion( region );
  fixedImage->SetBufferedRegion( region );
  fixedImage->SetRequestedRegion( region );
  fixedImage->Allocate();

  movingImage->SetLargestPossibleRegion( region );
  movingImage->SetBufferedRegion( region );
  movingImage->SetRequestedRegion( region );
  movingImage->Allocate();


  typedef itk::ImageRegionIterator<MovingImageType> MovingImageIterator;
  typedef itk::ImageRegionIterator<FixedImageType>  FixedImageIterator;

  itk::Point<double,dimension> center;
  for ( j = 0; j < dimension; j++ )
    {
    center[j] = 0.5 *  (double)region.GetSize()[j];
    }

  itk::Point<double,dimension> p;
  itk::Vector<double,dimension> d;

  MovingImageIterator mIter( movingImage, region );
  FixedImageIterator  fIter( fixedImage, region );

  while( !mIter.IsAtEnd() )
    {
    for ( j = 0; j < dimension; j++ )
      {
      p[j] = mIter.GetIndex()[j];
      }

    d = p - center;

    fIter.Set( (PixelType) F(d) );

    for ( j = 0; j < dimension; j++ )
      {
      d[j] = d[j] * scale[j] + displacement[j];
      }

    mIter.Set( (PixelType) F(d) );

    ++fIter;
    ++mIter;

    }

  // set the image origin to be center of the image
  double transCenter[dimension];
  for ( j = 0; j < dimension; j++ )
    {
    transCenter[j] = -0.5 * double(size[j]);
    }

  movingImage->SetOrigin( transCenter );
  fixedImage->SetOrigin( transCenter );

  RegistrationType::ScheduleType  fixedImageSchedule;
  RegistrationType::ScheduleType  movingImageSchedule;

  /* The first registration run invokes SetNumberOfLevels to specify
   * the number of computation levels */
 {

  MetricType::Pointer         metric        = MetricType::New();
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
 InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  FixedImagePyramidType::Pointer fixedImagePyramid =
    FixedImagePyramidType::New();
  MovingImagePyramidType::Pointer movingImagePyramid =
    MovingImagePyramidType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  /******************************************************************
   * Set up the optimizer.
   ******************************************************************/

  // set the translation scale
  typedef OptimizerType::ScalesType ScalesType;
  ScalesType parametersScales( transform->GetNumberOfParameters() );

  parametersScales.Fill( 1.0 );

  for ( j = 9; j < 12; j++ )
    {
    parametersScales[j] = 0.0001;
    }

  optimizer->SetScales( parametersScales );

  // need to maximize for mutual information
  optimizer->MaximizeOn();

  /******************************************************************
   * Set up the metric.
   ******************************************************************/
  metric->SetMovingImageStandardDeviation( 5.0 );
  metric->SetFixedImageStandardDeviation( 5.0 );
  metric->SetNumberOfSpatialSamples( 50 );

  /******************************************************************
   * Set up the registrator.
   ******************************************************************/

  // connect up the components
  registration->SetMetric( metric );
  registration->SetOptimizer( optimizer );
  registration->SetTransform( transform );
  registration->SetFixedImage( fixedImage );
  registration->SetMovingImage( movingImage );
  registration->SetInterpolator( interpolator );
  registration->SetFixedImagePyramid( fixedImagePyramid );
  registration->SetMovingImagePyramid( movingImagePyramid );
  registration->SetFixedImageRegion( fixedImage->GetBufferedRegion() );

  // set initial parameters to identity
  RegistrationType::ParametersType initialParameters(
    transform->GetNumberOfParameters() );

  initialParameters.Fill( 0.0 );
  initialParameters[0] = 1.0;
  initialParameters[4] = 1.0;
  initialParameters[8] = 1.0;

  /******************************************************************
   * Attach registration to a simple UI and run registration
   ******************************************************************/
  SimpleMultiResolutionImageRegistrationUI2<RegistrationType>
    simpleUI( registration );

  unsigned short numberOfLevels = 3;

  itk::Array<unsigned int> niter( numberOfLevels );
  itk::Array<double>       rates( numberOfLevels );

  niter[0] = 100;
  niter[1] = 300;
  niter[2] = 550;

  rates[0] = 1e-3;
  rates[1] = 5e-4;
  rates[2] = 1e-4;

  simpleUI.SetNumberOfIterations( niter );
  simpleUI.SetLearningRates( rates );

  try
    {
    metric->ReinitializeSeed( 121212 );
    registration->SetNumberOfLevels( numberOfLevels );
    registration->SetInitialTransformParameters( initialParameters );

    registration->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Registration failed" << std::endl;
    std::cout << "Reason " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  /***********************************************************
   * Check the results
   ************************************************************/
  RegistrationType::ParametersType solution =
    registration->GetLastTransformParameters();

  std::cout << "Solution is: " << solution << std::endl;


  RegistrationType::ParametersType trueParameters(
    transform->GetNumberOfParameters() );
  trueParameters.Fill( 0.0 );
  trueParameters[ 0] = 1/scale[0];
  trueParameters[ 4] = 1/scale[1];
  trueParameters[ 8] = 1/scale[2];
  trueParameters[ 9] = - displacement[0]/scale[0];
  trueParameters[10] = - displacement[1]/scale[1];
  trueParameters[11] = - displacement[2]/scale[2];

  std::cout << "True solution is: " << trueParameters << std::endl;

  for( j = 0; j < 9; j++ )
    {
    if( itk::Math::abs( solution[j] - trueParameters[j] ) > 0.025 )
      {
      pass = false;
      }
    }
  for( j = 9; j < 12; j++ )
    {
    if( itk::Math::abs( solution[j] - trueParameters[j] ) > 1.0 )
      {
      pass = false;
      }
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  //store the schedules for fixed and moving images. These schedules will be
  //used by the second registration run.
  fixedImageSchedule = registration->GetFixedImagePyramid()->GetSchedule();
  movingImageSchedule = registration->GetMovingImagePyramid()->GetSchedule();


  /*************************************************
   * Check for parzen window exception
   **************************************************/
  double oldValue = metric->GetMovingImageStandardDeviation();
  metric->SetMovingImageStandardDeviation( 0.005 );

  try
    {
    pass = false;
    registration->Update();
    }
  catch(itk::ExceptionObject& err)
    {
    std::cout << "Caught expected ExceptionObject" << std::endl;
    std::cout << err << std::endl;
    pass = true;
    }

  if( !pass )
    {
    std::cout << "Should have caught an exception" << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  metric->SetMovingImageStandardDeviation( oldValue );


  /*************************************************
   * Check for mapped out of image error
   **************************************************/
  solution[5] = 1000;
  registration->SetInitialTransformParameters( solution );

  try
    {
    pass = false;
    registration->Update();
    }
  catch(itk::ExceptionObject& err)
    {
    std::cout << "Caught expected ExceptionObject" << std::endl;
    std::cout << err << std::endl;
    pass = true;
    }

  if( !pass )
    {
    std::cout << "Should have caught an exception" << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  /* To avoid confusion, SetNumberOfLevels and SetSchedules are not allowed to be
   * used together. An exception is thrown if SetSchedules() is invoked after
   * invoking SetNumberOfLevels */
 try
    {
    registration->SetNumberOfLevels( numberOfLevels );
    registration->SetSchedules( fixedImageSchedule, movingImageSchedule  );
    pass=false;
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Expected exception is thrown since we tried to set schedules after"
                 << " setting the number of levels" << std::endl;
    std::cout << "Reason " << e.GetDescription() << std::endl;
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }
  }

  /* The second registration uses user defined schedules. For testing purpose, we
   * will use the schedules internally generated in the first registration run
   * by the fixed and moving image after the number of levels is set. The final
   * registration transform parameter values should remain the same*/

  {

  MetricType::Pointer         metric        = MetricType::New();
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  FixedImagePyramidType::Pointer fixedImagePyramid =
    FixedImagePyramidType::New();
  MovingImagePyramidType::Pointer movingImagePyramid =
    MovingImagePyramidType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  /******************************************************************
   * Set up the optimizer.
   ******************************************************************/

  // set the translation scale
  typedef OptimizerType::ScalesType ScalesType;
  ScalesType parametersScales( transform->GetNumberOfParameters() );

  parametersScales.Fill( 1.0 );

  for ( j = 9; j < 12; j++ )
    {
    parametersScales[j] = 0.0001;
    }

  optimizer->SetScales( parametersScales );

  // need to maximize for mutual information
  optimizer->MaximizeOn();

  /******************************************************************
   * Set up the metric.
   ******************************************************************/
  metric->SetMovingImageStandardDeviation( 5.0 );
  metric->SetFixedImageStandardDeviation( 5.0 );
  metric->SetNumberOfSpatialSamples( 50 );

  /******************************************************************
   * Set up the registrator.
   ******************************************************************/

  // connect up the components
  registration->SetMetric( metric );
  registration->SetOptimizer( optimizer );
  registration->SetTransform( transform );
  registration->SetFixedImage( fixedImage );
  registration->SetMovingImage( movingImage );
  registration->SetInterpolator( interpolator );
  registration->SetFixedImagePyramid( fixedImagePyramid );
  registration->SetMovingImagePyramid( movingImagePyramid );
  registration->SetFixedImageRegion( fixedImage->GetBufferedRegion() );

  // set initial parameters to identity
  RegistrationType::ParametersType initialParameters(
    transform->GetNumberOfParameters() );

  initialParameters.Fill( 0.0 );
  initialParameters[0] = 1.0;
  initialParameters[4] = 1.0;
  initialParameters[8] = 1.0;

  /******************************************************************
   * Attach registration to a simple UI and run registration
   ******************************************************************/
  SimpleMultiResolutionImageRegistrationUI2<RegistrationType>
    simpleUI( registration );

  unsigned short numberOfLevels = 3;

  itk::Array<unsigned int> niter( numberOfLevels );
  itk::Array<double>       rates( numberOfLevels );

  niter[0] = 100;
  niter[1] = 300;
  niter[2] = 550;

  rates[0] = 1e-3;
  rates[1] = 5e-4;
  rates[2] = 1e-4;

  simpleUI.SetNumberOfIterations( niter );
  simpleUI.SetLearningRates( rates );

  try
    {
    metric->ReinitializeSeed( 121212 );
    registration->SetSchedules( fixedImageSchedule, movingImageSchedule);
    registration->SetInitialTransformParameters( initialParameters );

    registration->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Registration failed" << std::endl;
    std::cout << "Reason " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }


  /***********************************************************
   * Check the results
   ************************************************************/
  RegistrationType::ParametersType solution =
    registration->GetLastTransformParameters();

  std::cout << "Solution is: " << solution << std::endl;


  RegistrationType::ParametersType trueParameters(
    transform->GetNumberOfParameters() );
  trueParameters.Fill( 0.0 );
  trueParameters[ 0] = 1/scale[0];
  trueParameters[ 4] = 1/scale[1];
  trueParameters[ 8] = 1/scale[2];
  trueParameters[ 9] = - displacement[0]/scale[0];
  trueParameters[10] = - displacement[1]/scale[1];
  trueParameters[11] = - displacement[2]/scale[2];

  std::cout << "True solution is: " << trueParameters << std::endl;

  for( j = 0; j < 9; j++ )
    {
    if( itk::Math::abs( solution[j] - trueParameters[j] ) > 0.025 )
      {
      pass = false;
      }
    }
  for( j = 9; j < 12; j++ )
    {
    if( itk::Math::abs( solution[j] - trueParameters[j] ) > 1.0 )
      {
      pass = false;
      }
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}

namespace
{
/**
 * This function defines the test image pattern.
 * The pattern is a 3D gaussian in the middle
 * and some directional pattern on the outside.
 */
double F( itk::Vector<double,3> & v )
{
  double x = v[0];
  double y = v[1];
  double z = v[2];
  const double s = 50;
  double value = 200.0 * std::exp( - ( x*x + y*y + z*z )/(s*s) );
  x -= 8; y += 3; z += 0;
  double r = std::sqrt( x*x + y*y + z*z );
  if( r > 35 )
    {
    value = 2 * ( itk::Math::abs( x ) +
      0.8 * itk::Math::abs( y ) +
      0.5 * itk::Math::abs( z ) );
    }
  if( r < 4 )
    {
    value = 400;
    }

  return value;

}
}

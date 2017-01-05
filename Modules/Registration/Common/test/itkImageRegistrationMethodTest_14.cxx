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

#include "itkImageRegistrationMethod.h"
#include "itkQuaternionRigidTransform.h"
#include "itkMutualInformationImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkQuaternionRigidTransformGradientDescentOptimizer.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

#include "itkTextOutput.h"
#include "itkCommandIterationUpdate.h"

namespace
{
double F( itk::Vector<double,3> & v );
}

/**
 *  This program test one instantiation of the itk::ImageRegistrationMethod class
 *
 *  This file tests the combination of:
 *   - MutualInformation
 *   - QuaternionRigidTransform
 *   - QuaternionRigidTransformGradientDescentOptimizer
 *   - LinearInterpolateImageFunction
 *
 *  The test image pattern consists of a 3D gaussian in the middle
 *  with some directional pattern on the outside.
 *  One image is rotated and shifted relative to the other.
 *
 * Notes
 * =====
 * This example performs an rigid registration
 * between a 3D fixed (target) image and 3D moving (source) image
 * using mutual information.
 * It uses the optimization method of Viola and Wells to find the
 * best rigid transform to register the moving image onto the fixed
 * image.
 *
 * The mutual information value and its derivatives are estimated
 * using spatial sampling. The performance
 * of the registration depends on good choices of the parameters
 * used to estimate the mutual information. Refer to the documentation
 * for MutualInformationImageToImageMetric for details on these
 * parameters and how to set them.
 *
 * The registration uses a simple stochastic gradient ascent scheme. Steps
 * are repeatedly taken that are proportional to the approximate
 * deriviative of the mutual information with respect to the rotation
 * transform parameters. The stepsize is governed by the LearningRate
 * parameter.
 *
 * In this example, the rigid transformation is represent by a vector
 * of 7 doubles. The first 4 parameters defines the
 * quaternion and the last 3 parameters the translation in each dimension.
 * Since the parameters of the rotation part is different in magnitude
 * to the parameters in the offset part, scaling is required
 * to improve convergence. The scaling can set via the optimizer.
 *
 * NB: In the Viola and Wells paper, the scaling is specified by
 * using different learning rates for the linear and offset part.
 * The following formula translate their scaling parameters to
 * those used in this framework:
 *
 * LearningRate = lambda_R
 * TranslationScale = sqrt( lambda_T / lambda_R );
 *
 * In the optimizer's scale transform set the scaling for
 * all the translation parameters to TranslationScale^{-2}.
 * Set the scale for all other parameters to 1.0.
 *
 * Note: the optimization performance can be improved by
 * setting the image origin to center of mass of the image.
 *
 * Implementaton of this class is based on:
 * Viola, P. and Wells III, W. (1997).
 * "Alignment by Maximization of Mutual Information"
 * International Journal of Computer Vision, 24(2):137-154
 *
 * Caveat: this exampe only work for 3D images
 *
 */

int itkImageRegistrationMethodTest_14(int, char* [] )
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
  typedef itk::QuaternionRigidTransform< double >       TransformType;

  // Optimizer Type
  typedef itk::QuaternionRigidTransformGradientDescentOptimizer
                                                         OptimizerType;

  // Metric Type
  typedef itk::MutualInformationImageToImageMetric<
                                    FixedImageType,
                                    MovingImageType >    MetricType;

  // Interpolation technique
  typedef itk:: LinearInterpolateImageFunction<
                                    MovingImageType,
                                    double          >    InterpolatorType;

  // Registration Method
  typedef itk::ImageRegistrationMethod<
                                    FixedImageType,
                                    MovingImageType >    RegistrationType;


  MetricType::Pointer         metric        = MetricType::New();
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  FixedImageType::Pointer     fixedImage    = FixedImageType::New();
  MovingImageType::Pointer    movingImage   = MovingImageType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  /*********************************************************
   * Set up the two input images.
   * One image rotated (xy plane) and shifted with respect to the other.
   **********************************************************/
  double displacement[dimension] = {7,3,2};
  double angle = 10.0 / 180.0 * itk::Math::pi;

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
  itk::Vector<double,dimension> d, d2;

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


    d2[0] =  d[0] * std::cos(angle) + d[1] * std::sin(angle) + displacement[0];
    d2[1] = -d[0] * std::sin(angle) + d[1] * std::cos(angle) + displacement[1];
    d2[2] = d[2] + displacement[2];

    mIter.Set( (PixelType) F(d2) );

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


  /******************************************************************
   * Set up the optimizer.
   ******************************************************************/

  // set the translation scale
  typedef OptimizerType::ScalesType ScalesType;
  ScalesType parametersScales( transform->GetNumberOfParameters() );

  parametersScales.Fill( 1.0 );

  for ( j = 4; j < 7; j++ )
    {
    parametersScales[j] = 0.0001;
    }

  optimizer->SetScales( parametersScales );

  // need to maximize for mutual information
  optimizer->MaximizeOn();

  /******************************************************************
   * Set up the optimizer observer
   ******************************************************************/
  typedef itk::CommandIterationUpdate< OptimizerType > CommandIterationType;
  CommandIterationType::Pointer iterationCommand =
    CommandIterationType::New();

  iterationCommand->SetOptimizer( optimizer );

  /******************************************************************
   * Set up the metric.
   ******************************************************************/
  metric->SetMovingImageStandardDeviation( 5.0 );
  metric->SetFixedImageStandardDeviation( 5.0 );
  metric->SetNumberOfSpatialSamples( 50 );
  metric->SetFixedImageRegion( fixedImage->GetBufferedRegion() );
  metric->ReinitializeSeed(121212);

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

  // set initial parameters to identity
  RegistrationType::ParametersType initialParameters(
    transform->GetNumberOfParameters() );

  initialParameters.Fill( 0.0 );
  initialParameters[3] = 1.0;


  /***********************************************************
   * Run the registration - reducing learning rate as we go
   ************************************************************/
  const unsigned int numberOfLoops = 3;
  unsigned int iter[numberOfLoops] = { 300, 300, 350 };
  double      rates[numberOfLoops] = { 1e-3, 5e-4, 1e-4 };

  for ( j = 0; j < numberOfLoops; j++ )
    {

    try
      {
        optimizer->SetNumberOfIterations( iter[j] );
        optimizer->SetLearningRate( rates[j] );
        registration->SetInitialTransformParameters( initialParameters );
        registration->Update();

        initialParameters = registration->GetLastTransformParameters();

      }
    catch( itk::ExceptionObject & e )
      {
      std::cout << "Registration failed" << std::endl;
      std::cout << "Reason " << e.GetDescription() << std::endl;
      return EXIT_FAILURE;
      }

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
  trueParameters[2] =   std::sin( angle / 2.0 );
  trueParameters[3] =   std::cos( angle / 2.0 );
  trueParameters[4] = -1.0 * ( displacement[0] * std::cos(angle) -
                               displacement[1] * std::sin(angle) );
  trueParameters[5] = -1.0 * ( displacement[0] * std::sin(angle) +
                               displacement[1] * std::cos(angle) );
  trueParameters[6] = -1.0 * displacement[2];

  std::cout << "True solution is: " << trueParameters << std::endl;

  for( j = 0; j < 4; j++ )
    {
    if( itk::Math::abs( solution[j] - trueParameters[j] ) > 0.025 )
      {
      pass = false;
      }
    }
  for( j = 4; j < 7; j++ )
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

  // exerise other methods
  transform->SetParameters( trueParameters );
  trueParameters = transform->GetParameters();

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

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
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkGradientDescentOptimizer.h"

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
 *   - MattesMutualInformation
 *   - AffineTransform
 *   - GradientDescentOptimizer
 *   - BSplineInterpolateImageFunction
 *
 *  The test image pattern consists of a 3D gaussian in the middle
 *  with some directional pattern on the outside.
 *  One image is scaled and shifted relative to the other.
 *
 * Notes:
 * =======
 * This example performs an affine registration
 * between a moving (source) and fixed (target) image using mutual information.
 * It uses a simple steepest descent optimizer to find the
 * best affine transform to register the moving image onto the fixed
 * image.
 *
 * The mutual information value and its derivatives are estimated
 * using spatial sampling.
 *
 * The registration uses a simple stochastic gradient ascent scheme. Steps
 * are repeatedly taken that are proportional to the approximate
 * deriviative of the mutual information with respect to the affine
 * transform parameters. The stepsize is governed by the LearningRate
 * parameter.
 *
 * Since the parameters of the linear part is different in magnitude
 * to the parameters in the offset part, scaling is required
 * to improve convergence. The scaling can set via the optimizer.
 *
 * In the optimizer's scale transform set the scaling for
 * all the translation parameters to TranslationScale^{-2}.
 * Set the scale for all other parameters to 1.0.
 *
 * Note: the optimization performance can be improved by
 * setting the image origin to center of mass of the image.
 *
 */
int itkImageRegistrationMethodTest_15(int, char* [] )
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
  typedef itk::MattesMutualInformationImageToImageMetric<
                                    FixedImageType,
                                    MovingImageType >    MetricType;

  // Interpolation technique
  typedef itk:: BSplineInterpolateImageFunction<
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
   * One image scaled and shifted with respect to the other.
   **********************************************************/
  double displacement[dimension] = {3,1,1};
  double scale[dimension] = { 0.90, 1.0, 1.0 };

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
  optimizer->MaximizeOff();

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
  metric->SetNumberOfSpatialSamples( static_cast<unsigned long>(
    0.01 * fixedImage->GetBufferedRegion().GetNumberOfPixels() ) );

  metric->SetNumberOfHistogramBins( 50 );

  for( unsigned int jj = 0; jj < dimension; jj++ )
    {
    size[jj] -= 4;
    index[jj] += 2;
    }
  region.SetSize( size );
  region.SetIndex( index );
  metric->SetFixedImageRegion( region );
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
  initialParameters[0] = 1.0;
  initialParameters[4] = 1.0;
  initialParameters[8] = 1.0;


  /***********************************************************
   * Run the registration
   ************************************************************/
  const unsigned int numberOfLoops = 2;
  unsigned int iter[numberOfLoops] = { 50, 0 };
  double      rates[numberOfLoops] = { 1e-3, 5e-4 };


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

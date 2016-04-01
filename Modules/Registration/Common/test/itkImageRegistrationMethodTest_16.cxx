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
#include "itkLinearInterpolateImageFunction.h"
#include "itkGradientDescentOptimizer.h"
#include "itkMeanSquaresImageToImageMetric.h"

#include "itkImageRegistrationMethodImageSource.h"

/**
 *  This program tests one instantiation of the itk::ImageRegistrationMethod class
 *
 *
 */


template<typename DataType>
bool DoRegistration ()
{

  bool pass = true;

  const unsigned int dimension = 2;

  // Fixed Image Type
  typedef itk::Image<DataType,dimension>               FixedImageType;

  // Moving Image Type
  typedef itk::Image<DataType,dimension>               MovingImageType;

  // Size Type
  typedef typename MovingImageType::SizeType                 SizeType;

  // Transform Type
  typedef itk::AffineTransform< double, dimension > TransformType;
  typedef typename TransformType::ParametersType    ParametersType;

  typedef typename FixedImageType::PixelType     FixedImagePixelType;
  typedef typename MovingImageType::PixelType    MovingImagePixelType;

  // ImageSource
  typedef itk::testhelper::ImageRegistrationMethodImageSource<
                                  FixedImagePixelType,
                                  MovingImagePixelType,
                                  dimension >       ImageSourceType;
  // Transform Type
  typedef itk::AffineTransform< double, dimension > TransformType;
  typedef typename TransformType::ParametersType    ParametersType;

  // Optimizer Type
  typedef itk::GradientDescentOptimizer             OptimizerType;

  // Metric Type
  typedef itk::MeanSquaresImageToImageMetric<
                                    FixedImageType,
                                    MovingImageType >    MetricType;

  // Interpolation technique
  typedef itk:: LinearInterpolateImageFunction<
                                    MovingImageType,
                                    double >             InterpolatorType;

  // Registration Method
  typedef itk::ImageRegistrationMethod<
                                    FixedImageType,
                                    MovingImageType >    RegistrationType;

  typedef itk::CommandIterationUpdate<
                                  OptimizerType >    CommandIterationType;


  typename MetricType::Pointer         metric        = MetricType::New();
  typename TransformType::Pointer      transform     = TransformType::New();
  typename OptimizerType::Pointer      optimizer     = OptimizerType::New();
  typename InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  typename RegistrationType::Pointer   registration  = RegistrationType::New();

  typename ImageSourceType::Pointer    imageSource   = ImageSourceType::New();

  SizeType size;
  size[0] = 100;
  size[1] = 100;

  imageSource->GenerateImages( size );

  typename FixedImageType::ConstPointer     fixedImage    = imageSource->GetFixedImage();
  typename MovingImageType::ConstPointer    movingImage   = imageSource->GetMovingImage();

  //
  // Connect all the components required for Registratio
  //
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetFixedImage(    fixedImage    );
  registration->SetMovingImage(   movingImage   );
  registration->SetInterpolator(  interpolator  );


  // Select the Region of Interest over which the Metric will be computed
  // Registration time will be proportional to the number of pixels in this region.
  metric->SetFixedImageRegion( fixedImage->GetBufferedRegion() );

  // Instantiate an Observer to report the progress of the Optimization
  CommandIterationType::Pointer iterationCommand = CommandIterationType::New();
  iterationCommand->SetOptimizer(  optimizer.GetPointer() );

  // Scale the translation components of the Transform in the Optimizer
  OptimizerType::ScalesType scales( transform->GetNumberOfParameters() );
  scales.Fill( 1.0 );


  unsigned long   numberOfIterations =  100;
  double          translationScale   = 1e-6;
  double          learningRate       = 1e-8;

  for( unsigned int i=0; i<dimension; i++)
    {
    scales[ i + dimension * dimension ] = translationScale;
    }

  optimizer->SetScales( scales );
  optimizer->SetLearningRate( learningRate );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->MinimizeOn();

  // Start from an Identity transform (in a normal case, the user
  // can probably provide a better guess than the identity...
  transform->SetIdentity();
  registration->SetInitialTransformParameters( transform->GetParameters() );

  // Initialize the internal connections of the registration method.
  // This can potentially throw an exception
  try
    {
    registration->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << e << std::endl;
    pass = false;
    }

  ParametersType actualParameters = imageSource->GetActualParameters();
  ParametersType finalParameters  = registration->GetLastTransformParameters();

  const unsigned int numbeOfParameters = actualParameters.Size();

  // We know that for the Affine transform the Translation parameters are at
  // the end of the list of parameters.
  const unsigned int offsetOrder = finalParameters.Size()-actualParameters.Size();
  const double tolerance = 1.0;  // equivalent to 1 pixel.

  for(unsigned int i=0; i<numbeOfParameters; i++)
    {
    // the parameters are negated in order to get the inverse transformation.
    // this only works for comparing translation parameters....
    std::cout << finalParameters[i+offsetOrder] << " == " << -actualParameters[i] << std::endl;
    if( itk::Math::abs ( finalParameters[i+offsetOrder] - (-actualParameters[i]) ) > tolerance )
      {
      std::cout << "Tolerance exceeded at component " << i << std::endl;
      pass = false;
      }
    }

  return pass;

}
int itkImageRegistrationMethodTest_16(int itkNotUsed(argc), char*[] itkNotUsed(argv) )
{
  bool result_uc, result_c, result_us, result_s,
    result_ui, result_i, result_ul, result_l,
    result_f, result_d;
  result_uc = DoRegistration<unsigned char>();
  result_c  = DoRegistration<char>();
  result_us = DoRegistration<unsigned short>();
  result_s  = DoRegistration<short>();
  result_ui = DoRegistration<unsigned int>();
  result_i = DoRegistration<int>();
  result_ul = DoRegistration<unsigned long>();
  result_l  = DoRegistration<long>();
  result_f  = DoRegistration<float>();
  result_d  = DoRegistration<double>();

  std::cout << "<unsigned char>:  " << result_uc << std::endl;
  std::cout << "<char>:           " << result_c  << std::endl;
  std::cout << "<unsigned short>: " << result_us << std::endl;
  std::cout << "<short>:          " << result_s  << std::endl;
  std::cout << "<unsigned int>:   " << result_ui << std::endl;
  std::cout << "<int>:            " << result_i  << std::endl;
  std::cout << "<unsigned long>:  " << result_ul << std::endl;
  std::cout << "<long>:           " << result_l  << std::endl;
  std::cout << "<float>:          " << result_f  << std::endl;
  std::cout << "<double>:         " << result_d  << std::endl;

  return EXIT_SUCCESS;
}

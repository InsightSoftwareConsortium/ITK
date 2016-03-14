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
#include "itkTranslationTransform.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkConjugateGradientOptimizer.h"
#include "itkCommandVnlIterationUpdate.h"

#include "itkImageRegistrationMethodImageSource.h"

/**
 *  This program tests one instantiation of the itk::ImageRegistrationMethod class
 *
 *
 */

int itkImageRegistrationMethodTest_9(int argc, char* argv[] )
{

  bool pass = true;

  const unsigned int dimension = 2;

  // Fixed Image Type
  typedef itk::Image<float,dimension>               FixedImageType;

  // Moving Image Type
  typedef itk::Image<float,dimension>               MovingImageType;

  // Size Type
  typedef MovingImageType::SizeType                 SizeType;


  // ImageSource
  typedef itk::testhelper::ImageRegistrationMethodImageSource<
                                  FixedImageType::PixelType,
                                  MovingImageType::PixelType,
                                  dimension >            ImageSourceType;
  // Transform Type
  typedef itk::TranslationTransform< double, dimension > TransformType;
  typedef TransformType::ParametersType                  ParametersType;

  // Optimizer Type
  typedef itk::ConjugateGradientOptimizer                OptimizerType;

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

  typedef itk::CommandVnlIterationUpdate<
                                  OptimizerType >    CommandIterationType;


  MetricType::Pointer         metric        = MetricType::New();
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  ImageSourceType::Pointer    imageSource   = ImageSourceType::New();

  SizeType size;
  size[0] = 100;
  size[1] = 100;

  imageSource->GenerateImages( size );

  FixedImageType::ConstPointer     fixedImage    = imageSource->GetFixedImage();
  MovingImageType::ConstPointer    movingImage   = imageSource->GetMovingImage();

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


  double          translationScale   =  1.0;

  if( argc > 1 )
    {
    translationScale = atof( argv[2] );
    std::cout << "translationScale = " << translationScale << std::endl;
    }

  for( unsigned int i=0; i<dimension; i++)
    {
    scales[ i ] = translationScale;
    }

  optimizer->SetScales( scales );


  // Start from an Identity transform (in a normal case, the user
  // can probably provide a better guess than the identity...
  transform->SetIdentity();
  registration->SetInitialTransformParameters( transform->GetParameters() );

  // Initialize the internal connections of the registration method.
  // This can potentially throw an exception
  try
    {
    registration->Initialize();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  // Set the parameters of the vnl optimizer
  // this can only be done after the registration has been initialized
  // because the vnl_optimizer is instantiated there.
  vnl_conjugate_gradient * vnlOptimizer = optimizer->GetOptimizer();

  const double F_Tolerance      = 1e-3;  // Function value tolerance
  const double G_Tolerance      = 1e-4;  // Gradient magnitude tolerance
  const double X_Tolerance      = 1e-8;  // Search space tolerance
  const double Epsilon_Function = 1e-10; // Step
  const int    Max_Iterations   =   100; // Maximum number of iterations

  vnlOptimizer->set_f_tolerance( F_Tolerance );
  vnlOptimizer->set_g_tolerance( G_Tolerance );
  vnlOptimizer->set_x_tolerance( X_Tolerance );
  vnlOptimizer->set_epsilon_function( Epsilon_Function );
  vnlOptimizer->set_max_function_evals( Max_Iterations );

  vnlOptimizer->set_check_derivatives( 3 );


  // Start the actual registration process
  try
    {
    registration->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  ParametersType actualParameters = imageSource->GetActualParameters();
  ParametersType finalParameters  = registration->GetLastTransformParameters();

  const unsigned int numbeOfParameters = actualParameters.Size();


  const double tolerance = 1.0;  // equivalent to 1 pixel.

  for(unsigned int i=0; i<numbeOfParameters; i++)
    {
    // the parameters are negated in order to get the inverse transformation.
    // this only works for comparing translation parameters....
    std::cout << finalParameters[i] << " == " << -actualParameters[i] << std::endl;
    if( itk::Math::abs ( finalParameters[i] - (-actualParameters[i]) ) > tolerance )
      {
      std::cout << "Tolerance exceeded at component " << i << std::endl;
      pass = false;
      }
    }


  if( !pass )
    {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;


}

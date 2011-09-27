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
#include "itkMeanReciprocalSquareDifferenceImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkGradientDescentOptimizer.h"

/**
 *  This program test one instantiation of the itk::ImageRegistrationMethod class
 *
 *  Only typedef are tested in this file.
 */

int itkImageRegistrationMethodTest_11(int, char* [] )
{

  bool pass = true;

  const unsigned int dimension = 3;

  // Fixed Image Type
  typedef itk::Image<float,dimension>                    FixedImageType;

  // Moving Image Type
  typedef itk::Image<char,dimension>                     MovingImageType;

  // Transform Type
  typedef itk::TranslationTransform< double,dimension >  TransformType;

  // Optimizer Type
  typedef itk::GradientDescentOptimizer                  OptimizerType;

  // Metric Type
  typedef itk::MeanReciprocalSquareDifferenceImageToImageMetric<
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
  TransformType::Pointer      trasform      = TransformType::New();
  FixedImageType::Pointer     fixedImage    = FixedImageType::New();
  MovingImageType::Pointer    movingImage   = MovingImageType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();


  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetFixedImage(    fixedImage    );
  registration->SetMovingImage(   movingImage   );
  registration->SetInterpolator(  interpolator  );


  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}

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

#include "itkTranslationTransform.h"
#include "itkEuclideanDistancePointMetric.h"
#include "itkLevenbergMarquardtOptimizer.h"
#include "itkPointSetToPointSetRegistrationMethod.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkPointSetToImageFilter.h"
#include <iostream>

/**
 *
 *  This program tests the registration of a PointSet against an other PointSet.
 *
 */

int itkPointSetToPointSetRegistrationTest(int, char* [] )
{

//------------------------------------------------------------
// Create two simple point sets
//------------------------------------------------------------
  typedef itk::PointSet< float, 2 >   FixedPointSetType;
  FixedPointSetType::Pointer fixedPointSet = FixedPointSetType::New();

  const unsigned int numberOfPoints = 500;

  fixedPointSet->SetPointData( FixedPointSetType::PointDataContainer::New() );

  fixedPointSet->GetPoints()->Reserve( numberOfPoints );
  fixedPointSet->GetPointData()->Reserve( numberOfPoints );

  FixedPointSetType::PointType  point;

  unsigned int id = 0;
  for(unsigned int i=0;i<numberOfPoints/2;i++)
    {
    point[0]=0;
    point[1]=i;
    fixedPointSet->SetPoint( id++, point );
    }
  for(unsigned int i=0;i<numberOfPoints/2;i++)
    {
    point[0]=i;
    point[1]=0;
    fixedPointSet->SetPoint( id++, point );
    }

 // Moving Point Set
  typedef itk::PointSet< float, 2 >  MovingPointSetType;
  MovingPointSetType::Pointer movingPointSet = MovingPointSetType::New();


  movingPointSet->SetPointData( MovingPointSetType::PointDataContainer::New() );

  movingPointSet->GetPoints()->Reserve( numberOfPoints );
  movingPointSet->GetPointData()->Reserve( numberOfPoints );

  id = 0;
  for(unsigned int i=0;i<numberOfPoints/2;i++)
    {
    point[0]=0;
    point[1]=i;
    movingPointSet->SetPoint( id++, point );
    }
  for(unsigned int i=0;i<numberOfPoints/2;i++)
    {
    point[0]=i;
    point[1]=0;
    movingPointSet->SetPoint( id++, point );
    }

//-----------------------------------------------------------
// Set up  the Metric
//-----------------------------------------------------------
  typedef itk::EuclideanDistancePointMetric<
    FixedPointSetType,
    MovingPointSetType>
    MetricType;

  typedef MetricType::TransformType                 TransformBaseType;
  typedef TransformBaseType::ParametersType         ParametersType;

  MetricType::Pointer  metric = MetricType::New();


//-----------------------------------------------------------
// Set up a Transform
//-----------------------------------------------------------

  typedef itk::TranslationTransform<
    double,
    2 >         TransformType;

  TransformType::Pointer transform = TransformType::New();


  // Optimizer Type
  typedef itk::LevenbergMarquardtOptimizer OptimizerType;

  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  optimizer->SetUseCostFunctionGradient(false);

  // Registration Method
  typedef itk::PointSetToPointSetRegistrationMethod<
    FixedPointSetType,
    MovingPointSetType >    RegistrationType;


  RegistrationType::Pointer   registration  = RegistrationType::New();

  // Scale the translation components of the Transform in the Optimizer
  OptimizerType::ScalesType scales( transform->GetNumberOfParameters() );
  scales.Fill( 1.0 );


  unsigned long   numberOfIterations =   100;
  double          gradientTolerance  =  1e-1; // convergence criterion
  double          valueTolerance =  1e-1; // convergence criterion
  double          epsilonFunction =  1e-9; // convergence criterion


  optimizer->SetScales( scales );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetValueTolerance(valueTolerance);
  optimizer->SetGradientTolerance(gradientTolerance);
  optimizer->SetEpsilonFunction(epsilonFunction);

  // Start from an Identity transform (in a normal case, the user
  // can probably provide a better guess than the identity...
  transform->SetIdentity();

  registration->SetInitialTransformParameters( transform->GetParameters() );

  //------------------------------------------------------
  // Connect all the components required for Registration
  //------------------------------------------------------
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetFixedPointSet( fixedPointSet );
  registration->SetMovingPointSet(   movingPointSet   );


//------------------------------------------------------------
// Set up transform parameters
//------------------------------------------------------------
  ParametersType parameters( transform->GetNumberOfParameters() );

  // initialize the offset/vector part
  for( unsigned int k = 0; k < 2; k++ )
    {
    parameters[k]= 10.0;
    }

  transform->SetParameters(parameters);
  registration->SetInitialTransformParameters( transform->GetParameters() );

  try
    {
    registration->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << e << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Solution = " << transform->GetParameters() << std::endl;

  if((vnl_math_abs(transform->GetParameters()[0])>1.0)
    ||
    (vnl_math_abs(transform->GetParameters()[1])>1.0)
    )
    {
    return EXIT_FAILURE;
    }

  /** Test with the danielsson distance map */
  typedef itk::Image<unsigned char,2>  BinaryImageType;
  typedef itk::Image<unsigned short,2> ImageType;

  typedef itk::PointSetToImageFilter<FixedPointSetType,BinaryImageType> PSToImageFilterType;
  PSToImageFilterType::Pointer psToImageFilter = PSToImageFilterType::New();

  psToImageFilter->SetInput(fixedPointSet);
  double origin[2] = {0.0, 0.0}, spacing[2] = {1.0, 1.0};
  psToImageFilter->SetSpacing(spacing);
  psToImageFilter->SetOrigin(origin);
  std::cout << "Spacing and origin set: ["
            << psToImageFilter->GetSpacing() << "], ,["
            << psToImageFilter->GetOrigin() << "]" << std::endl;
  psToImageFilter->Update();
  std::cout << "psToImageFilter: " << psToImageFilter << std::endl;

  BinaryImageType::Pointer binaryImage = psToImageFilter->GetOutput();

  typedef itk::DanielssonDistanceMapImageFilter<BinaryImageType,ImageType> DDFilterType;
  DDFilterType::Pointer ddFilter = DDFilterType::New();
  ddFilter->SetInput(binaryImage);
  ddFilter->Update();

  metric->SetDistanceMap(ddFilter->GetOutput());
  metric->ComputeSquaredDistanceOn();

   // initialize the offset/vector part
  for( unsigned int k = 0; k < 2; k++ )
    {
    parameters[k]= 10.0;
    }

  transform->SetParameters(parameters);
  registration->SetInitialTransformParameters( transform->GetParameters() );

  try
    {
    registration->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << e << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Solution = " << transform->GetParameters() << std::endl;

  if((vnl_math_abs(transform->GetParameters()[0])>1.0)
    ||
    (vnl_math_abs(transform->GetParameters()[1])>1.0)
    )
    {
    return EXIT_FAILURE;
    }


  std::cout << "TEST DONE" << std::endl;

  return EXIT_SUCCESS;

}

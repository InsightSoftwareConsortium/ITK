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
// This example illustrates how to perform Iterative Closest Point (ICP)
// registration in ITK using sets of 3D points.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkTranslationTransform.h"
#include "itkEuclideanDistancePointMetric.h"
#include "itkLevenbergMarquardtOptimizer.h"
#include "itkPointSetToPointSetRegistrationMethod.h"
// Software Guide : EndCodeSnippet

#include <fstream>


int main(int argc, char * argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Arguments Missing. " << std::endl;
    std::cerr <<
      "Usage:  IterativeClosestPoint2   fixedPointsFile  movingPointsFile "
      << std::endl;
    return EXIT_FAILURE;
    }

// Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 3;

  typedef itk::PointSet< float, Dimension >   PointSetType;

  PointSetType::Pointer fixedPointSet  = PointSetType::New();
  PointSetType::Pointer movingPointSet = PointSetType::New();

  typedef PointSetType::PointType     PointType;

  typedef PointSetType::PointsContainer  PointsContainer;

  PointsContainer::Pointer fixedPointContainer  = PointsContainer::New();
  PointsContainer::Pointer movingPointContainer = PointsContainer::New();

  PointType fixedPoint;
  PointType movingPoint;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Read the file containing coordinates of fixed points.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  std::ifstream   fixedFile;
  fixedFile.open( argv[1] );
  if( fixedFile.fail() )
    {
    std::cerr << "Error opening points file with name : " << std::endl;
    std::cerr << argv[1] << std::endl;
    return EXIT_FAILURE;
    }

  unsigned int pointId = 0;
  fixedFile >> fixedPoint;
  while( !fixedFile.eof() )
    {
    fixedPointContainer->InsertElement( pointId, fixedPoint );
    fixedFile >> fixedPoint;
    pointId++;
    }
  fixedPointSet->SetPoints( fixedPointContainer );
  std::cout <<
    "Number of fixed Points = " <<
    fixedPointSet->GetNumberOfPoints() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Read the file containing coordinates of moving points.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  std::ifstream   movingFile;
  movingFile.open( argv[2] );
  if( movingFile.fail() )
    {
    std::cerr << "Error opening points file with name : " << std::endl;
    std::cerr << argv[2] << std::endl;
    return EXIT_FAILURE;
    }

  pointId = 0;
  movingFile >> movingPoint;
  while( !movingFile.eof() )
    {
    movingPointContainer->InsertElement( pointId, movingPoint );
    movingFile >> movingPoint;
    pointId++;
    }
  movingPointSet->SetPoints( movingPointContainer );
  std::cout << "Number of moving Points = "
    << movingPointSet->GetNumberOfPoints() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Set up the Metric.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::EuclideanDistancePointMetric<
                                    PointSetType,
                                    PointSetType>
                                                    MetricType;

  MetricType::Pointer  metric = MetricType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Set up a Transform.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension >      TransformType;

  TransformType::Pointer transform = TransformType::New();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Set up a the Optimizer and RegistrationMethod.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::LevenbergMarquardtOptimizer OptimizerType;

  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  optimizer->SetUseCostFunctionGradient(false);

  typedef itk::PointSetToPointSetRegistrationMethod<
                                            PointSetType,
                                            PointSetType >
                                                    RegistrationType;


  RegistrationType::Pointer   registration  = RegistrationType::New();

  // Scale the translation components of the Transform in the Optimizer
  OptimizerType::ScalesType scales( transform->GetNumberOfParameters() );

  const double translationScale = 1000.0;   // dynamic range of translations
  const double rotationScale    =    1.0;   // dynamic range of rotations

  scales[0] = 1.0 / rotationScale;
  scales[1] = 1.0 / rotationScale;
  scales[2] = 1.0 / rotationScale;
  scales[3] = 1.0 / translationScale;
  scales[4] = 1.0 / translationScale;
  scales[5] = 1.0 / translationScale;

  unsigned long   numberOfIterations =  2000;
  double          gradientTolerance  =  1e-4;   // convergence criterion
  double          valueTolerance     =  1e-4;   // convergence criterion
  double          epsilonFunction    =  1e-5;   // convergence criterion


  optimizer->SetScales( scales );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetValueTolerance( valueTolerance );
  optimizer->SetGradientTolerance( gradientTolerance );
  optimizer->SetEpsilonFunction( epsilonFunction );

  // Start from an Identity transform (in a normal case, the user
  // can probably provide a better guess than the identity...
  transform->SetIdentity();

  registration->SetInitialTransformParameters( transform->GetParameters() );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Connect all the components required for Registration
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetFixedPointSet( fixedPointSet );
  registration->SetMovingPointSet(   movingPointSet   );


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
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}

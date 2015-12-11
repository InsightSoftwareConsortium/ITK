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

// Software Guide : BeginLatex
//
// The first step is to include the relevant headers.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkEuler3DTransform.h"
#include "itkEuclideanDistancePointMetric.h"
#include "itkLevenbergMarquardtOptimizer.h"
#include "itkPointSetToPointSetRegistrationMethod.h"
#include <iostream>
#include <fstream>
// Software Guide : EndCodeSnippet

class CommandIterationUpdate : public itk::Command
{
public:
  typedef CommandIterationUpdate  Self;
  typedef itk::Command            Superclass;
  typedef itk::SmartPointer<Self> Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate() {};

public:
  typedef itk::LevenbergMarquardtOptimizer     OptimizerType;
  typedef const OptimizerType *                OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    OptimizerPointer optimizer = dynamic_cast< OptimizerPointer >( object );
    if( optimizer == ITK_NULLPTR )
      {
      itkExceptionMacro( "Could not cast optimizer." );
      }

    if( ! itk::IterationEvent().CheckEvent( &event ) )
      {
      return;
      }

    std::cout << "Value = " << optimizer->GetCachedValue() << std::endl;
    std::cout << "Position = "  << optimizer->GetCachedCurrentPosition();
    std::cout << std::endl << std::endl;
    }
};


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

  const unsigned int Dimension = 3;

// Software Guide : BeginLatex
//
// First, define the necessary types for the moving and fixed point sets.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
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

  // Read the file containing coordinates of fixed points.
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
    "Number of fixed Points = " << fixedPointSet->GetNumberOfPoints()
    << std::endl;

  // Read the file containing coordinates of moving points.
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
  std::cout <<
    "Number of moving Points = "
    << movingPointSet->GetNumberOfPoints() << std::endl;


// Software Guide : BeginLatex
//
// After the points are read in from files, setup the metric to be used
// later by the registration.
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
// Next, setup the tranform, optimizers, and registration.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::Euler3DTransform< double >      TransformType;

  TransformType::Pointer transform = TransformType::New();


  // Optimizer Type
  typedef itk::LevenbergMarquardtOptimizer OptimizerType;

  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  optimizer->SetUseCostFunctionGradient(false);

  // Registration Method
  typedef itk::PointSetToPointSetRegistrationMethod<
                                            PointSetType,
                                            PointSetType >
                                                    RegistrationType;


  RegistrationType::Pointer   registration  = RegistrationType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Scale the translation components of the Transform in the Optimizer
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  OptimizerType::ScalesType scales( transform->GetNumberOfParameters() );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next, set the scales and ranges for translations and rotations in the
// transform. Also, set the convergence criteria and number of iterations
// to be used by the optimizer.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
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
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Here we start with an identity transform, although the user will usually
// be able to provide a better guess than this.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  transform->SetIdentity();
// Software Guide : EndCodeSnippet

  registration->SetInitialTransformParameters( transform->GetParameters() );

// Software Guide : BeginLatex
//
// Connect all the components required for the registration.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetFixedPointSet( fixedPointSet );
  registration->SetMovingPointSet(   movingPointSet   );
// Software Guide : EndCodeSnippet
//
  // Connect an observer
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );

  try
    {
    registration->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Solution = " << transform->GetParameters() << std::endl;
  std::cout << "Stopping condition: " << optimizer->GetStopConditionDescription() << std::endl;

  return EXIT_SUCCESS;
}

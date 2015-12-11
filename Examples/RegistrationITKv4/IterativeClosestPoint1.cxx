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
// registration in ITK. The main class featured in this section is the
// \doxygen{EuclideanDistancePointMetric}.
//
// Software Guide : EndLatex

// Software Guide : BeginLatex
//
// The first step is to include the relevant headers.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkTranslationTransform.h"
#include "itkEuclideanDistancePointMetric.h"
#include "itkLevenbergMarquardtOptimizer.h"
#include "itkPointSetToPointSetRegistrationMethod.h"
// Software Guide : EndCodeSnippet

#include <iostream>
#include <fstream>

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
    std::cerr
      << "Usage:  IterativeClosestPoint1   fixedPointsFile  movingPointsFile "
      << std::endl;
    return EXIT_FAILURE;
    }

// Software Guide : BeginLatex
//
// Next, define the necessary types for the fixed and moving pointsets and
// point containers.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;

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
    "Number of fixed Points = " <<
    fixedPointSet->GetNumberOfPoints() << std::endl;

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
  std::cout << "Number of moving Points = "
    << movingPointSet->GetNumberOfPoints() << std::endl;

// Software Guide : BeginLatex
//
// After the points are read in from files, set up the metric type.
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
// Now, setup the transform, optimizers, and registration method using the
// point set types defined earlier.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension >      TransformType;

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

  // Scale the translation components of the Transform in the Optimizer
  OptimizerType::ScalesType scales( transform->GetNumberOfParameters() );
  scales.Fill( 0.01 );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next we setup the convergence criteria, and other properties required
// by the optimizer.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  unsigned long   numberOfIterations =  100;
  double          gradientTolerance  =  1e-5;    // convergence criterion
  double          valueTolerance     =  1e-5;    // convergence criterion
  double          epsilonFunction    =  1e-6;   // convergence criterion


  optimizer->SetScales( scales );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetValueTolerance( valueTolerance );
  optimizer->SetGradientTolerance( gradientTolerance );
  optimizer->SetEpsilonFunction( epsilonFunction );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// In this case we start from an identity transform, but in reality the user
// will usually be able to provide a better guess than this.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  transform->SetIdentity();
// Software Guide : EndCodeSnippet

  registration->SetInitialTransformParameters( transform->GetParameters() );

// Software Guide : BeginLatex
//
// Finally, connect all the components required for the registration, and an
// observer.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetFixedPointSet( fixedPointSet );
  registration->SetMovingPointSet(   movingPointSet   );

  // Connect an observer
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );
// Software Guide : EndCodeSnippet

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
  return EXIT_SUCCESS;
}

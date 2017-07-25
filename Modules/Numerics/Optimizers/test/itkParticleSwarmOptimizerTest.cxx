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

#include <iostream>
#include "itkParticleSwarmOptimizer.h"
#include "itkParticleSwarmOptimizerTestFunctions.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

typedef  itk::ParticleSwarmOptimizer  OptimizerType;
static OptimizerType::RandomVariateGeneratorType::IntegerType seedOffset = 0;

/**
 * Test using a 1D function with two minima, two parabolas. Check that the
 * optimizer converges to the global minimum if it is initialized inside the
 * domain of either parabolas (runs the optimizer once with initial guess in
 * each of the domains).
 */
int PSOTest1();


/**
 * Test using a 2D quadratic function (single minimum), check that converges
 * correctly.
 */
int PSOTest2();


/**
 * Test using the 2D Rosenbrock function.
 */
int PSOTest3();

bool verboseFlag = false;

/**
* The particle swarm optimizer is a stochastic algorithm. Consequentially, we run
* the same test multiple times and deem it a success if the number of successful
* runs is above a threshold.
*/
int itkParticleSwarmOptimizerTest(int argc, char* argv[] )
{
  if( argc > 1 )
    {
    verboseFlag = atoi(argv[1]) ? true : false;
    }

  unsigned int i, allIterations=10;
  double threshold = 0.8;
  unsigned int success1, success2, success3;

  std::cout << "Particle Swarm Optimizer Test \n \n";

 success1 = success2 = success3 = 0;
  for( i=0; i<allIterations; i++ )
    {
    if( EXIT_SUCCESS == PSOTest1() )
      {
      success1++;
      }
    if( EXIT_SUCCESS == PSOTest2() )
      {
      success2++;
      }
    if( EXIT_SUCCESS == PSOTest3() )
      {
      success3++;
      }
    }

  std::cout<< "All Tests Completed."<< std::endl;

  if( static_cast<double>(success1)/ static_cast<double>(allIterations) <= threshold ||
      static_cast<double>(success2)/ static_cast<double>(allIterations) <= threshold ||
      static_cast<double>(success3)/ static_cast<double>(allIterations) <= threshold )
    {
    std::cout<<"[FAILURE]\n";
    return EXIT_FAILURE;
    }
  std::cout << "[SUCCESS]" << std::endl;
  return EXIT_SUCCESS;
}


int PSOTest1()
{
  std::cout << "Particle Swarm Optimizer Test 1 [f(x) = if(x<0) x^2+4x; else 2x^2-8x]\n";
  std::cout << "-------------------------------\n";

  double knownParameters = 2.0;

    //the function we want to optimize
  itk::ParticleSwarmTestF1::Pointer costFunction =
    itk::ParticleSwarmTestF1::New();

  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();
  itkOptimizer->UseSeedOn();
  itkOptimizer->SetSeed(8775070 + seedOffset++);

         // set optimizer parameters
  OptimizerType::ParameterBoundsType bounds;
  bounds.push_back( std::make_pair( -10, 10 ) );
  unsigned int numberOfParticles = 10;
  unsigned int maxIterations = 100;
  double xTolerance = 0.1;
  double fTolerance = 0.001;
  OptimizerType::ParametersType initialParameters( 1 ), finalParameters;

  itkOptimizer->SetParameterBounds( bounds );
  itkOptimizer->SetNumberOfParticles( numberOfParticles );
  itkOptimizer->SetMaximalNumberOfIterations( maxIterations );
  itkOptimizer->SetParametersConvergenceTolerance( xTolerance,
                                                   costFunction->GetNumberOfParameters() );
  itkOptimizer->SetFunctionConvergenceTolerance( fTolerance );
  itkOptimizer->SetCostFunction( costFunction.GetPointer() );

  //observe the iterations
  itk::CommandIterationUpdateParticleSwarm::Pointer observer =
   itk::CommandIterationUpdateParticleSwarm::New();
  if( verboseFlag )
    {
   itkOptimizer->AddObserver( itk::IterationEvent(), observer );
    itkOptimizer->AddObserver( itk::StartEvent(), observer );
    }

  try
    {
    initialParameters[0] =  -9;
    itkOptimizer->SetInitialPosition( initialParameters );
    itkOptimizer->StartOptimization();
    finalParameters = itkOptimizer->GetCurrentPosition();

              //check why we stopped and see if the optimization succeeded
    std::cout<<"Reason for stopping optimization:\n";
    std::cout<<"\t"<<itkOptimizer->GetStopConditionDescription()<<"\n";
    finalParameters = itkOptimizer->GetCurrentPosition();
    std::cout << "Known parameters   = " << knownParameters<<"   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;
    if( fabs( finalParameters[0] - knownParameters ) > xTolerance )
      {
      std::cout << "[Test 1 FAILURE]" << std::endl;
      return EXIT_FAILURE;
      }
             //run optimization again with a different initial value
    initialParameters[0] =  9;
    itkOptimizer->SetInitialPosition( initialParameters );

    if( verboseFlag )
      {
      observer->Reset();
      }

    itkOptimizer->ClearSwarm();
    itkOptimizer->StartOptimization();
    finalParameters = itkOptimizer->GetCurrentPosition();

              //check why we stopped and see if the optimization succeeded
    std::cout<<"Reason for stopping optimization:\n";
    std::cout<<"\t"<<itkOptimizer->GetStopConditionDescription()<<"\n";
    finalParameters = itkOptimizer->GetCurrentPosition();
    std::cout << "Known parameters   = " << knownParameters<<"   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;
    if( fabs( finalParameters[0] - knownParameters ) > xTolerance )
      {
      std::cout << "[Test 1 FAILURE]" << std::endl;
      return EXIT_FAILURE;
      }
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "[Test 1 FAILURE]" << std::endl;
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[Test 1 SUCCESS]" << std::endl;
  return EXIT_SUCCESS;
}


int PSOTest2()
{
  std::cout << "Particle Swarm Optimizer Test 2 [f(x) = 1/2 x^T A x - b^T x]\n";
  std::cout << "----------------------------------\n";

  itk::Array<double> knownParameters( 2 );
  knownParameters[0] = 2.0;
  knownParameters[1] = -2.0;

    //the function we want to optimize
  itk::ParticleSwarmTestF2::Pointer costFunction =
    itk::ParticleSwarmTestF2::New();

  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();
  itkOptimizer->UseSeedOn();
  itkOptimizer->SetSeed(8775070 + seedOffset++);

         // set optimizer parameters
  OptimizerType::ParameterBoundsType bounds;
  bounds.push_back( std::make_pair( -10, 10 ) );
  bounds.push_back( std::make_pair( -10, 10 ) );
  unsigned int numberOfParticles = 10;
  unsigned int maxIterations = 100;
  double xTolerance = 0.1;
  double fTolerance = 0.001;
  OptimizerType::ParametersType initialParameters( 2 ), finalParameters;

  itkOptimizer->SetParameterBounds( bounds );
  itkOptimizer->SetNumberOfParticles( numberOfParticles );
  itkOptimizer->SetMaximalNumberOfIterations( maxIterations );
  itkOptimizer->SetParametersConvergenceTolerance( xTolerance,
                                                   costFunction->GetNumberOfParameters() );
  itkOptimizer->SetFunctionConvergenceTolerance( fTolerance );
  itkOptimizer->SetCostFunction( costFunction.GetPointer() );

  //observe the iterations
  itk::CommandIterationUpdateParticleSwarm::Pointer observer =
      itk::CommandIterationUpdateParticleSwarm::New();
  if( verboseFlag )
    {
    itkOptimizer->AddObserver( itk::IterationEvent(), observer );
    }

  try
    {
    initialParameters[0] =  9;
    initialParameters[1] = -9;
    itkOptimizer->SetInitialPosition( initialParameters );
    itkOptimizer->StartOptimization();
    finalParameters = itkOptimizer->GetCurrentPosition();

              //check why we stopped and see if the optimization succeeded
    std::cout<<"Reason for stopping optimization:\n";
    std::cout<<"\t"<<itkOptimizer->GetStopConditionDescription()<<"\n";
    finalParameters = itkOptimizer->GetCurrentPosition();
    std::cout << "Known parameters   = " << knownParameters<<"   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;
    if( fabs( finalParameters[0] - knownParameters[0] ) > xTolerance ||
        fabs( finalParameters[1] - knownParameters[1] ) > xTolerance )
      {
      std::cout << "[Test 2 FAILURE]" << std::endl;
      return EXIT_FAILURE;
      }
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "[Test 2 FAILURE]" << std::endl;
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[Test 2 SUCCESS]" << std::endl;
  return EXIT_SUCCESS;
}


int PSOTest3()
{
  std::cout << "Particle Swarm Optimizer Test 3 [f(x,y) = (1-x)^2 + 100(y-x^2)^2]\n";
  std::cout << "----------------------------------\n";


  const double tolerance = 1e-16;

  itk::Array<double> knownParameters( 2 );
  knownParameters[0] = 1.0;
  knownParameters[1] = 1.0;

    //the function we want to optimize
  itk::ParticleSwarmTestF3::Pointer costFunction =
    itk::ParticleSwarmTestF3::New();

  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();
  itkOptimizer->UseSeedOn();
  itkOptimizer->SetSeed(8775070 + seedOffset++);

  // set optimizer parameters
  OptimizerType::ParameterBoundsType bounds;
  bounds.push_back( std::make_pair( -100, 100 ) );
  bounds.push_back( std::make_pair( -100, 100 ) );
  unsigned int numberOfParticles = 100;
  unsigned int maxIterations = 200;
  double xTolerance = 0.1;
  double fTolerance = 0.01;
  OptimizerType::ParametersType initialParameters( 2 );
  OptimizerType::ParametersType finalParameters;

  //Exercise Get/Set methods
  itkOptimizer->PrintSwarmOn();
  itkOptimizer->SetParameterBounds( bounds );

  itkOptimizer->SetNumberOfParticles( numberOfParticles );
  if( numberOfParticles != itkOptimizer->GetNumberOfParticles())
    {
    std::cerr << "Error in Set/Get methods for NumberOfParticles";
    return EXIT_FAILURE;
    }

  itkOptimizer->SetMaximalNumberOfIterations( maxIterations );
  if( maxIterations != itkOptimizer->GetMaximalNumberOfIterations())
    {
    std::cerr << "Error in Set/Get methods for maxIterations";
    return EXIT_FAILURE;
    }

  unsigned int  numberOfGenerationsWithMinimalImprovement = 1;
  itkOptimizer->SetNumberOfGenerationsWithMinimalImprovement(
          numberOfGenerationsWithMinimalImprovement );
  if( itkOptimizer->GetNumberOfGenerationsWithMinimalImprovement() !=
                      numberOfGenerationsWithMinimalImprovement )
    {
    std::cerr << "Error in Set/Get methods for number of generations allowed with minimal improvement ";
    return EXIT_FAILURE;
    }

  itkOptimizer->SetParametersConvergenceTolerance( xTolerance,
                                                   costFunction->GetNumberOfParameters() );

  itkOptimizer->SetFunctionConvergenceTolerance( fTolerance );
  if( fabs(itkOptimizer->GetFunctionConvergenceTolerance() - fTolerance) > tolerance )
    {
    std::cerr << "Error in Set/Get method for FunctionConvergenceTolerance";
    return EXIT_FAILURE;
    }
  itkOptimizer->SetCostFunction( costFunction.GetPointer() );

  double percentageParticlesConverged = 0.6;
  itkOptimizer->SetPercentageParticlesConverged( percentageParticlesConverged );
  if( fabs( itkOptimizer->GetPercentageParticlesConverged() - percentageParticlesConverged ) > tolerance )
    {
    std::cerr << "Error in Set/Get methods for percentage particles converged parameter";
    return EXIT_FAILURE;
    }

  double inertiaCoefficient = 0.7298;
  itkOptimizer->SetInertiaCoefficient( inertiaCoefficient );
  if ( fabs( itkOptimizer->GetInertiaCoefficient() - inertiaCoefficient ) )
    {
    std::cerr << "Error in Set/Get method for inertia coefficent parameter";
    return EXIT_FAILURE;
    }

  double personalCoefficient = 1.496;
  itkOptimizer->SetPersonalCoefficient( personalCoefficient );
  if ( fabs( itkOptimizer->GetPersonalCoefficient() - personalCoefficient ) )
    {
    std::cerr << "Error in Set/Get method for personal coefficent parameter";
    return EXIT_FAILURE;
    }

  double gobalCoefficient = 1.496;
  itkOptimizer->SetGlobalCoefficient( gobalCoefficient );
  if ( fabs( itkOptimizer->GetGlobalCoefficient() - gobalCoefficient ) )
    {
    std::cerr << "Error in Set/Get method for gobal coefficent parameter";
    return EXIT_FAILURE;
    }

  //Exercise the print self method
  itkOptimizer->Print( std::cout );

  //observe the iterations
  itk::CommandIterationUpdateParticleSwarm::Pointer observer =
      itk::CommandIterationUpdateParticleSwarm::New();

  if( verboseFlag )
    {
    itkOptimizer->AddObserver( itk::IterationEvent(), observer );
    itkOptimizer->AddObserver( itk::StartEvent(), observer );
    }

  try
    {

    initialParameters[0] =  50;
    initialParameters[1] = 50;
    itkOptimizer->SetInitialPosition( initialParameters );
    itkOptimizer->StartOptimization();

              //check why we stopped and see if the optimization succeeded
    std::cout<<"Reason for stopping optimization:\n";
    std::cout<<"\t"<<itkOptimizer->GetStopConditionDescription()<<"\n";
    finalParameters = itkOptimizer->GetCurrentPosition();
    std::cout << "Known parameters   = " << knownParameters<<"   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;
    if( fabs( finalParameters[0] - knownParameters[0] ) > xTolerance ||
        fabs( finalParameters[1] - knownParameters[1] ) > xTolerance )
      {
      std::cout << "[Test 3 FAILURE]" << std::endl;
      return EXIT_FAILURE;
      }

                //initial position near known minimum (1,1) - for PSO this
                //makes no difference when the initial swarm is generated using
                //a uniform distribution
    initialParameters[0] =  10;
    initialParameters[1] = 10;
    itkOptimizer->SetInitialPosition( initialParameters );

    //reset the iteration count done by the observer
    if( verboseFlag )
      {
      observer->Reset();
      }

    itkOptimizer->ClearSwarm();
    itkOptimizer->StartOptimization();

              //check why we stopped and see if the optimization succeeded
    std::cout<<"Reason for stopping optimization:\n";
    std::cout<<"\t"<<itkOptimizer->GetStopConditionDescription()<<"\n";
    finalParameters = itkOptimizer->GetCurrentPosition();
    std::cout << "Known parameters   = " << knownParameters<<"   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;
    if( fabs( finalParameters[0] - knownParameters[0] ) > xTolerance ||
        fabs( finalParameters[1] - knownParameters[1] ) > xTolerance )
      {
      std::cout << "[Test 3 FAILURE]" << std::endl;
      return EXIT_FAILURE;
      }

                //initial position near known minimum (1,1) - potentially reduce
                //the number of iterations by initializing particles using a normal
                //distribution centered on initial parameter values
    initialParameters[0] =  10;
    initialParameters[1] = 10;
    itkOptimizer->SetInitialPosition( initialParameters );

    itkOptimizer->InitializeNormalDistributionOn();
    if( !itkOptimizer->GetInitializeNormalDistribution() )
      {
      std::cerr << " Error in Set/Get methods for initialize normal distribution ";
      return EXIT_FAILURE;
      }

    //reset the iteration count done by the observer
    if( verboseFlag )
      {
      observer->Reset();
      }

    itkOptimizer->ClearSwarm();
    itkOptimizer->StartOptimization();

    //check why we stopped and see if the optimization succeeded
    std::cout<<"Reason for stopping optimization:\n";
    std::cout<<"\t"<<itkOptimizer->GetStopConditionDescription()<<"\n";
    finalParameters = itkOptimizer->GetCurrentPosition();
    std::cout << "Known parameters   = " << knownParameters<<"   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;
    if( fabs( finalParameters[0] - knownParameters[0] ) > xTolerance ||
        fabs( finalParameters[1] - knownParameters[1] ) > xTolerance )
      {
      std::cout << "[Test 3 FAILURE]" << std::endl;
      return EXIT_FAILURE;
      }
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "[Test 3 FAILURE]" << std::endl;
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[Test 3 SUCCESS]" << std::endl;
  return EXIT_SUCCESS;
}

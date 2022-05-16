/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/


#include <iostream>
#include "itkInitializationBiasedParticleSwarmOptimizer.h"
#include "itkParticleSwarmOptimizerTestFunctions.h"
#include "itkTestingMacros.h"

using OptimizerType = itk::InitializationBiasedParticleSwarmOptimizer;
static OptimizerType::RandomVariateGeneratorType::IntegerType seedOffset = 0;

/**
 * Test using a 1D function with two minima, two parabolas. Check that the
 * optimizer converges to the global minimum if it is initialized inside the
 * domain of either parabolas (runs the optimizer once with initial guess in
 * each of the domains).
 */
int IBPSOTest1(typename OptimizerType::CoefficientType,
               typename OptimizerType::CoefficientType,
               typename OptimizerType::CoefficientType,
               typename OptimizerType::CoefficientType);


/**
 * Test using a 2D quadratic function (single minimum), check that converges
 * correctly.
 */
int IBPSOTest2(typename OptimizerType::CoefficientType,
               typename OptimizerType::CoefficientType,
               typename OptimizerType::CoefficientType,
               typename OptimizerType::CoefficientType);


/**
 * Test using the 2D Rosenbrock function.
 */
int IBPSOTest3(typename OptimizerType::CoefficientType,
               typename OptimizerType::CoefficientType,
               typename OptimizerType::CoefficientType,
               typename OptimizerType::CoefficientType);

bool initalizationBasedTestVerboseFlag = false;

/**
 * The particle swarm optimizer is a stochastic algorithm. Consequentially, we run
 * the same test multiple times and deem it a success if the number of successful
 * runs is above a threshold.
 */
int
itkInitializationBiasedParticleSwarmOptimizerTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inertiaCoefficient"
              << " personalCoefficient"
              << " globalCoefficient"
              << " initializationCoefficient"
              << " [initalizationBasedTestVerboseFlag]" << std::endl;
    return EXIT_FAILURE;
  }

  if (argc > 5)
  {
    initalizationBasedTestVerboseFlag = std::stoi(argv[5]) ? true : false;
  }

  unsigned int i, allIterations = 10;
  double       threshold = 0.8;
  unsigned int success1, success2, success3;

  std::cout << "Initialization Biased Particle Swarm Optimizer Test \n \n";

  auto inertiaCoefficient = static_cast<typename OptimizerType::CoefficientType>(std::stod(argv[1]));
  auto personalCoefficient = static_cast<typename OptimizerType::CoefficientType>(std::stod(argv[2]));
  auto globalCoefficient = static_cast<typename OptimizerType::CoefficientType>(std::stod(argv[3]));
  auto initializationCoefficient = static_cast<typename OptimizerType::CoefficientType>(std::stod(argv[4]));

  success1 = success2 = success3 = 0;
  for (i = 0; i < allIterations; ++i)
  {
    if (EXIT_SUCCESS ==
        IBPSOTest1(inertiaCoefficient, personalCoefficient, globalCoefficient, initializationCoefficient))
    {
      success1++;
    }
    if (EXIT_SUCCESS ==
        IBPSOTest2(inertiaCoefficient, personalCoefficient, globalCoefficient, initializationCoefficient))
    {
      success2++;
    }
    if (EXIT_SUCCESS ==
        IBPSOTest3(inertiaCoefficient, personalCoefficient, globalCoefficient, initializationCoefficient))
    {
      success3++;
    }
  }

  std::cout << "All Tests Completed." << std::endl;

  if (static_cast<double>(success1) / static_cast<double>(allIterations) <= threshold ||
      static_cast<double>(success2) / static_cast<double>(allIterations) <= threshold ||
      static_cast<double>(success3) / static_cast<double>(allIterations) <= threshold)
  {
    std::cout << "[FAILURE]\n";
    return EXIT_FAILURE;
  }
  std::cout << "[SUCCESS]" << std::endl;
  return EXIT_SUCCESS;
}


int
IBPSOTest1(typename OptimizerType::CoefficientType inertiaCoefficient,
           typename OptimizerType::CoefficientType personalCoefficient,
           typename OptimizerType::CoefficientType globalCoefficient,
           typename OptimizerType::CoefficientType initializationCoefficient)
{
  std::cout << "Particle Swarm Optimizer Test 1 [f(x) = if(x<0) x^2+4x; else 2x^2-8x]\n";
  std::cout << "-------------------------------\n";

  double knownParameters = 2.0;

  // the function we want to optimize
  itk::ParticleSwarmTestF1::Pointer costFunction = itk::ParticleSwarmTestF1::New();

  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    itkOptimizer, InitializationBiasedParticleSwarmOptimizer, ParticleSwarmOptimizerBase);


  itkOptimizer->SetInertiaCoefficient(inertiaCoefficient);
  ITK_TEST_SET_GET_VALUE(inertiaCoefficient, itkOptimizer->GetInertiaCoefficient());

  itkOptimizer->SetPersonalCoefficient(personalCoefficient);
  ITK_TEST_SET_GET_VALUE(personalCoefficient, itkOptimizer->GetPersonalCoefficient());

  itkOptimizer->SetGlobalCoefficient(globalCoefficient);
  ITK_TEST_SET_GET_VALUE(globalCoefficient, itkOptimizer->GetGlobalCoefficient());

  itkOptimizer->SetInitializationCoefficient(globalCoefficient);
  ITK_TEST_SET_GET_VALUE(initializationCoefficient, itkOptimizer->GetInitializationCoefficient());

  itkOptimizer->UseSeedOn();
  itkOptimizer->SetSeed(8775070 + seedOffset++);

  // set optimizer parameters
  OptimizerType::ParameterBoundsType bounds;
  bounds.push_back(std::make_pair(-10, 10));
  unsigned int                  numberOfParticles = 10;
  unsigned int                  maxIterations = 200;
  double                        xTolerance = 0.1;
  double                        fTolerance = 0.001;
  OptimizerType::ParametersType initialParameters(1), finalParameters;

  itkOptimizer->SetParameterBounds(bounds);
  itkOptimizer->SetNumberOfParticles(numberOfParticles);
  itkOptimizer->SetMaximalNumberOfIterations(maxIterations);
  itkOptimizer->SetParametersConvergenceTolerance(xTolerance, costFunction->GetNumberOfParameters());
  itkOptimizer->SetFunctionConvergenceTolerance(fTolerance);
  itkOptimizer->SetCostFunction(costFunction);

  // observe the iterations
  itk::CommandIterationUpdateParticleSwarm::Pointer observer = itk::CommandIterationUpdateParticleSwarm::New();
  if (initalizationBasedTestVerboseFlag)
  {
    itkOptimizer->AddObserver(itk::IterationEvent(), observer);
    itkOptimizer->AddObserver(itk::StartEvent(), observer);
  }

  try
  {
    initialParameters[0] = 3;
    itkOptimizer->SetInitialPosition(initialParameters);
    itkOptimizer->StartOptimization();
    finalParameters = itkOptimizer->GetCurrentPosition();

    // show why we stopped and see if the optimization succeeded

    std::cout << "Reason for stopping optimization:\n";
    std::cout << "\t" << itkOptimizer->GetStopConditionDescription() << "\n";

    finalParameters = itkOptimizer->GetCurrentPosition();

    std::cout << "Known parameters   = " << knownParameters << "   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;

    if (itk::Math::abs(finalParameters[0] - knownParameters) > xTolerance)
    {
      std::cout << "[Test 1 FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
    // run optimization again with a different initial value
    initialParameters[0] = 2.5;
    itkOptimizer->ClearSwarm();
    itkOptimizer->SetInitialPosition(initialParameters);

    if (initalizationBasedTestVerboseFlag)
    {
      observer->Reset();
    }

    itkOptimizer->StartOptimization();
    finalParameters = itkOptimizer->GetCurrentPosition();

    // show why we stopped and see if the optimization succeeded

    std::cout << "Reason for stopping optimization:\n";
    std::cout << "\t" << itkOptimizer->GetStopConditionDescription() << "\n";

    finalParameters = itkOptimizer->GetCurrentPosition();

    std::cout << "Known parameters   = " << knownParameters << "   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;

    if (itk::Math::abs(finalParameters[0] - knownParameters) > xTolerance)
    {
      std::cout << "[Test 1 FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "[Test 1 FAILURE]" << std::endl;
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation() << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[Test 1 SUCCESS]" << std::endl;
  return EXIT_SUCCESS;
}


int
IBPSOTest2(typename OptimizerType::CoefficientType inertiaCoefficient,
           typename OptimizerType::CoefficientType personalCoefficient,
           typename OptimizerType::CoefficientType globalCoefficient,
           typename OptimizerType::CoefficientType initializationCoefficient)
{
  std::cout << "Particle Swarm Optimizer Test 2 [f(x) = 1/2 x^T A x - b^T x]\n";
  std::cout << "----------------------------------\n";

  itk::Array<double> knownParameters(2);
  knownParameters[0] = 2.0;
  knownParameters[1] = -2.0;

  // the function we want to optimize
  itk::ParticleSwarmTestF2::Pointer costFunction = itk::ParticleSwarmTestF2::New();

  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    itkOptimizer, InitializationBiasedParticleSwarmOptimizer, ParticleSwarmOptimizerBase);


  itkOptimizer->SetInertiaCoefficient(inertiaCoefficient);
  ITK_TEST_SET_GET_VALUE(inertiaCoefficient, itkOptimizer->GetInertiaCoefficient());

  itkOptimizer->SetPersonalCoefficient(personalCoefficient);
  ITK_TEST_SET_GET_VALUE(personalCoefficient, itkOptimizer->GetPersonalCoefficient());

  itkOptimizer->SetGlobalCoefficient(globalCoefficient);
  ITK_TEST_SET_GET_VALUE(globalCoefficient, itkOptimizer->GetGlobalCoefficient());

  itkOptimizer->SetInitializationCoefficient(globalCoefficient);
  ITK_TEST_SET_GET_VALUE(initializationCoefficient, itkOptimizer->GetInitializationCoefficient());

  itkOptimizer->UseSeedOn();
  itkOptimizer->SetSeed(8775070 + seedOffset++);

  // set optimizer parameters
  OptimizerType::ParameterBoundsType bounds;
  bounds.push_back(std::make_pair(-10, 10));
  bounds.push_back(std::make_pair(-10, 10));
  unsigned int                  numberOfParticles = 10;
  unsigned int                  maxIterations = 200;
  double                        xTolerance = 0.1;
  double                        fTolerance = 0.001;
  OptimizerType::ParametersType initialParameters(2), finalParameters;

  itkOptimizer->SetParameterBounds(bounds);
  itkOptimizer->SetNumberOfParticles(numberOfParticles);
  itkOptimizer->SetMaximalNumberOfIterations(maxIterations);
  itkOptimizer->SetParametersConvergenceTolerance(xTolerance, costFunction->GetNumberOfParameters());
  itkOptimizer->SetFunctionConvergenceTolerance(fTolerance);
  itkOptimizer->SetCostFunction(costFunction);

  // observe the iterations
  itk::CommandIterationUpdateParticleSwarm::Pointer observer = itk::CommandIterationUpdateParticleSwarm::New();
  if (initalizationBasedTestVerboseFlag)
  {
    itkOptimizer->AddObserver(itk::IterationEvent(), observer);
  }

  try
  {
    initialParameters[0] = 9;
    initialParameters[1] = -9;
    itkOptimizer->SetInitialPosition(initialParameters);
    itkOptimizer->StartOptimization();
    finalParameters = itkOptimizer->GetCurrentPosition();

    // show why we stopped and see if the optimization succeeded

    std::cout << "Reason for stopping optimization:\n";
    std::cout << "\t" << itkOptimizer->GetStopConditionDescription() << "\n";

    finalParameters = itkOptimizer->GetCurrentPosition();

    std::cout << "Known parameters   = " << knownParameters << "   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;

    if (itk::Math::abs(finalParameters[0] - knownParameters[0]) > xTolerance ||
        itk::Math::abs(finalParameters[1] - knownParameters[1]) > xTolerance)
    {
      std::cout << "[Test 2 FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "[Test 2 FAILURE]" << std::endl;
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation() << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[Test 2 SUCCESS]" << std::endl;
  return EXIT_SUCCESS;
}

int
IBPSOTest3(typename OptimizerType::CoefficientType inertiaCoefficient,
           typename OptimizerType::CoefficientType personalCoefficient,
           typename OptimizerType::CoefficientType globalCoefficient,
           typename OptimizerType::CoefficientType initializationCoefficient)
{
  std::cout << "Particle Swarm Optimizer Test 3 [f(x,y) = (1-x)^2 + 100(y-x^2)^2]\n";
  std::cout << "----------------------------------\n";

  itk::Array<double> knownParameters(2);
  knownParameters[0] = 1.0;
  knownParameters[1] = 1.0;

  // the function we want to optimize
  itk::ParticleSwarmTestF3::Pointer costFunction = itk::ParticleSwarmTestF3::New();

  auto itkOptimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    itkOptimizer, InitializationBiasedParticleSwarmOptimizer, ParticleSwarmOptimizerBase);


  itkOptimizer->SetInertiaCoefficient(inertiaCoefficient);
  ITK_TEST_SET_GET_VALUE(inertiaCoefficient, itkOptimizer->GetInertiaCoefficient());

  itkOptimizer->SetPersonalCoefficient(personalCoefficient);
  ITK_TEST_SET_GET_VALUE(personalCoefficient, itkOptimizer->GetPersonalCoefficient());

  itkOptimizer->SetGlobalCoefficient(globalCoefficient);
  ITK_TEST_SET_GET_VALUE(globalCoefficient, itkOptimizer->GetGlobalCoefficient());

  itkOptimizer->SetInitializationCoefficient(globalCoefficient);
  ITK_TEST_SET_GET_VALUE(initializationCoefficient, itkOptimizer->GetInitializationCoefficient());

  itkOptimizer->UseSeedOn();
  itkOptimizer->SetSeed(8775070 + seedOffset++);

  // set optimizer parameters
  OptimizerType::ParameterBoundsType bounds;
  bounds.push_back(std::make_pair(-100, 100));
  bounds.push_back(std::make_pair(-100, 100));
  unsigned int                  numberOfParticles = 100;
  unsigned int                  maxIterations = 1000;
  double                        xTolerance = 0.1;
  double                        fTolerance = 0.01;
  OptimizerType::ParametersType initialParameters(2), finalParameters;

  itkOptimizer->SetParameterBounds(bounds);
  itkOptimizer->SetNumberOfParticles(numberOfParticles);
  itkOptimizer->SetMaximalNumberOfIterations(maxIterations);
  itkOptimizer->SetParametersConvergenceTolerance(xTolerance, costFunction->GetNumberOfParameters());
  itkOptimizer->SetFunctionConvergenceTolerance(fTolerance);
  itkOptimizer->SetCostFunction(costFunction);

  // observe the iterations
  itk::CommandIterationUpdateParticleSwarm::Pointer observer = itk::CommandIterationUpdateParticleSwarm::New();
  if (initalizationBasedTestVerboseFlag)
  {
    itkOptimizer->AddObserver(itk::IterationEvent(), observer);
    itkOptimizer->AddObserver(itk::StartEvent(), observer);
  }

  try
  {
    initialParameters[0] = 3;
    initialParameters[1] = 3;
    itkOptimizer->SetInitialPosition(initialParameters);
    itkOptimizer->StartOptimization();
    finalParameters = itkOptimizer->GetCurrentPosition();

    // show why we stopped and see if the optimization succeeded
    std::cout << "Reason for stopping optimization:\n";
    std::cout << "\t" << itkOptimizer->GetStopConditionDescription() << "\n";

    finalParameters = itkOptimizer->GetCurrentPosition();

    std::cout << "Known parameters   = " << knownParameters << "   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;

    if (itk::Math::abs(finalParameters[0] - knownParameters[0]) > xTolerance ||
        itk::Math::abs(finalParameters[1] - knownParameters[1]) > xTolerance)
    {
      std::cout << "[Test 3 FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }

    // initial position near known minimum (1,1) - for PSO this
    // makes no difference when the initial swarm is generated using
    // a uniform distribution
    initialParameters[0] = 10;
    initialParameters[1] = 10;
    itkOptimizer->SetInitialPosition(initialParameters);

    // reset the iteration count done by the observer
    if (initalizationBasedTestVerboseFlag)
    {
      observer->Reset();
    }

    itkOptimizer->ClearSwarm();
    itkOptimizer->StartOptimization();
    finalParameters = itkOptimizer->GetCurrentPosition();

    // show why we stopped and see if the optimization succeeded
    std::cout << "Reason for stopping optimization:\n";
    std::cout << "\t" << itkOptimizer->GetStopConditionDescription() << "\n";

    finalParameters = itkOptimizer->GetCurrentPosition();

    std::cout << "Known parameters   = " << knownParameters << "   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;

    if (itk::Math::abs(finalParameters[0] - knownParameters[0]) > xTolerance ||
        itk::Math::abs(finalParameters[1] - knownParameters[1]) > xTolerance)
    {
      std::cout << "[Test 3 FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }

    // initial position near known minimum (1,1) - potentially reduce
    // the number of iterations by initializing particles using a normal
    // distribution centered on initial parameter values
    initialParameters[0] = 10;
    initialParameters[1] = 10;
    itkOptimizer->SetInitialPosition(initialParameters);
    itkOptimizer->InitializeNormalDistributionOn();

    // reset the iteration count done by the observer
    if (initalizationBasedTestVerboseFlag)
    {
      observer->Reset();
    }

    itkOptimizer->ClearSwarm();
    itkOptimizer->StartOptimization();
    finalParameters = itkOptimizer->GetCurrentPosition();

    // show why we stopped and see if the optimization succeeded
    std::cout << "Reason for stopping optimization:\n";
    std::cout << "\t" << itkOptimizer->GetStopConditionDescription() << "\n";

    finalParameters = itkOptimizer->GetCurrentPosition();

    std::cout << "Known parameters   = " << knownParameters << "   ";
    std::cout << "Estimated parameters = " << finalParameters << std::endl;

    if (itk::Math::abs(finalParameters[0] - knownParameters[0]) > xTolerance ||
        itk::Math::abs(finalParameters[1] - knownParameters[1]) > xTolerance)
    {
      std::cout << "[Test 3 FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "[Test 3 FAILURE]" << std::endl;
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation() << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[Test 3 SUCCESS]" << std::endl;
  return EXIT_SUCCESS;
}

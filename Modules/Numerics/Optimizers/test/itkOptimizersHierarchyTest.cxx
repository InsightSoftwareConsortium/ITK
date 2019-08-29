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

#include "itkAmoebaOptimizer.h"
#include "itkConjugateGradientOptimizer.h"
#include "itkCumulativeGaussianOptimizer.h"
#include "itkLBFGSOptimizer.h"
#include "itkVersorTransformOptimizer.h"
#include "itkQuaternionRigidTransformGradientDescentOptimizer.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"


#include <iostream>


/**
 *
 *  This file performs only simple C++ tests of
 *  the base classes in the Optimizers hierarchy.
 *
 *  Nothing numerical is computed in these tests,
 *  only code conformance.
 */

int
itkOptimizersHierarchyTest(int, char *[])
{
  bool pass = true;

  using OptimizerType = itk::Optimizer;
  OptimizerType::Pointer genericOptimizer = OptimizerType::New();

  unsigned int spaceDimension = 10;

  OptimizerType::ParametersType initialPosition(spaceDimension);
  OptimizerType::ParametersType currentPosition(spaceDimension);
  OptimizerType::ScalesType     parameterScale(spaceDimension);

  parameterScale.Fill(1.5);
  initialPosition.Fill(2.0);

  genericOptimizer->SetInitialPosition(initialPosition);
  genericOptimizer->SetScales(parameterScale);

  const OptimizerType::ScalesType & parameterScaleGot = genericOptimizer->GetScales();

  const double tolerance = 1e-10;

  for (unsigned int i = 0; i < spaceDimension; i++)
  {
    if (itk::Math::abs(parameterScaleGot[i] - parameterScale[i]) > tolerance)
    {
      std::cout << "Test failed." << std::endl;
      std::cout << "Scale parameters are damaged after being set." << std::endl;
      std::cout << "Scale was set to: " << parameterScale << std::endl;
      std::cout << "Scale was got as: " << parameterScaleGot << std::endl;
      return EXIT_FAILURE;
    }
  }

  OptimizerType::ParametersType initialPositionGot = genericOptimizer->GetInitialPosition();

  for (unsigned int i = 0; i < spaceDimension; i++)
  {
    if (itk::Math::abs(initialPositionGot[i] - initialPosition[i]) > tolerance)
    {
      std::cout << "Test failed." << std::endl;
      std::cout << "InitialPosition parameters are damaged after being set." << std::endl;
      std::cout << "InitialPosition was set to: " << initialPosition << std::endl;
      std::cout << "InitialPosition was got as: " << initialPositionGot << std::endl;
      return EXIT_FAILURE;
    }
  }

  using NonLinearOptimizerType = itk::NonLinearOptimizer;
  NonLinearOptimizerType::Pointer nonLinearOptimizer = NonLinearOptimizerType::New();
  if (nonLinearOptimizer.IsNull())
  {
    pass = false;
  }

  using SingleValuedNonLinearOptimizerType = itk::SingleValuedNonLinearOptimizer;
  SingleValuedNonLinearOptimizerType::Pointer singleValuedOptimizer = SingleValuedNonLinearOptimizerType::New();
  if (singleValuedOptimizer.IsNull())
  {
    pass = false;
  }

  using AmoebaOptimizerType = itk::AmoebaOptimizer;
  AmoebaOptimizerType::Pointer amoeba = AmoebaOptimizerType::New();
  if (amoeba.IsNull())
  {
    pass = false;
  }

  using ConjugateGradientOptimizerType = itk::ConjugateGradientOptimizer;
  ConjugateGradientOptimizerType::Pointer conjugate = ConjugateGradientOptimizerType::New();
  if (conjugate.IsNull())
  {
    pass = false;
  }

  using LBFGSOptimizerType = itk::LBFGSOptimizer;
  LBFGSOptimizerType::Pointer lbfgs = LBFGSOptimizerType::New();
  if (lbfgs.IsNull())
  {
    pass = false;
  }

  // Note that a "Versor" is a Unit Quaternion
  using VersorOptimizerType = itk::VersorTransformOptimizer;
  VersorOptimizerType::Pointer versoropt = VersorOptimizerType::New();
  if (versoropt.IsNull())
  {
    pass = false;
  }

  using QuaternionOptimizerType = itk::QuaternionRigidTransformGradientDescentOptimizer;
  QuaternionOptimizerType::Pointer quaternionopt = QuaternionOptimizerType::New();
  if (quaternionopt.IsNull())
  {
    pass = false;
  }

  using OnePlusOneEvolutionaryOptimizerType = itk::OnePlusOneEvolutionaryOptimizer;
  OnePlusOneEvolutionaryOptimizerType::Pointer onePlusOne = OnePlusOneEvolutionaryOptimizerType::New();
  if (onePlusOne.IsNull())
  {
    pass = false;
  }

  using CumulativeGaussianOptimizerType = itk::CumulativeGaussianOptimizer;
  CumulativeGaussianOptimizerType::Pointer cumgaussopt = CumulativeGaussianOptimizerType::New();
  if (cumgaussopt.IsNull())
  {
    pass = false;
  }

  using CumulativeGaussianCostFunctionType = itk::CumulativeGaussianCostFunction;
  CumulativeGaussianCostFunctionType::Pointer cumgausstype = CumulativeGaussianCostFunctionType::New();
  if (cumgausstype.IsNull())
  {
    pass = false;
  }

  if (!pass)
  {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}

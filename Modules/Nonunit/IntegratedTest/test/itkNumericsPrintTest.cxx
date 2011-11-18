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
#include "itkCompositeValleyFunction.h"
#include "itkConjugateGradientOptimizer.h"
#include "itkCumulativeGaussianOptimizer.h"
#include "itkLBFGSOptimizer.h"
#include "itkLevenbergMarquardtOptimizer.h"
#include "itkMultivariateLegendrePolynomial.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkQuaternionRigidTransformGradientDescentOptimizer.h"
#include "itkVersorTransformOptimizer.h"

int itkNumericsPrintTest(int , char* [])
{
  itk::AmoebaOptimizer::Pointer AmoebaOptimizerObj =
    itk::AmoebaOptimizer::New();
  std::cout << "----------AmoebaOptimizer " << AmoebaOptimizerObj;

  itk::CacheableScalarFunction * CacheableScalarFunctionObj =
    new itk::CacheableScalarFunction;
  std::cout << "----------CacheableScalarFunction " << CacheableScalarFunctionObj;
  delete CacheableScalarFunctionObj;

  itk::ConjugateGradientOptimizer::Pointer ConjugateGradientOptimizerObj =
    itk::ConjugateGradientOptimizer::New();
  std::cout << "----------ConjugateGradientOptimizer " << ConjugateGradientOptimizerObj;

  itk::CumulativeGaussianOptimizer::Pointer CumulativeGaussianOptimizerObj =
    itk::CumulativeGaussianOptimizer::New();
  std::cout << "----------CumulativeGaussianOptimizer " << CumulativeGaussianOptimizerObj;

  itk::CumulativeGaussianCostFunction::Pointer CumulativeGaussianCostFunctionObj =
    itk::CumulativeGaussianCostFunction::New();
  std::cout << "----------CumulativeGaussianCostFunction " << CumulativeGaussianCostFunctionObj;

  itk::GradientDescentOptimizer::Pointer GradientDescentOptimizerObj =
    itk::GradientDescentOptimizer::New();
  std::cout << "----------GradientDescentOptimizer " << GradientDescentOptimizerObj;

  itk::LBFGSOptimizer::Pointer LBFGSOptimizerObj =
    itk::LBFGSOptimizer::New();
  std::cout << "----------LBFGSOptimizer " << LBFGSOptimizerObj;

  itk::LevenbergMarquardtOptimizer::Pointer LevenbergMarquardtOptimizerObj =
    itk::LevenbergMarquardtOptimizer::New();
  std::cout << "----------LevenbergMarquardtOptimizer " << LevenbergMarquardtOptimizerObj;

  typedef itk::MultivariateLegendrePolynomial PolynomialType;
  const unsigned int dimension = 3;
  const unsigned int degree    = 3;
  PolynomialType::DomainSizeType domainSize(dimension);
  itk::MultivariateLegendrePolynomial * MultivariateLegendrePolynomialObj =
    new itk::MultivariateLegendrePolynomial(dimension,degree,domainSize);
  std::cout << "----------MultivariateLegendrePolynomial " << *MultivariateLegendrePolynomialObj;
  delete MultivariateLegendrePolynomialObj;

  itk::OnePlusOneEvolutionaryOptimizer::Pointer OnePlusOneEvolutionaryOptimizerObj =
    itk::OnePlusOneEvolutionaryOptimizer::New();
  std::cout << "----------OnePlusOneEvolutionaryOptimizer " << OnePlusOneEvolutionaryOptimizerObj;

  itk::Optimizer::Pointer OptimizerObj =
    itk::Optimizer::New();
  std::cout << "----------Optimizer " << OptimizerObj;

  itk::QuaternionRigidTransformGradientDescentOptimizer::Pointer QuaternionRigidTransformGradientDescentOptimizerObj =
    itk::QuaternionRigidTransformGradientDescentOptimizer::New();
  std::cout << "----------QuaternionRigidTransformGradientDescentOptimizer " << QuaternionRigidTransformGradientDescentOptimizerObj;

  itk::RegularStepGradientDescentBaseOptimizer::Pointer RegularStepGradientDescentBaseOptimizerObj =
    itk::RegularStepGradientDescentBaseOptimizer::New();
  std::cout << "----------RegularStepGradientDescentBaseOptimizer " << RegularStepGradientDescentBaseOptimizerObj;

  itk::RegularStepGradientDescentOptimizer::Pointer RegularStepGradientDescentOptimizerObj =
    itk::RegularStepGradientDescentOptimizer::New();
  std::cout << "----------RegularStepGradientDescentOptimizer " << RegularStepGradientDescentOptimizerObj;

  itk::SingleValuedVnlCostFunctionAdaptor * SingleValuedVnlCostFunctionAdaptorObj =
    new itk::SingleValuedVnlCostFunctionAdaptor(3);
  std::cout << "----------SingleValuedVnlCostFunctionAdaptor " << SingleValuedVnlCostFunctionAdaptorObj;
  delete SingleValuedVnlCostFunctionAdaptorObj;

  itk::VersorTransformOptimizer::Pointer VersorTransformOptimizerObj =
    itk::VersorTransformOptimizer::New();
  std::cout << "----------VersorTransformOptimizer " << VersorTransformOptimizerObj;

  return 0;
}

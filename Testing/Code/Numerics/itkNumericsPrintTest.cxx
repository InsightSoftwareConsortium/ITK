/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericsPrintTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkAmoebaOptimizer.h"
#include "itkCacheableScalarFunction.h"
#include "itkCompositeValleyFunction.h"
#include "itkConjugateGradientOptimizer.h"
#include "itkCostFunction.h"
#include "itkGradientDescentOptimizer.h"
#include "itkLBFGSOptimizer.h"
#include "itkLevenbergMarquardtOptimizer.h"
#include "itkMultipleValuedNonLinearOptimizer.h"
#include "itkMultipleValuedNonLinearVnlOptimizer.h"
#include "itkMultipleValuedVnlCostFunctionAdaptor.h"
#include "itkMultivariateLegendrePolynomial.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkOptimizer.h"
#include "itkQuaternionRigidTransformGradientDescentOptimizer.h"
#include "itkRegularStepGradientDescentBaseOptimizer.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "itkSingleValuedVnlCostFunctionAdaptor.h"
#include "itkVersorTransformOptimizer.h"

int itkNumericsPrintTest(int , char* [])
{
  itk::AmoebaOptimizer::Pointer AmoebaOptimizerObj =
    itk::AmoebaOptimizer::New();
  std::cout << "----------AmoebaOptimizer " << AmoebaOptimizerObj;

#if 0
  itk::CacheableScalarFunction<foo>::Pointer CacheableScalarFunctionObj =
    itk::CacheableScalarFunction<foo>::New();
  std::cout << "----------CacheableScalarFunction " << CacheableScalarFunctionObj;

  itk::CompositeValleyFunction<foo>::Pointer CompositeValleyFunctionObj =
    itk::CompositeValleyFunction<foo>::New();
  std::cout << "----------CompositeValleyFunction " << CompositeValleyFunctionObj;

  itk::ConjugateGradientOptimizer<foo>::Pointer ConjugateGradientOptimizerObj =
    itk::ConjugateGradientOptimizer<foo>::New();
  std::cout << "----------ConjugateGradientOptimizer " << ConjugateGradientOptimizerObj;

  itk::CostFunction<foo>::Pointer CostFunctionObj =
    itk::CostFunction<foo>::New();
  std::cout << "----------CostFunction " << CostFunctionObj;

  itk::GradientDescentOptimizer<foo>::Pointer GradientDescentOptimizerObj =
    itk::GradientDescentOptimizer<foo>::New();
  std::cout << "----------GradientDescentOptimizer " << GradientDescentOptimizerObj;

  itk::LBFGSOptimizer<foo>::Pointer LBFGSOptimizerObj =
    itk::LBFGSOptimizer<foo>::New();
  std::cout << "----------LBFGSOptimizer " << LBFGSOptimizerObj;

  itk::LevenbergMarquardtOptimizer<foo>::Pointer LevenbergMarquardtOptimizerObj =
    itk::LevenbergMarquardtOptimizer<foo>::New();
  std::cout << "----------LevenbergMarquardtOptimizer " << LevenbergMarquardtOptimizerObj;

  itk::MultipleValuedNonLinearOptimizer<foo>::Pointer MultipleValuedNonLinearOptimizerObj =
    itk::MultipleValuedNonLinearOptimizer<foo>::New();
  std::cout << "----------MultipleValuedNonLinearOptimizer " << MultipleValuedNonLinearOptimizerObj;

  itk::MultipleValuedNonLinearVnlOptimizer<foo>::Pointer MultipleValuedNonLinearVnlOptimizerObj =
    itk::MultipleValuedNonLinearVnlOptimizer<foo>::New();
  std::cout << "----------MultipleValuedNonLinearVnlOptimizer " << MultipleValuedNonLinearVnlOptimizerObj;

  itk::MultipleValuedVnlCostFunctionAdaptor<foo>::Pointer MultipleValuedVnlCostFunctionAdaptorObj =
    itk::MultipleValuedVnlCostFunctionAdaptor<foo>::New();
  std::cout << "----------MultipleValuedVnlCostFunctionAdaptor " << MultipleValuedVnlCostFunctionAdaptorObj;

  itk::MultivariateLegendrePolynomial<foo>::Pointer MultivariateLegendrePolynomialObj =
    itk::MultivariateLegendrePolynomial<foo>::New();
  std::cout << "----------MultivariateLegendrePolynomial " << MultivariateLegendrePolynomialObj;

  itk::OnePlusOneEvolutionaryOptimizer<foo>::Pointer OnePlusOneEvolutionaryOptimizerObj =
    itk::OnePlusOneEvolutionaryOptimizer<foo>::New();
  std::cout << "----------OnePlusOneEvolutionaryOptimizer " << OnePlusOneEvolutionaryOptimizerObj;

  itk::Optimizer<foo>::Pointer OptimizerObj =
    itk::Optimizer<foo>::New();
  std::cout << "----------Optimizer " << OptimizerObj;

  itk::QuaternionRigidTransformGradientDescentOptimizer<foo>::Pointer QuaternionRigidTransformGradientDescentOptimizerObj =
    itk::QuaternionRigidTransformGradientDescentOptimizer<foo>::New();
  std::cout << "----------QuaternionRigidTransformGradientDescentOptimizer " << QuaternionRigidTransformGradientDescentOptimizerObj;

  itk::RegularStepGradientDescentBaseOptimizer<foo>::Pointer RegularStepGradientDescentBaseOptimizerObj =
    itk::RegularStepGradientDescentBaseOptimizer<foo>::New();
  std::cout << "----------RegularStepGradientDescentBaseOptimizer " << RegularStepGradientDescentBaseOptimizerObj;

  itk::RegularStepGradientDescentOptimizer<foo>::Pointer RegularStepGradientDescentOptimizerObj =
    itk::RegularStepGradientDescentOptimizer<foo>::New();
  std::cout << "----------RegularStepGradientDescentOptimizer " << RegularStepGradientDescentOptimizerObj;

  itk::SingleValuedNonLinearOptimizer<foo>::Pointer SingleValuedNonLinearOptimizerObj =
    itk::SingleValuedNonLinearOptimizer<foo>::New();
  std::cout << "----------SingleValuedNonLinearOptimizer " << SingleValuedNonLinearOptimizerObj;

  itk::SingleValuedNonLinearVnlOptimizer<foo>::Pointer SingleValuedNonLinearVnlOptimizerObj =
    itk::SingleValuedNonLinearVnlOptimizer<foo>::New();
  std::cout << "----------SingleValuedNonLinearVnlOptimizer " << SingleValuedNonLinearVnlOptimizerObj;

  itk::SingleValuedVnlCostFunctionAdaptor<foo>::Pointer SingleValuedVnlCostFunctionAdaptorObj =
    itk::SingleValuedVnlCostFunctionAdaptor<foo>::New();
  std::cout << "----------SingleValuedVnlCostFunctionAdaptor " << SingleValuedVnlCostFunctionAdaptorObj;

  itk::VersorTransformOptimizer<foo>::Pointer VersorTransformOptimizerObj =
    itk::VersorTransformOptimizer<foo>::New();
  std::cout << "----------VersorTransformOptimizer " << VersorTransformOptimizerObj;

#endif

  return 0;
}

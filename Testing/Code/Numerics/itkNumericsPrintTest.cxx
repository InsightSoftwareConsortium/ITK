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
  itk::CacheableScalarFunction::Pointer CacheableScalarFunctionObj =
    itk::CacheableScalarFunction::New();
  std::cout << "----------CacheableScalarFunction " << CacheableScalarFunctionObj;

  itk::CompositeValleyFunction::Pointer CompositeValleyFunctionObj =
    itk::CompositeValleyFunction::New();
  std::cout << "----------CompositeValleyFunction " << CompositeValleyFunctionObj;
#endif
  itk::ConjugateGradientOptimizer::Pointer ConjugateGradientOptimizerObj =
    itk::ConjugateGradientOptimizer::New();
  std::cout << "----------ConjugateGradientOptimizer " << ConjugateGradientOptimizerObj;
#if 0
  itk::CostFunction::Pointer CostFunctionObj =
    itk::CostFunction::New();
  std::cout << "----------CostFunction " << CostFunctionObj;
#endif
  itk::GradientDescentOptimizer::Pointer GradientDescentOptimizerObj =
    itk::GradientDescentOptimizer::New();
  std::cout << "----------GradientDescentOptimizer " << GradientDescentOptimizerObj;

  itk::LBFGSOptimizer::Pointer LBFGSOptimizerObj =
    itk::LBFGSOptimizer::New();
  std::cout << "----------LBFGSOptimizer " << LBFGSOptimizerObj;

  itk::LevenbergMarquardtOptimizer::Pointer LevenbergMarquardtOptimizerObj =
    itk::LevenbergMarquardtOptimizer::New();
  std::cout << "----------LevenbergMarquardtOptimizer " << LevenbergMarquardtOptimizerObj;

  itk::MultipleValuedNonLinearOptimizer::Pointer MultipleValuedNonLinearOptimizerObj =
    itk::MultipleValuedNonLinearOptimizer::New();
  std::cout << "----------MultipleValuedNonLinearOptimizer " << MultipleValuedNonLinearOptimizerObj;
#if 0
  itk::MultipleValuedNonLinearVnlOptimizer::Pointer MultipleValuedNonLinearVnlOptimizerObj =
    itk::MultipleValuedNonLinearVnlOptimizer::New();
  std::cout << "----------MultipleValuedNonLinearVnlOptimizer " << MultipleValuedNonLinearVnlOptimizerObj;

  itk::MultipleValuedVnlCostFunctionAdaptor::Pointer MultipleValuedVnlCostFunctionAdaptorObj =
    itk::MultipleValuedVnlCostFunctionAdaptor::New();
  std::cout << "----------MultipleValuedVnlCostFunctionAdaptor " << MultipleValuedVnlCostFunctionAdaptorObj;

  itk::MultivariateLegendrePolynomial::Pointer MultivariateLegendrePolynomialObj =
    itk::MultivariateLegendrePolynomial::New();
  std::cout << "----------MultivariateLegendrePolynomial " << MultivariateLegendrePolynomialObj;
#endif  
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

  itk::SingleValuedNonLinearOptimizer::Pointer SingleValuedNonLinearOptimizerObj =
    itk::SingleValuedNonLinearOptimizer::New();
  std::cout << "----------SingleValuedNonLinearOptimizer " << SingleValuedNonLinearOptimizerObj;
#if 0
  itk::SingleValuedNonLinearVnlOptimizer::Pointer SingleValuedNonLinearVnlOptimizerObj =
    itk::SingleValuedNonLinearVnlOptimizer::New();
  std::cout << "----------SingleValuedNonLinearVnlOptimizer " << SingleValuedNonLinearVnlOptimizerObj;

  itk::SingleValuedVnlCostFunctionAdaptor::Pointer SingleValuedVnlCostFunctionAdaptorObj =
    itk::SingleValuedVnlCostFunctionAdaptor::New();
  std::cout << "----------SingleValuedVnlCostFunctionAdaptor " << SingleValuedVnlCostFunctionAdaptorObj;
#endif
  itk::VersorTransformOptimizer::Pointer VersorTransformOptimizerObj =
    itk::VersorTransformOptimizer::New();
  std::cout << "----------VersorTransformOptimizer " << VersorTransformOptimizerObj;

  return 0;
}

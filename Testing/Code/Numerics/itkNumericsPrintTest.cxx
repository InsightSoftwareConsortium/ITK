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

  itk::CacheableScalarFunction * CacheableScalarFunctionObj =
    new itk::CacheableScalarFunction;
  std::cout << "----------CacheableScalarFunction " << CacheableScalarFunctionObj;
  delete CacheableScalarFunctionObj;

  itk::ConjugateGradientOptimizer::Pointer ConjugateGradientOptimizerObj =
    itk::ConjugateGradientOptimizer::New();
  std::cout << "----------ConjugateGradientOptimizer " << ConjugateGradientOptimizerObj;

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
  std::cout << "----------MultivariateLegendrePolynomial " << MultivariateLegendrePolynomialObj;
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

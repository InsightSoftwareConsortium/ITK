/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptimizersHierarchyTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/



#include "itkOptimizer.h"
#include "itkNonLinearOptimizer.h"
#include "itkSingleValuedCostFunction.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "itkAmoebaOptimizer.h"
#include "itkConjugateGradientOptimizer.h"
#include "itkLBFGSOptimizer.h"
#include "itkMultipleValuedCostFunction.h"
#include "itkMultipleValuedNonLinearOptimizer.h"
#include "itkVersorTransformOptimizer.h"
#include "itkQuaternionRigidTransformGradientDescentOptimizer.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"

#include "vnl/vnl_math.h"

#include <iostream>



/** 
 *
 *  This file performs only simple C++ tests of
 *  the base classes in the Optimizers hierarchy.
 *
 *  Nothing numerical is computed in these tests,
 *  only code conformance.
 */ 

int itkOptimizersHierarchyTest(int, char* [] )
{
  bool pass = true;


  typedef itk::Optimizer              OptimizerType;
  OptimizerType::Pointer genericOptimizer = OptimizerType::New();


  unsigned int spaceDimension = 10;

  OptimizerType::ParametersType initialPosition(spaceDimension);
  OptimizerType::ParametersType currentPosition(spaceDimension);
  OptimizerType::ScalesType     parameterScale(spaceDimension);

  parameterScale.Fill( 1.5 );
  initialPosition.Fill( 2.0 );

  genericOptimizer->SetInitialPosition( initialPosition );
  genericOptimizer->SetScales( parameterScale );

  OptimizerType::ScalesType parameterScaleGot = 
                                 genericOptimizer->GetScales();
  
  const double tolerance = 1e-10;

  for(unsigned int i=0; i<spaceDimension; i++)
  {
    if( vnl_math_abs( parameterScaleGot[i] - parameterScale[i] ) > tolerance )
      {
      std::cout << "Test failed." << std::endl;
      std::cout << "Scale parameters are damaged after being set." << std::endl;
      std::cout << "Scale was set to: " << parameterScale    << std::endl;
      std::cout << "Scale was got as: " << parameterScaleGot << std::endl;
      return EXIT_FAILURE;
      }
  }


  OptimizerType::ParametersType initialPositionGot = 
                                   genericOptimizer->GetInitialPosition();
  
  for(unsigned int i=0; i<spaceDimension; i++)
  {
    if( vnl_math_abs( initialPositionGot[i] - initialPosition[i] ) > tolerance )
      {
      std::cout << "Test failed." << std::endl;
      std::cout << "InitialPosition parameters are damaged after being set." << std::endl;
      std::cout << "InitialPosition was set to: " << initialPosition    << std::endl;
      std::cout << "InitialPosition was got as: " << initialPositionGot << std::endl;
      return EXIT_FAILURE;
      }
  }


  typedef itk::NonLinearOptimizer     NonLinearOptimizerType;
  NonLinearOptimizerType::Pointer nonLinearOptimizer =
                                    NonLinearOptimizerType::New();




  typedef itk::SingleValuedNonLinearOptimizer 
                                SingleValuedNonLinearOptimizerType;

  SingleValuedNonLinearOptimizerType::Pointer singleValuedOptimizer =
                                SingleValuedNonLinearOptimizerType::New();




  // This cannot be instantiated due to abstract function SetCostFunction()
  typedef itk::SingleValuedNonLinearVnlOptimizer 
                                SingleValuedNonLinearVnlOptimizerType;


  // This is only type checking. This class is not expected to be instantiated
  typedef itk::CostFunction     CostFunctionType;

  // This is only type checking. This class is not expected to be instantiated
  typedef itk::SingleValuedCostFunction     SingleValuedCostFunctionType;


  typedef itk::AmoebaOptimizer    AmoebaOptimizerType;
  AmoebaOptimizerType::Pointer   amoeba = AmoebaOptimizerType::New();

  typedef itk::ConjugateGradientOptimizer    ConjugateGradientOptimizerType;
  ConjugateGradientOptimizerType::Pointer  conjugete 
                                    = ConjugateGradientOptimizerType::New();

  typedef itk::LBFGSOptimizer    LBFGSOptimizerType;
  LBFGSOptimizerType::Pointer   lbfgs = LBFGSOptimizerType::New();


  // Note that a "Versor" is a Unit Quaternion
  typedef itk::VersorTransformOptimizer    VersorOptimizerType;
  VersorOptimizerType::Pointer   versoropt = VersorOptimizerType::New();

  typedef itk::QuaternionRigidTransformGradientDescentOptimizer    QuaternionOptimizerType;
  QuaternionOptimizerType::Pointer   quaternionopt = QuaternionOptimizerType::New();


  typedef itk::OnePlusOneEvolutionaryOptimizer OnePlusOneEvolutionaryOptimizerType;

  OnePlusOneEvolutionaryOptimizerType::Pointer onePlusOne = 
                                          OnePlusOneEvolutionaryOptimizerType::New();


  if ( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}




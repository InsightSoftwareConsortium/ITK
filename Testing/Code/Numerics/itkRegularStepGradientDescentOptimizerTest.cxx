/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <itkRegularStepGradientDescentOptimizer.h>
#include <itkPoint.h>
#include <itkCovariantVector.h>


/** 
 *  The objectif function is the quadratic form:
 *
 *  1/2 x^T A x - b^T x
 *
 *  Where A is a matrix and b is a vector
 *  The system in this example is:
 *
 *     | 3  2 ||x|   | 2|   |0|
 *     | 2  6 ||y| + |-8| = |0|
 *
 *
 *   the solution is the vector | 2 -2 |
 *
 */ 
class CostFunction : public itk::LightObject 
{
public:

  typedef CostFunction Self;
  typedef itk::LightObject  Superclass;
  typedef itk::SmartPointer<Self> Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  itkNewMacro( Self );

  enum { SpaceDimension=2 };
  
  typedef itk::Point<  double, SpaceDimension >          ParametersType;
  
  typedef itk::CovariantVector< double, SpaceDimension > DerivativeType;

  typedef double MeasureType ;


  CostFunction() 
  {
  }

  const ParametersType & GetParameters(void) const 
  { 
    return m_Parameters;
  }

  const MeasureType & GetValue( const ParametersType & parameters ) const
  { 
    
    m_Parameters = parameters;

    double x = m_Parameters[0];
    double y = m_Parameters[1];

    std::cout << "GetValue( " ;
    std::cout << x << " ";
    std::cout << y << ") = ";

    double val = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << val << std::endl; 
    return m_Measure;

  }

  const DerivativeType & GetDerivative( 
             const ParametersType & parameters ) const
  {

    m_Parameters = parameters;

    double x = m_Parameters[0];
    double y = m_Parameters[1];

    std::cout << "GetDerivative( " ;
    std::cout << x << " ";
    std::cout << y << ") = ";

    m_Derivative[0] = 3 * x + 2 * y -2;
    m_Derivative[1] = 2 * x + 6 * y +8;

    std::cout << m_Derivative << std::endl;

    return m_Derivative;
  }

private:

  mutable ParametersType  m_Parameters;
  mutable MeasureType     m_Measure;
  mutable DerivativeType  m_Derivative;

};



int main() 
{
  std::cout << "Conjugate Gradient Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::RegularStepGradientDescentOptimizer< 
                                CostFunction >  OptimizerType;

  
  
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction adaptor
  CostFunction::Pointer costFunction = CostFunction::New();


  itkOptimizer->SetCostFunction( costFunction );

  
  typedef CostFunction::ParametersType    ParametersType;

  typedef OptimizerType::TransformType   TransformType;
  typedef TransformType::ParametersType  TransformParametersType;

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition;
  initialPosition[0] =  100;
  initialPosition[1] = -100;
  
  TransformParametersType parametersScale;
  parametersScale[0] = 1.0;
  parametersScale[1] = 1.0;

  itkOptimizer->MinimizeOn();
  itkOptimizer->GetTransform()->SetScale( parametersScale );
  itkOptimizer->SetGradientMagnitudeTolerance( 1e-6 );
  itkOptimizer->SetMaximumStepLength( 30.0 );
  itkOptimizer->SetMinimumStepLength( 1e-6 );
  itkOptimizer->SetNumberOfIterations( 900 );

  itkOptimizer->SetInitialPosition( initialPosition );
  itkOptimizer->StartOptimization();

  ParametersType finalPosition;
  finalPosition = costFunction->GetParameters();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << "," ;
  std::cout << finalPosition[1] << ")" << std::endl;  

  //
  // check results to see if it is within range
  //
  bool pass = true;
  double trueParameters[2] = { 2, -2 };
  for( unsigned int j = 0; j < 2; j++ )
    {
    if( vnl_math_abs( finalPosition[j] - trueParameters[j] ) > 0.01 )
      pass = false;
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}




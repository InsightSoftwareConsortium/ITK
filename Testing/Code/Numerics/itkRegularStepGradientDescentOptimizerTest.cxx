/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <itkGradientDescentOptimizer.h>
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

  typedef  itk::GradientDescentOptimizer< 
                                CostFunction >  OptimizerType;

  
  
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction adaptor
  CostFunction costFunction;


  itkOptimizer->SetCostFunction( &costFunction );

  
   typedef CostFunction::ParametersType    ParametersType;

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition;
  initialPosition[0] =  100;
  initialPosition[1] = -100;
  
  ParametersType  parametersScale;
  parametersScale[0] = 1.0;
  parametersScale[1] = 1.0;

  itkOptimizer->SetMinimize();
  itkOptimizer->SetScale( parametersScale );
  itkOptimizer->SetGradientMagnitudeTolerance( 1e-6 );
  itkOptimizer->SetMaximumStepLength( 30.0 );
  itkOptimizer->SetMinimumStepLength( 1e-6 );
  itkOptimizer->SetMaximumNumberOfIterations( 900 );

  itkOptimizer->SetInitialPosition( initialPosition );
  itkOptimizer->StartOptimization();

  ParametersType finalPosition;
  finalPosition = costFunction.GetParameters();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << "," ;
  std::cout << finalPosition[1] << ")" << std::endl;  

  return 0;

}




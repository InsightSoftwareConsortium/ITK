/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientDescentOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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

    m_Measure = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << m_Measure << std::endl; 

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
 
  void GetValueAndDerivative( const ParametersType & parameters,
    MeasureType& value, DerivativeType& deriv ) const
  {
    value = this->GetValue( parameters );
    deriv = this->GetDerivative( parameters );
  }

private:

  mutable ParametersType  m_Parameters;
  mutable MeasureType     m_Measure;
  mutable DerivativeType  m_Derivative;

};



int main() 
{
  std::cout << "Gradient Descent Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::GradientDescentOptimizer< 
                                CostFunction >  OptimizerType;

  typedef OptimizerType::TransformType   TransformType;
  typedef TransformType::ParametersType  TransformParametersType;


    
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction adaptor
  CostFunction::Pointer costFunction = CostFunction::New();


  itkOptimizer->SetCostFunction( costFunction );

  
   typedef CostFunction::ParametersType    ParametersType;

  // We start not so far from  | 2 -2 |
  ParametersType  initialPosition;
  initialPosition[0] =  100;
  initialPosition[1] = -100;
  
  TransformParametersType parametersScale;
  parametersScale[0] = 1.0;
  parametersScale[1] = 1.0;

  itkOptimizer->MinimizeOn();
  itkOptimizer->GetTransform()->SetScale( parametersScale );
  itkOptimizer->SetLearningRate( 0.1 );
  itkOptimizer->SetNumberOfIterations( 50 );

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

  // Exercise various member functions.
  std::cout << "Maximize: " << itkOptimizer->GetMaximize() << std::endl;
  std::cout << "LearningRate: " << itkOptimizer->GetLearningRate();
  std::cout << std::endl;
  std::cout << "NumberOfIterations: " << itkOptimizer->GetNumberOfIterations();
  std::cout << std::endl;

  itkOptimizer->Print( std::cout );

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientDescentOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <itkGradientDescentOptimizer.h>
#include <itkVectorContainer.h>


/** 
 *  The objectif function is the quadratic form:
 *
 *  1/2 x^T A x - b^T x
 *
 *  Where A is represented as an itkMatrix and 
 *  b is represented as a itkVector
 *
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
  typedef itk::VectorContainer<unsigned int,double> ParametersType;
  typedef itk::VectorContainer<unsigned int,double> DerivativeType;
  typedef double MeasureType ;


  CostFunction() 
  {
    m_Parameters = ParametersType::New(); 
    m_Parameters->Reserve(2);
  }

  const ParametersType::Pointer & GetParameters(void) const 
  { 
    return m_Parameters;
  }

  double GetValue( void ) 
  { 
    std::cout << "GetValue ( " ;

    ParametersType::ConstIterator it = m_Parameters->Begin();
    
    double x = it.Value();
    it++;
    double y = it.Value();

    it = m_Parameters->Begin();
    while ( it != m_Parameters->End() )
    { 
      std::cout << it.Value() << " ";
      it++;
    }
    std::cout << ") = ";

    double val = 0.5*(3*x*x+4*x*y+6*y*y) - 2*x + 8*y;

    std::cout << val << std::endl; 
    return val;
  }

  DerivativeType::Pointer GetDerivative( void ) 
  {
    std::cout << "GetDerivative ( " ;
    ParametersType::ConstIterator it;

    it = m_Parameters->Begin();
    double x = it.Value();
    it++;
    double y = it.Value();
    it = m_Parameters->Begin();
    while ( it != m_Parameters->End() )
    { 
      std::cout << it.Value() << " ";
      it++;
    }
    std::cout << ") = ";

    DerivativeType::Pointer grad = DerivativeType::New();
    grad->Reserve(2);
    DerivativeType::Iterator dit;
    std::cout << "(" ; 
    dit = grad->Begin();
    dit.Value() = 3*x + 2*y -2;
    std::cout << dit.Value() <<" ";
    dit++;
    dit.Value() = 2*x + 6*y +8;
    std::cout << dit.Value() << ")" << std::endl;
    return grad;
  }

private:

  ParametersType::Pointer   m_Parameters;

};



int main() 
{
  std::cout << "Conjugate Gradient Optimizer Test \n \n";

  typedef  itk::GradientDescentOptimizer< 
                                CostFunction >  OptimizerType;

  
  
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction adaptor
  CostFunction costFunction;


  itkOptimizer->SetCostFunction( &costFunction );

  
  const double F_Tolerance      = 1e-3;  // Function value tolerance
  const double G_Tolerance      = 1e-4;  // Gradient magnitude tolerance 
  const double X_Tolerance      = 1e-8;  // Search space tolerance
  const double Epsilon_Function = 1e-10; // Step
  const int    Max_Iterations   =   100; // Maximum number of iterations


  itkOptimizer->SetMaximumNumberOfIterations( Max_Iterations );

  typedef itk::VectorContainer<unsigned int,double> ParametersType ;
  typedef ParametersType::Pointer ParametersPointer;
  ParametersPointer initialValue = ParametersType::New();
  initialValue->Reserve(2);

  ParametersType::Iterator it;
  // We start not so far from  | 2 -2 |
  it = initialValue->Begin();
  it.Value() = 100;
  it++;     
  it.Value() = -100;

  itkOptimizer->StartOptimization( initialValue );


  it = costFunction.GetParameters()->Begin();
  std::cout << "Solution        = (" << it.Value() << "," ;
  it++;
  std::cout << it.Value() << ")" << std::endl;  

  return 0;

}




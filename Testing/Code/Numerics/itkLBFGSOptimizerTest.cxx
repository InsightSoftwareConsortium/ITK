/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLBFGSOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/



#include <itkLBFGSOptimizer.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>



typedef vnl_matrix<double> MatrixType;
typedef vnl_vector<double> VectorType;



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
class CostFunction {
public:
  enum { SpaceDimension=2 };
  CostFunction():m_A(2,2),m_b(2) {
    
    m_A[0][0] =  3;
    m_A[0][1] =  2;
    m_A[1][0] =  2;
    m_A[1][1] =  6;

    m_b[0]    =  2;
    m_b[1]    = -8;

  }
  double GetValue( const VectorType & v ) 
  {
    std::cout << "GetValue( " << v << " ) = ";
    VectorType Av = m_A * v;
    double val = ( inner_product<double>( Av , v ) )/2.0;
    val -= inner_product< double >( m_b , v );
    std::cout << val << std::endl;
    return val;
  }
  VectorType GetDerivative( const VectorType & v ) 
  {
    std::cout << "GetDerivative( " << v << " ) = ";
    VectorType grad = m_A * v  - m_b;
    std::cout << grad << std::endl;
    return grad;
  }

private:
  MatrixType        m_A;
  VectorType        m_b;
};



int main() 
{


  typedef  itk::LBFGSOptimizer< 
                                CostFunction >  OptimizerType;

  typedef  OptimizerType::InternalOptimizerType  vnlOptimizerType;

  
  
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


  vnlOptimizerType & vnlOptimizer = itkOptimizer->GetOptimizer();

  vnlOptimizer.set_f_tolerance( F_Tolerance );
  vnlOptimizer.set_g_tolerance( G_Tolerance );
  vnlOptimizer.set_x_tolerance( X_Tolerance ); 
  vnlOptimizer.set_epsilon_function( Epsilon_Function );
  vnlOptimizer.set_max_function_evals( Max_Iterations );

  vnlOptimizer.set_trace( true );   // activate print out per iteration
  vnlOptimizer.set_verbose( true ); // activate verbose mode

  vnlOptimizer.set_check_derivatives( 3 );
      
  VectorType initialValue(2);       // constructor requires vector size

  initialValue[0] =  100;             // We start not so far from  | 2 -2 |
  initialValue[1] = -100;


  itkOptimizer->StartOptimization( initialValue );

  std::cout << "End condition   = " << vnlOptimizer.get_failure_code() << std::endl;
  std::cout << "Number of iters = " << vnlOptimizer.get_num_iterations() << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer.get_num_evaluations() << std::endl;    
  std::cout << std::endl;
  std::cout << "Solution        = " << initialValue << std::endl;    

  return 0;

}




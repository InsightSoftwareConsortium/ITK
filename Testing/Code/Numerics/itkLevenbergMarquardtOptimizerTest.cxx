/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevenbergMarquardtOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <itkLevenbergMarquardtOptimizer.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <itkPoint.h>


typedef vnl_matrix<double> MatrixType;
typedef vnl_vector<double> VectorType;



/** 
 *
 *   This example solves the equation:
 *
 *     (a-3) x^2  + (b-2) y^2
 *
 *   for the (a,b) parameters
 *
 *   the solution is the vector | 3 2 |
 *
 *   (x,y) values are sampled over a rectangular domain
 *   whose size is defined by XRange and YRange
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

  enum { XRange = 1,
         YRange = 1 };   // size of the domain to sample the cost function
         
  enum { SpaceDimension =  2 };
  enum { RangeDimension =  ( 2*XRange+1 ) * ( 2*YRange+1 ) };

  typedef itk::Point<double,SpaceDimension>    ParametersType;
  typedef VectorType                           MeasureType;
  typedef MatrixType                           DerivativeType;

  CostFunction() 
  {
    m_Measure.resize(RangeDimension);
    m_Derivative.resize(RangeDimension,SpaceDimension);
  }

  const ParametersType  & GetParameters(void) const 
  { 
    return m_Parameters;
  }

  const MeasureType & GetValue( const ParametersType & parameters ) 
  {

    m_Parameters = parameters;
    
    std::cout << "GetValue( ";
    double a = m_Parameters[0];
    double b = m_Parameters[1];

    std::cout << a << " , ";
    std::cout << b << ") = ";

    // Compute points of the function over a square domain
    unsigned valueindex = 0;
    for( int y = -YRange; y<=YRange; y++ ) 
    {
      const double yp = y*y*b;
      for( int x = -XRange; x<=XRange; x++ ) 
      {
        m_Measure[valueindex] = a * x*x + yp - (3.0 * x*x + 2 * y*y );
        std::cout << m_Measure[valueindex] << "  ";
        valueindex++;
      }
    }

    std::cout << std::endl;

    return m_Measure; 
 }

  const DerivativeType &  GetDerivative( 
                 const ParametersType & parameters ) const
  {
   
    return m_Derivative;
  }

private:

  mutable ParametersType    m_Parameters;
  mutable MeasureType       m_Measure;
  mutable DerivativeType    m_Derivative;

};



int main() 
{
  std::cout << "Levenberg Marquardt optimizer test \n \n"; 
  
  typedef  itk::LevenbergMarquardtOptimizer< \
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

  vnlOptimizer.set_check_derivatives( 3 );
    
  // We start not so far from  | 3 2 |
  typedef CostFunction::ParametersType ParametersType;
  ParametersType initialValue;
  initialValue = 20,10;

  itkOptimizer->SetInitialPosition( initialValue );

  itkOptimizer->StartOptimization();

  std::cout << "End condition   = " << vnlOptimizer.get_failure_code() << std::endl;
  std::cout << "Number of iters = " << vnlOptimizer.get_num_iterations() << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer.get_num_evaluations() << std::endl;    
  std::cout << std::endl;

  ParametersType finalPosition;
  finalPosition = costFunction.GetParameters();
  std::cout << "Solution        = (";
  std::cout << finalPosition[0] << "," ;
  std::cout << finalPosition[1] << ")" << std::endl;  

  return 0;

}




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
#include <itkVectorContainer.h>


typedef vnl_matrix<double> MatrixType;
typedef vnl_vector<double> VectorType;



/** 
 *   TODO: Comment this example with the initial equation we have to solve
 *
 *   the solution is the vector | 3 2 |
 *
 */ 
class CostFunction : public itk::LightObject{
public:
  typedef CostFunction Self;
  typedef itk::LightObject  Superclass;
  typedef itk::SmartPointer<Self> Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  itkNewMacro( Self );

  enum { SpaceDimension =  2 };
  enum { RangeDimension =  9 };
  typedef itk::VectorContainer<unsigned int,double> ParametersType;
  typedef itk::VectorContainer<unsigned int,double> DerivativeType;
  typedef itk::VectorContainer<unsigned int,double> VectorMeasureType;

  CostFunction() {
    m_Parameters = ParametersType::New(); 
    m_Parameters->Reserve(2);
  }

  const ParametersType::Pointer & GetParameters(void) const 
  { 
    return m_Parameters;
  }

  void GetValue( VectorMeasureType::Pointer & values ) 
  {
    std::cout << "GetValue( ";
    ParametersType::Iterator it = m_Parameters->Begin();
    
    double a = it.Value();
    it++;
    double b = it.Value();

    it = m_Parameters->Begin();
    while ( it != m_Parameters->End() )
    { 
      std::cout << it.Value() << " ";
      it++;
    }
    std::cout << ") = ";

    it = values->Begin();
    for(int y = -1; y<=1; y++) 
    {
      const double yp = y*y*b;
      for(int x = -1; x<=1; x++) 
      {
        it.Value() = a * x*x + yp - (3.0 * x*x + 2 * y*y );
        std::cout << it.Value() << " ";
        it++;
      }
    }

    std::cout << std::endl;
 
 }

DerivativeType::Pointer GetDerivative(void) 
  {
   
 }

private:

  ParametersType::Pointer   m_Parameters;

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

  vnlOptimizer.set_trace( true );   // activate print out per iteration
  vnlOptimizer.set_verbose( true ); // activate verbose mode

  vnlOptimizer.set_check_derivatives( 3 );
    
  typedef itk::VectorContainer<unsigned int,double> ParametersType ;
  typedef ParametersType::Pointer ParametersPointer;
  ParametersPointer initialValue = ParametersType::New();
  initialValue->Reserve(2);

  ParametersType::Iterator it;
  // We start not so far from  | 3 2 |
  it = initialValue->Begin();
  it.Value() = 2;
  it++;     
  it.Value() = 1;

  itkOptimizer->StartOptimization( initialValue );

  std::cout << "End condition   = " << vnlOptimizer.get_failure_code() << std::endl;
  std::cout << "Number of iters = " << vnlOptimizer.get_num_iterations() << std::endl;
  std::cout << "Number of evals = " << vnlOptimizer.get_num_evaluations() << std::endl;    
  std::cout << std::endl;

  it = costFunction.GetParameters()->Begin();
  std::cout << "Solution        = (" << it.Value() << "," ;
  it++;
  std::cout << it.Value() << ")" << std::endl;  

  return 0;

}




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/



#include <itkAmoebaOptimizer.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_math.h>
#include <iostream>



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
class CostFunction : public itk::Object {
public:

  typedef CostFunction Self;
  typedef itk::LightObject  Superclass;
  typedef itk::SmartPointer<Self> Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  itkNewMacro( Self );

  enum { SpaceDimension=2 };

  typedef vnl_vector_fixed<double,SpaceDimension> ParametersType;
  typedef vnl_vector_fixed<double,SpaceDimension> DerivativeType;
  typedef vnl_vector<double>                      VectorType;
  typedef vnl_matrix<double>                      MatrixType;

  typedef double MeasureType ;


  CostFunction():m_A(SpaceDimension,SpaceDimension),m_b(SpaceDimension) {
    
    m_A[0][0] =  3;
    m_A[0][1] =  2;
    m_A[1][0] =  2;
    m_A[1][1] =  6;

    m_b[0]    =  2;
    m_b[1]    = -8;

  }
  double GetValue( const ParametersType & parameters ) const
  {
    m_Parameters = parameters;
    std::cout << "GetValue( " << m_Parameters << " ) = ";
    VectorType Av = m_A * m_Parameters;
    double val = ( inner_product<double>( Av , m_Parameters ) )/2.0;
    val -= inner_product< double >( m_b , m_Parameters );
    std::cout << val << std::endl;
    return val;
  }
  const DerivativeType & GetDerivative( const ParametersType & v ) const
  {
    std::cout << "GetDerivative( " << v << " ) = ";
    m_Parameters = v;
    m_Gradient = m_A * m_Parameters  - m_b;
    std::cout << m_Gradient << std::endl;
    return m_Gradient;
  }
  
  const ParametersType & GetParameters(void) const 
  { 
    return m_Parameters;
  }


private:
  MatrixType        m_A;
  VectorType        m_b;

  mutable DerivativeType    m_Gradient;
  mutable ParametersType    m_Parameters;

};



int main() 
{


  typedef  itk::AmoebaOptimizer< 
                                CostFunction >  OptimizerType;

  typedef  OptimizerType::InternalOptimizerType  vnlOptimizerType;

  
  
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction adaptor
  CostFunction::Pointer costFunction = CostFunction::New();


  itkOptimizer->SetCostFunction( costFunction );


  vnlOptimizerType & vnlOptimizer = itkOptimizer->GetOptimizer();


  OptimizerType::ParametersType initialValue(2);       // constructor requires vector size

  initialValue[0] =  100;             // We start not so far from  | 2 -2 |
  initialValue[1] = -100;

  OptimizerType::ParametersType currentValue(2);

  currentValue = initialValue;

  itkOptimizer->SetInitialPosition( currentValue );
  itkOptimizer->StartOptimization();

  std::cout << "Number of evals = " << vnlOptimizer.get_num_evaluations() << std::endl;    

  //
  // check results to see if it is within range
  //

  OptimizerType::ParametersType finalPosition;
  finalPosition = costFunction->GetParameters();

  double trueParameters[2] = { 2, -2 };
  bool pass = true;

  std::cout << "Right answer   = " << trueParameters[0] << " , " << trueParameters[1] << std::endl; 
  std::cout << "Final position = " << finalPosition     << std::endl;
  
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




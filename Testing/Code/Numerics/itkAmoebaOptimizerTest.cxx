/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
class amoebaCostFunction : public itk::SingleValuedCostFunction 
{
public:

  typedef amoebaCostFunction                    Self;
  typedef itk::SingleValuedCostFunction     Superclass;
  typedef itk::SmartPointer<Self>           Pointer;
  typedef itk::SmartPointer<const Self>     ConstPointer;
  itkNewMacro( Self );
  itkTypeMacro( amoebaCostFunction, SingleValuedCostFunction );

  enum { SpaceDimension=2 };

  typedef Superclass::ParametersType              ParametersType;
  typedef Superclass::DerivativeType              DerivativeType;
  typedef Superclass::MeasureType                 MeasureType;

  typedef vnl_vector<double>                      VectorType;
  typedef vnl_matrix<double>                      MatrixType;


  amoebaCostFunction():m_A(SpaceDimension,SpaceDimension),m_b(SpaceDimension) 
   {
    m_A[0][0] =  3;
    m_A[0][1] =  2;
    m_A[1][0] =  2;
    m_A[1][1] =  6;

    m_b[0]    =  2;
    m_b[1]    = -8;
    }

  double GetValue( const ParametersType & parameters ) const 
    {

    VectorType v( parameters.Size() );
    for(unsigned int i=0; i<SpaceDimension; i++)
      {
      v[i] = parameters[i];
      }
    std::cout << "GetValue( " << v << " ) = ";
    VectorType Av = m_A * v;
    double val = ( inner_product<double>( Av , v ) )/2.0;
    val -= inner_product< double >( m_b , v );
    std::cout << val << std::endl;
    return val;
    }

  void GetDerivative( const ParametersType & parameters,
                            DerivativeType & derivative ) const
    {

    VectorType v( parameters.Size() );
    for(unsigned int i=0; i<SpaceDimension; i++)
      {
      v[i] = parameters[i];
      }
    std::cout << "GetDerivative( " << v << " ) = ";
    VectorType gradient = m_A * v  - m_b;
    std::cout << gradient << std::endl;
    derivative = DerivativeType(SpaceDimension);
    for(unsigned int i=0; i<SpaceDimension; i++)
      {
      derivative[i] = gradient[i];
      }
    }
  
  unsigned int GetNumberOfParameters(void) const
    {
    return SpaceDimension;
    }

private:
  MatrixType        m_A;
  VectorType        m_b;

};



int itkAmoebaOptimizerTest(int, char* [] ) 
{

  std::cout << "Amoeba Optimizer Test \n \n";

  typedef  itk::AmoebaOptimizer  OptimizerType;

  typedef  OptimizerType::InternalOptimizerType  vnlOptimizerType;

  
  
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction adaptor
  amoebaCostFunction::Pointer costFunction = amoebaCostFunction::New();


  itkOptimizer->SetCostFunction( costFunction.GetPointer() );


  vnlOptimizerType * vnlOptimizer = itkOptimizer->GetOptimizer();


  OptimizerType::ParametersType initialValue(2);       // constructor requires vector size

  initialValue[0] =  100;             // We start not so far from  | 2 -2 |
  initialValue[1] = -100;

  OptimizerType::ParametersType currentValue(2);

  currentValue = initialValue;

  itkOptimizer->SetInitialPosition( currentValue );

  try 
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error ocurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Number of evals = " << vnlOptimizer->get_num_evaluations() << std::endl;    

  std::cout << "Optimizer: " << itkOptimizer;

  //
  // check results to see if it is within range
  //

  OptimizerType::ParametersType finalPosition;
  finalPosition = itkOptimizer->GetCurrentPosition();

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




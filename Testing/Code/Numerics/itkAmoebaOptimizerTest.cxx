/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTransformOptimizerTest.cxx
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

#include <itkVersorTransformOptimizer.h>
#include <itkVector.h>
#include <itkVersor.h>
#include <itkCovariantVector.h>
#include <itkVersorTransform.h>


/** 
 *  The objectif function is the scalar product:
 *
 *  f( V ) = < A, V(B) >
 * 
 *  where:
 *
 *    V  is a Versor representing a rotation
 *    A  is a vector 
 *    B  is another vector
 *
 *  the vector A = [ 0 0 1 ]
 *  the vector B = [ 0 1 0 ]
 * 
 *  the Versor solution should be: V = [ k1 0 0 k2 ]
 *
 *        k1 = sin( 45 degrees )
 *        k2 = cos( 45 degrees )
 *
 */ 
class CostFunction : public itk::LightObject 
{
public:

  typedef CostFunction Self;
  typedef itk::LightObject  Superclass;
  typedef itk::SmartPointer<Self> Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;
  
  typedef itk::VersorTransform<double>    TransformType;
    
  itkNewMacro( Self );

  enum { SpaceDimension = 3 };
  
  typedef itk::Versor< double >                    ParametersType;
  typedef ParametersType::VectorType               AxisType;

  typedef itk::CovariantVector< double, 
                                SpaceDimension >   DerivativeType;
  typedef itk::Vector< double,  SpaceDimension >   VectorType;

  typedef double MeasureType;


  CostFunction() 
  {
    m_Transform = TransformType::New();
  }

  const ParametersType & GetParameters(void) const 
    { 
    return m_Parameters;
    }

  const MeasureType & GetValue( const ParametersType & parameters ) const
  { 
    

    VectorType A;
    VectorType B;

    A[0] = 0;
    A[1] = 0;
    A[2] = 1;

    B[0] = 0;
    B[1] = 1;
    B[2] = 0;

    m_Parameters = parameters;


    m_Transform->SetRotation( m_Parameters );

    const VectorType C = m_Transform->TransformVector( B );

    m_Measure = A * C;

    return m_Measure;

  }

  const DerivativeType & GetDerivative( 
                     const ParametersType & parameters ) const
  {

    const MeasureType baseValue =  this->GetValue( parameters );

    ParametersType versorX;
    ParametersType versorY;
    ParametersType versorZ;

    const double deltaAngle = 0.00175; // in radians = about 0.1 degree

    versorX.SetRotationAroundX( deltaAngle );
    versorY.SetRotationAroundY( deltaAngle );
    versorZ.SetRotationAroundZ( deltaAngle );

    const MeasureType turnXValue = this->GetValue( parameters * versorX );
    const MeasureType turnYValue = this->GetValue( parameters * versorY );
    const MeasureType turnZValue = this->GetValue( parameters * versorZ );

    m_Derivative[0] = ( turnXValue - baseValue ) / deltaAngle;
    m_Derivative[1] = ( turnYValue - baseValue ) / deltaAngle;
    m_Derivative[2] = ( turnZValue - baseValue ) / deltaAngle;

    return m_Derivative;
  }

private:

  mutable   TransformType::Pointer  m_Transform;

  mutable   ParametersType          m_Parameters;
  mutable   MeasureType             m_Measure;
  mutable   DerivativeType          m_Derivative;

};



int main() 
{
  std::cout << "VersorTransform Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::VersorTransformOptimizer< CostFunction >  OptimizerType;

  
  
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction adaptor
  CostFunction::Pointer costFunction = CostFunction::New();


  itkOptimizer->SetCostFunction( costFunction );

  
  typedef CostFunction::ParametersType    ParametersType;

  typedef OptimizerType::TransformType   TransformType;
  typedef TransformType::ParametersType  TransformParametersType;

  // We start with a null rotation
  ParametersType  initialRotation;
  ParametersType::VectorType axis;

  axis = 1.0f, 0.0f, 0.0f;

  ParametersType::ValueType angle = 0.0f;

  initialRotation.Set( axis, angle );
  
  itkOptimizer->MaximizeOn();
  itkOptimizer->SetGradientMagnitudeTolerance( 1e-15 );
  itkOptimizer->SetMaximumStepLength( 0.1745 ); // About 10 deegres
  itkOptimizer->SetMinimumStepLength( 1e-9 );
  itkOptimizer->SetNumberOfIterations( 300 );

  itkOptimizer->SetInitialPosition( initialRotation );
  itkOptimizer->StartOptimization();

  ParametersType finalPosition;
  finalPosition = costFunction->GetParameters();
  std::cout << "Solution        = (" << finalPosition << ")" << std::endl;  

  //
  // check results to see if it is within range
  //
  bool pass = true;

  // True versor
  ParametersType trueParameters;
  ParametersType::VectorType trueAxis;
  ParametersType::ValueType  trueAngle;
  trueAxis  = 1.0f, 0.0f, 0.0f;
  trueAngle = 2.0 * atan( 1.0f );

  trueParameters.Set( trueAxis, trueAngle );
    
  std::cout << "True Parameters = " << trueParameters << std::endl;
  for( unsigned int j = 0; j < 2; j++ )
    {
    ParametersType ratio = finalPosition * trueParameters.GetReciprocal();
    const ParametersType::ValueType cosHalfAngle = ratio.GetW();
    const ParametersType::ValueType cosHalfAngleSquare = 
                                            cosHalfAngle * cosHalfAngle;
    if( cosHalfAngleSquare < 0.95 )
      {
      pass = false;
      break;
      }
    }

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTransformOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
  
  typedef itk::Array< double >                     ParametersType;
  typedef itk::Versor< double >                    VersorType;
  typedef VersorType::VectorType                   AxisType;

  typedef itk::Array< double >                     DerivativeType;
  typedef itk::Vector< double,  SpaceDimension >   VectorType;

  typedef double MeasureType;


  CostFunction():
        m_Parameters(SpaceDimension),
        m_Derivative(SpaceDimension) 
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

    VectorType rightPart;
    for(unsigned int i=0; i<3; i++)
      {
      rightPart[i] = m_Parameters[i];
      }

    VersorType versor;
    versor.Set( rightPart );

    m_Transform->SetRotation( versor );

    const VectorType C = m_Transform->TransformVector( B );

    m_Measure = A * C;

    return m_Measure;

  }

  const DerivativeType & GetDerivative( 
                     const ParametersType & parameters ) const
  {

    VectorType rightPart;
    for(unsigned int i=0; i<3; i++)
      {
      rightPart[i] = m_Parameters[i];
      }

    VersorType currentVersor;
    currentVersor.Set( rightPart );


    const MeasureType baseValue =  this->GetValue( parameters );

    VersorType versorX;
    VersorType versorY;
    VersorType versorZ;

    const double deltaAngle = 0.00175; // in radians = about 0.1 degree

    versorX.SetRotationAroundX( deltaAngle );
    versorY.SetRotationAroundY( deltaAngle );
    versorZ.SetRotationAroundZ( deltaAngle );

    VersorType plusdDeltaX = currentVersor * versorX;
    VersorType plusdDeltaY = currentVersor * versorY;
    VersorType plusdDeltaZ = currentVersor * versorZ;

    ParametersType parametersPlustDeltaX(SpaceDimension);
    ParametersType parametersPlustDeltaY(SpaceDimension);
    ParametersType parametersPlustDeltaZ(SpaceDimension);
    
    parametersPlustDeltaX[0] = plusdDeltaX.GetX();
    parametersPlustDeltaX[1] = plusdDeltaX.GetY();
    parametersPlustDeltaX[2] = plusdDeltaX.GetZ();

    parametersPlustDeltaY[0] = plusdDeltaY.GetX();
    parametersPlustDeltaY[1] = plusdDeltaY.GetY();
    parametersPlustDeltaY[2] = plusdDeltaY.GetZ();

    parametersPlustDeltaZ[0] = plusdDeltaZ.GetX();
    parametersPlustDeltaZ[1] = plusdDeltaZ.GetY();
    parametersPlustDeltaZ[2] = plusdDeltaZ.GetZ();

    const MeasureType turnXValue = this->GetValue( parametersPlustDeltaX );
    const MeasureType turnYValue = this->GetValue( parametersPlustDeltaY );
    const MeasureType turnZValue = this->GetValue( parametersPlustDeltaZ );

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
  typedef OptimizerType::VersorType      VersorType;

  // We start with a null rotation
  VersorType::VectorType axis;
  axis[0] =  1.0f;
  axis[1] =  0.0f;
  axis[2] =  0.0f;

  VersorType::ValueType angle = 0.0f;

  VersorType initialRotation;
  initialRotation.Set( axis, angle );
  
  ParametersType  initialPosition(CostFunction::SpaceDimension);
  initialPosition[0] = initialRotation.GetX();
  initialPosition[1] = initialRotation.GetY();
  initialPosition[2] = initialRotation.GetZ();

  itkOptimizer->MaximizeOn();
  itkOptimizer->SetGradientMagnitudeTolerance( 1e-15 );
  itkOptimizer->SetMaximumStepLength( 0.1745 ); // About 10 deegres
  itkOptimizer->SetMinimumStepLength( 1e-9 );
  itkOptimizer->SetNumberOfIterations( 300 );

  itkOptimizer->SetInitialPosition( initialPosition );
  itkOptimizer->StartOptimization();

  ParametersType finalPosition(CostFunction::SpaceDimension);
  finalPosition = costFunction->GetParameters();

  VersorType finalRotation;
  VersorType::VectorType finalRightPart;
  for(unsigned int i=0; i<CostFunction::SpaceDimension; i++)
    {
    finalRightPart[i] = finalPosition[i];
    }
  finalRotation.Set( finalRightPart );
  std::cout << "Solution        = (" << finalRotation << ")" << std::endl;  

  //
  // check results to see if it is within range
  //
  bool pass = true;

  // True versor

  VersorType::VectorType trueAxis;
  VersorType::ValueType  trueAngle;
  trueAxis  = 1.0f, 0.0f, 0.0f;
  trueAngle = 2.0 * atan( 1.0f );
  VersorType trueRotation;
  trueRotation.Set( trueAxis, trueAngle );
    
  ParametersType trueParameters(CostFunction::SpaceDimension);
  trueParameters[0] = trueRotation.GetX();
  trueParameters[1] = trueRotation.GetY();
  trueParameters[2] = trueRotation.GetZ();
  
  std::cout << "True Parameters = " << trueParameters << std::endl;

  VersorType ratio = finalRotation * trueRotation.GetReciprocal();
  const VersorType::ValueType cosHalfAngle = ratio.GetW();
  const VersorType::ValueType cosHalfAngleSquare = 
                                          cosHalfAngle * cosHalfAngle;
  if( cosHalfAngleSquare < 0.95 )
    {
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




/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorRigid3DTransformOptimizerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <itkVersorRigid3DTransformOptimizer.h>
#include <itkVector.h>
#include <itkVersor.h>
#include <itkCovariantVector.h>
#include <itkVersorRigid3DTransform.h>


/** The objectif function is the sum of squared distances between two pairs of points.
 *
 *  f( T ) =  |T(P1) - P|^2 + |T(Q1) - Q|^2   
 * 
 *  where:
 *
 *    P1, Q1 are two arbitrary points
 *    P = R( P1 ) => P is P1 transformed by R
 *    Q = R( Q1 ) => Q is Q1 transformed by R
 *
 *    T  is the VersorRigid transform being sought
 *       and corresponds to T = R
 *
 *  We arbitrarily choose 
 *       P1 = [ 0  0 10 ]
 *       Q1 = [ 0 10  0 ]
 *
 *       R = ( sin(10/2),0,0 ), ( 0, 30, 30 )
 *
 *  So P, and Q are 
 *
 *       P = 
 *       Q =
 *
 *
 */ 
class versorCostFunction : public itk::SingleValuedCostFunction 
{
public:

  typedef versorCostFunction                  Self;
  typedef itk::SingleValuedCostFunction       Superclass;
  typedef itk::SmartPointer<Self>             Pointer;
  typedef itk::SmartPointer<const Self>       ConstPointer;
  
  typedef itk::VersorRigid3DTransform<double>        TransformType;
    
  itkNewMacro( Self );
  itkTypeMacro( versorCostFunction, SingleValuedCostFunction );

  itkStaticConstMacro( SpaceDimension, unsigned int, 6 );
  
  typedef Superclass::ParametersType              ParametersType;
  typedef Superclass::DerivativeType              DerivativeType;

  typedef itk::Versor< double >                   VersorType;
  typedef VersorType::VectorType                  AxisType;
  typedef itk::Vector< double,  3 >               VectorType;
  typedef itk::Point<  double,  3 >               PointType;

  typedef double MeasureType;


  versorCostFunction()
  {
    m_Transform = TransformType::New();

    m_P1[0] =  0.0;
    m_P1[1] =  0.0;
    m_P1[2] = 10.0;

    m_Q1[0] =  0.0;
    m_Q1[1] = 10.0;
    m_Q1[2] =  0.0;

    VersorType versor;
    versor.SetRotationAroundX( 10.0 * atan( 1.0 ) / 45.0 );

    m_Transform->SetRotation( versor );
 
    TransformType::OutputVectorType translation;
    translation[0] =  0.0;
    translation[1] = 30.0;
    translation[2] = 30.0;

    m_Transform->SetTranslation( translation );
  
    m_P = m_Transform->TransformPoint( m_P1 );
    m_Q = m_Transform->TransformPoint( m_Q1 );
  }


  MeasureType GetValue( const ParametersType & parameters ) const
  { 
    
    std::cout << "GetValue( " << parameters << " ) = ";

    TransformType::ParametersType p;
    for(unsigned int i=0; i<6; i++)
      {
      p[i] = parameters[i];
      }

    m_Transform->SetParameters( p );

    PointType P2 = m_Transform->TransformPoint( m_P1 );
    PointType Q2 = m_Transform->TransformPoint( m_Q1 );

    MeasureType measure = P2.SquaredEuclideanDistanceTo( m_P ) +
                          Q2.SquaredEuclideanDistanceTo( m_Q ) ;

    std::cout << measure << std::endl;

    return measure;

  }

  void GetDerivative( const ParametersType & parameters,
                            DerivativeType & derivative  ) const
  {
  }

  unsigned int GetNumberOfParameters(void) const 
    {
    std::cout << "GetNumberOfParameters() = " << this->SpaceDimension << std::endl;
 //   return itkGetStaticConstMacro( SpaceDimension );
    return 6;
    }

private:

  mutable   TransformType::Pointer  m_Transform;
  PointType   m_P;
  PointType   m_Q;
  PointType   m_P1;
  PointType   m_Q1;

};






int itkVersorRigid3DTransformOptimizerTest(int, char* [] ) 
{
  std::cout << "VersorRigid3DTransform Optimizer Test ";
  std::cout << std::endl << std::endl;

  typedef  itk::VersorRigid3DTransformOptimizer  OptimizerType;

  typedef  OptimizerType::ScalesType            ScalesType;
  
  
  // Declaration of a itkOptimizer
  OptimizerType::Pointer  itkOptimizer = OptimizerType::New();


  // Declaration of the CostFunction adaptor
  versorCostFunction::Pointer costFunction = versorCostFunction::New();


  itkOptimizer->SetCostFunction( costFunction );

  
  typedef versorCostFunction::ParametersType    ParametersType;

  typedef itk::VersorRigid3DTransform< double > TransformType;

  typedef itk::Versor< double >                   VersorType;

  // We start with a null rotation
  VersorType::VectorType axis;
  axis[0] =  1.0f;
  axis[1] =  0.0f;
  axis[2] =  0.0f;

  VersorType::ValueType angle = 0.0f;

  VersorType initialRotation;
  initialRotation.Set( axis, angle );
  
  const unsigned int spaceDimensions = costFunction->GetNumberOfParameters();
  std::cout << "SpaceDimension = " << spaceDimensions << std::endl;

  ParametersType  initialPosition( spaceDimensions );
  initialPosition[0] = initialRotation.GetX();
  initialPosition[1] = initialRotation.GetY();
  initialPosition[2] = initialRotation.GetZ();
  initialPosition[3] = 0.0;
  initialPosition[4] = 0.0;
  initialPosition[5] = 0.0;

  ScalesType    parametersScale( spaceDimensions );
  parametersScale[0] = 1.0;
  parametersScale[1] = 1.0;
  parametersScale[2] = 1.0;
  parametersScale[3] = 1.0 / 50.0;
  parametersScale[4] = 1.0 / 50.0;
  parametersScale[5] = 1.0 / 50.0;

  itkOptimizer->MaximizeOn();
  itkOptimizer->SetScales( parametersScale );
  itkOptimizer->SetGradientMagnitudeTolerance( 1e-15 );
  itkOptimizer->SetMaximumStepLength( 0.1745 ); // About 10 deegres
  itkOptimizer->SetMinimumStepLength( 1e-9 );
  itkOptimizer->SetNumberOfIterations( 10 );

  itkOptimizer->SetInitialPosition( initialPosition );

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



  ParametersType finalPosition( spaceDimensions );
  finalPosition = itkOptimizer->GetCurrentPosition();

  VersorType finalRotation;
  VersorType::VectorType finalRightPart;
  for(unsigned int i=0; i< spaceDimensions; i++)
    {
    finalRightPart[i] = finalPosition[i];
    }
  finalRotation.Set( finalRightPart );
  std::cout << "Solution versor  = (" << finalRotation << ")" << std::endl;  

  VersorType::VectorType finalTranslation;
  for(unsigned int j=0; j< spaceDimensions; j++)
    {
    finalTranslation[j] = finalPosition[j+3];
    }
  std::cout << "Solution vector  = (" << finalTranslation << ")" << std::endl;  

  //
  // check results to see if it is within range
  //
  bool pass = true;

  // True versor

  VersorType::VectorType trueAxis;
  VersorType::ValueType  trueAngle;
  trueAxis[0]  = 1.0f;
  trueAxis[1]  = 0.0f;
  trueAxis[2]  = 0.0f;
  trueAngle = 10.0 * atan( 1.0f );
  VersorType trueRotation;
  trueRotation.Set( trueAxis, trueAngle );
    
  ParametersType trueParameters(spaceDimensions);
  trueParameters[0] = trueRotation.GetX();
  trueParameters[1] = trueRotation.GetY();
  trueParameters[2] = trueRotation.GetZ();
  trueParameters[3] =  0.0;
  trueParameters[4] = 30.0;
  trueParameters[5] = 30.0;
  
  std::cout << "SpaceDimension = " << spaceDimensions << std::endl;
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




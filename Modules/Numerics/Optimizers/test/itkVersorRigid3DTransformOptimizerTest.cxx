/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkVersorRigid3DTransformOptimizer.h"
#include "itkVersorRigid3DTransform.h"


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
 * \class versorRigid3DCostFunction
 *
 */
class versorRigid3DCostFunction : public itk::SingleValuedCostFunction
{
public:

  typedef versorRigid3DCostFunction           Self;
  typedef itk::SingleValuedCostFunction       Superclass;
  typedef itk::SmartPointer<Self>             Pointer;
  typedef itk::SmartPointer<const Self>       ConstPointer;

  typedef itk::VersorRigid3DTransform<double>        TransformType;

  itkNewMacro( Self );
  itkTypeMacro( versorRigid3DCostFunction, SingleValuedCostFunction );

  itkStaticConstMacro( SpaceDimension, unsigned int, 6 );

  typedef Superclass::ParametersType              ParametersType;
  typedef Superclass::DerivativeType              DerivativeType;

  typedef itk::Versor< double >                   VersorType;
  typedef VersorType::VectorType                  AxisType;
  typedef itk::Vector< double,  3 >               VectorType;
  typedef itk::Point<  double,  3 >               PointType;

  typedef double MeasureType;


  versorRigid3DCostFunction()
  {
    m_Transform = TransformType::New();

    m_P1[0] =  0.0;
    m_P1[1] =  0.0;
    m_P1[2] = 10.0;

    m_Q1[0] =  0.0;
    m_Q1[1] = 10.0;
    m_Q1[2] =  0.0;

    VersorType versor;
    const double angle = 10.0 * std::atan( 1.0 ) / 45.0;
    versor.SetRotationAroundX( angle );

    m_Transform->SetRotation( versor );

    TransformType::OutputVectorType translation;
    translation[0] =  0.0;
    translation[1] = 30.0;
    translation[2] = 30.0;

    m_Transform->SetTranslation( translation );

    m_P = m_Transform->TransformPoint( m_P1 );
    m_Q = m_Transform->TransformPoint( m_Q1 );

    std::cout << "Versor used = " << versor << std::endl;
    std::cout << "Vector used = " << translation << std::endl;

    std::cout << "m_P1 = " << m_P1  << std::endl;
    std::cout << "m_Q1 = " << m_Q1  << std::endl;
    std::cout << "m_P  = " << m_P   << std::endl;
    std::cout << "m_Q  = " << m_Q   << std::endl;
  }


  virtual MeasureType GetValue( const ParametersType & parameters ) const ITK_OVERRIDE
  {
    TransformType::ParametersType p( itkGetStaticConstMacro( SpaceDimension ));
    for(unsigned int i=0; i<6; i++)
      {
      p[i] = parameters[i];
      }

    m_Transform->SetParameters( p );

    PointType P2 = m_Transform->TransformPoint( m_P1 );
    PointType Q2 = m_Transform->TransformPoint( m_Q1 );

    MeasureType measure = P2.SquaredEuclideanDistanceTo( m_P ) +
                          Q2.SquaredEuclideanDistanceTo( m_Q );

    return measure;
  }

  void GetDerivative( const ParametersType & parameters,
                            DerivativeType & derivative  ) const ITK_OVERRIDE
  {
    VectorType rightPart;
    for(unsigned int i=0; i<3; i++)
      {
      rightPart[i] = parameters[i];
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

    ParametersType parametersPlustDeltaVX = parameters;
    ParametersType parametersPlustDeltaVY = parameters;
    ParametersType parametersPlustDeltaVZ = parameters;

    ParametersType parametersPlustDeltaTX = parameters;
    ParametersType parametersPlustDeltaTY = parameters;
    ParametersType parametersPlustDeltaTZ = parameters;

    parametersPlustDeltaVX[0] = plusdDeltaX.GetX();
    parametersPlustDeltaVX[1] = plusdDeltaX.GetY();
    parametersPlustDeltaVX[2] = plusdDeltaX.GetZ();

    parametersPlustDeltaVY[0] = plusdDeltaY.GetX();
    parametersPlustDeltaVY[1] = plusdDeltaY.GetY();
    parametersPlustDeltaVY[2] = plusdDeltaY.GetZ();

    parametersPlustDeltaVZ[0] = plusdDeltaZ.GetX();
    parametersPlustDeltaVZ[1] = plusdDeltaZ.GetY();
    parametersPlustDeltaVZ[2] = plusdDeltaZ.GetZ();

    const MeasureType turnXValue = this->GetValue( parametersPlustDeltaVX );
    const MeasureType turnYValue = this->GetValue( parametersPlustDeltaVY );
    const MeasureType turnZValue = this->GetValue( parametersPlustDeltaVZ );

    derivative = DerivativeType( SpaceDimension );

    derivative[0] = ( turnXValue - baseValue ) / deltaAngle;
    derivative[1] = ( turnYValue - baseValue ) / deltaAngle;
    derivative[2] = ( turnZValue - baseValue ) / deltaAngle;

    const double deltaTranslation = deltaAngle; // just to keep the scaling

    parametersPlustDeltaTX[3] += deltaTranslation;
    parametersPlustDeltaTY[4] += deltaTranslation;
    parametersPlustDeltaTZ[5] += deltaTranslation;

    const MeasureType transXValue = this->GetValue( parametersPlustDeltaTX );
    const MeasureType transYValue = this->GetValue( parametersPlustDeltaTY );
    const MeasureType transZValue = this->GetValue( parametersPlustDeltaTZ );

    derivative[3] = ( transXValue - baseValue ) / deltaTranslation;
    derivative[4] = ( transYValue - baseValue ) / deltaTranslation;
    derivative[5] = ( transZValue - baseValue ) / deltaTranslation;

  }

  virtual unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE
    {
    return itkGetStaticConstMacro( SpaceDimension );
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
  versorRigid3DCostFunction::Pointer costFunction = versorRigid3DCostFunction::New();


  itkOptimizer->SetCostFunction( costFunction );


  typedef versorRigid3DCostFunction::ParametersType ParametersType;
  typedef itk::Versor< double >                     VersorType;

  // We start with a null rotation
  VersorType::VectorType axis;
  axis[0] =  1.0f;
  axis[1] =  0.0f;
  axis[2] =  0.0f;

  VersorType::ValueType angle = 0.0f;

  VersorType initialRotation;
  initialRotation.Set( axis, angle );

  const unsigned int parametersDimensions = costFunction->GetNumberOfParameters();

  ParametersType  initialPosition( parametersDimensions );
  initialPosition[0] = initialRotation.GetX();
  initialPosition[1] = initialRotation.GetY();
  initialPosition[2] = initialRotation.GetZ();
  initialPosition[3] = 0.0;
  initialPosition[4] = 0.0;
  initialPosition[5] = 0.0;

  ScalesType    parametersScale( parametersDimensions );
  const double translationScaleFactor = 50.0;
  parametersScale[0] = 1.0;
  parametersScale[1] = 1.0;
  parametersScale[2] = 1.0;
  parametersScale[3] = 1.0 / translationScaleFactor;
  parametersScale[4] = 1.0 / translationScaleFactor;
  parametersScale[5] = 1.0 / translationScaleFactor;

  itkOptimizer->MaximizeOff();
  itkOptimizer->SetScales( parametersScale );
  itkOptimizer->SetGradientMagnitudeTolerance( 1e-35 );
  itkOptimizer->SetMaximumStepLength( 10.0 );
  itkOptimizer->SetMinimumStepLength( 1e-5 );
  itkOptimizer->SetNumberOfIterations( 50 );

  std::cout << "Initial Position = " << std::endl;
  std::cout << initialPosition << std::endl << std::endl;

  itkOptimizer->SetInitialPosition( initialPosition );

  try
    {
    itkOptimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  ParametersType finalPosition( parametersDimensions );
  finalPosition = itkOptimizer->GetCurrentPosition();

  const unsigned int spaceDimensions = 3;

  VersorType finalRotation;
  VersorType::VectorType finalRightPart;
  for(unsigned int i=0; i< spaceDimensions; i++)
    {
    finalRightPart[ i ] = finalPosition[ i ];
    }
  finalRotation.Set( finalRightPart );
  std::cout << std::endl;
  std::cout << "Solution versor  = (" << finalRotation << ")" << std::endl;

  VersorType::VectorType finalTranslation;
  for(unsigned int j=0; j< spaceDimensions; j++)
    {
    finalTranslation[ j ] = finalPosition[ j + spaceDimensions ];
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
  trueAngle = 10.0 * std::atan( 1.0f ) / 45.0;
  VersorType trueRotation;
  trueRotation.Set( trueAxis, trueAngle );

  ParametersType trueParameters( parametersDimensions );
  trueParameters[0] = trueRotation.GetX();
  trueParameters[1] = trueRotation.GetY();
  trueParameters[2] = trueRotation.GetZ();
  trueParameters[3] =  0.0;
  trueParameters[4] = 30.0;
  trueParameters[5] = 30.0;

  std::cout << std::endl;
  std::cout << "Final parameters = " << finalPosition << std::endl;
  std::cout << "True Parameters  = " << trueParameters << std::endl;

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
    std::cout << std::endl << "Test FAILEd !" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << std::endl << "Test PASSED !" << std::endl;
  return EXIT_SUCCESS;


}

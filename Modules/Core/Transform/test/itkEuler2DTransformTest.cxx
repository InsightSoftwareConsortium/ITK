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

#include <iostream>

#include "itkEuler2DTransform.h"
#include "itkMath.h"


namespace
{
bool CheckEqual(
  itk::Point<double, 2> p1,
  itk::Point<double, 2> p2 )
{
  const double epsilon = 1e-5;

  for( unsigned int i = 0; i < 2; i++ )
    {
    if( std::fabs( p1[i] - p2[i] ) > epsilon )
      {
      std::cout << p1 << " != " << p2 << ":[ FAILED ]" << std::endl;
      return false;
      }
    }
  std::cout << p1 << " == " << p2 << ":[ PASSED ]" << std::endl;
  return true;
}

}

int itkEuler2DTransformTest(int argc, char *argv[] )
{
  if( argc < 1 )
    {
    std::cout << "Usage: " << argv[0] << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "==================================" << std::endl;
  std::cout << "Testing Euler Angles 2D Transform" << std::endl << std::endl;

  const double       epsilon = 1e-10;
  const unsigned int N = 2;
  bool               Ok = true;

  typedef itk::Euler2DTransform<double> EulerTransformType;
  EulerTransformType::Pointer eulerTransform = EulerTransformType::New();

  // Testing Identity
  std::cout << "Testing identity transform: ";
  eulerTransform->SetIdentity();

  EulerTransformType::OffsetType offset = eulerTransform->GetOffset();
  if( offset[0] != 0.0
      || offset[1] != 0.0
      )
    {
    std::cout << "[ FAILED ]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[ PASSED ]" << std::endl;

  // 15 degrees in radians
  const double angle = 15.0 * std::atan( 1.0f ) / 45.0;
  const double sinth = std::sin( angle );
  const double costh = std::cos( angle );

  std::cout << "Testing Rotation:";
  eulerTransform->SetRotation(angle);

  // Rotate an itk::Point
  EulerTransformType::InputPointType::ValueType pInit[2] = {10, 10};
  EulerTransformType::InputPointType            p = pInit;
  EulerTransformType::InputPointType            q;

  q[0] =  p[0] * costh - p[1] * sinth;
  q[1] =  p[0] * sinth + p[1] * costh;

  EulerTransformType::OutputPointType r;
  r = eulerTransform->TransformPoint( p );
  for( unsigned int i = 0; i < N; i++ )
    {
    if( std::fabs( q[i] - r[i] ) > epsilon )
      {
      Ok = false;
      break;
      }
    }
  if( !Ok )
    {
    std::cerr << "Error rotating point   : " << p << std::endl;
    std::cerr << "Result should be       : " << q << std::endl;
    std::cerr << "Reported Result is     : " << r << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << " [ PASSED ] " << std::endl;
    }

  std::cout << "Testing Translation:";

  eulerTransform->SetRotation(0);

  EulerTransformType::OffsetType::ValueType ioffsetInit[2] = {1, 4};
  EulerTransformType::OffsetType            ioffset = ioffsetInit;

  eulerTransform->SetOffset( ioffset );

  q = p + ioffset;

  r = eulerTransform->TransformPoint( p );
  for( unsigned int i = 0; i < N; i++ )
    {
    if( std::fabs( q[i] - r[i] ) > epsilon )
      {
      Ok = false;
      break;
      }
    }
  if( !Ok )
    {
    std::cerr << "Error translating point: " << p << std::endl;
    std::cerr << "Result should be       : " << q << std::endl;
    std::cerr << "Reported Result is     : " << r << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << " [ PASSED ] " << std::endl;
    }

  // Testing Parameters
  std::cout << "Testing Parameters: ";
  EulerTransformType::ParametersType parameters = eulerTransform->GetParameters();

  if( parameters[0] != 0.0
      || parameters[1] != 1.0
      || parameters[2] != 4.0
      )
    {
    std::cout << " [ FAILED ] " << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;

  // Testing Jacobian
  std::cout << "Testing Jacobian: ";
  EulerTransformType::JacobianType jacobian;
  eulerTransform->ComputeJacobianWithRespectToParameters(pInit, jacobian);

  if( itk::Math::NotExactlyEquals(jacobian[0][0], -10.0) || jacobian[0][1] != 1.0
      || jacobian[0][2] != 0.0
      || jacobian[1][0] != 10.0 || jacobian[1][1] != 0.0
      || jacobian[1][2] != 1.0
      )
    {
    std::cout << " [ FAILED ] " << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;

  std::cout << "Testing Angle from matrix : ";
  eulerTransform->SetIdentity();
  eulerTransform->SetRotation(0.2);

  EulerTransformType::Pointer t2 = EulerTransformType::New();
  t2->SetIdentity();
  t2->Compose(eulerTransform);
  if( std::fabs(t2->GetParameters()[0] - 0.2) > 0.0001 )
    {
    std::cout << " [ FAILED ] " << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;

    {
    // Test instantiation, inverse computation, back transform etc.
    typedef EulerTransformType TransformType;
    TransformType::Pointer t1 = TransformType::New();

    // Set parameters
    TransformType::ParametersType parameters2( t1->GetNumberOfParameters() );
    TransformType::InputPointType center;

    parameters2[0] = -21.0 / 180.0 * itk::Math::pi;
    parameters2[1] = 67.8;
    parameters2[2] = -0.2;

    center[0] = 12.0;
    center[1] = -8.9;

    t1->SetParameters( parameters2 );
    t1->SetCenter( center );

    TransformType::InputPointType p1;
    p1[0] = 96.8;
    p1[1] = -3.2;

    TransformType::InputPointType p2;
    p2 = t1->TransformPoint( p1 );

    // Test inverse
    TransformType::Pointer t22;
    t1->CloneInverseTo( t22 );

    TransformType::InputPointType p3;
    p3 = t22->TransformPoint( p2 );

    std::cout << "Test CloneInverseTo(): ";
    if( !CheckEqual( p1, p3 ) )
      {
      return EXIT_FAILURE;
      }

    TransformType::Pointer t2dash = TransformType::New();
    t1->GetInverse( t2dash );
    TransformType::InputPointType p3dash;
    p3dash = t2dash->TransformPoint( p2 );

    std::cout << "Test GetInverse(): ";
    if( !CheckEqual( p1, p3dash ) )
      {
      return EXIT_FAILURE;
      }

    t2dash = dynamic_cast<TransformType *>(t1->GetInverseTransform().GetPointer() );
    if( !t2dash )
      {
      std::cout << "Cannot compute inverse transformation" << std::endl;
      return EXIT_FAILURE;
      }
    p3dash = t2dash->TransformPoint( p2 );

    std::cout << "Test GetInverseTransform(): ";
    if( !CheckEqual( p1, p3dash ) )
      {
      return EXIT_FAILURE;
      }

    // Test clone
    TransformType::Pointer t3;
    t1->CloneTo( t3 );

    TransformType::InputPointType p4;
    p4 = t3->TransformPoint( p1 );

    std::cout << "Test Clone(): ";
    if( !CheckEqual( p2, p4 ) )
      {
      return EXIT_FAILURE;
      }

    // Test compose
    TransformType::Pointer t4 = TransformType::New();

    parameters2[0] = 14.7 / 180.0 * itk::Math::pi;
    parameters2[1] = 67.1;
    parameters2[2] = 67.1;

    center.Fill( 4.0 );

    t4->SetParameters( parameters2 );
    t4->SetCenter( center );

    TransformType::Pointer t5;
    t1->CloneTo( t5 );
    t5->Compose( t4, false );

    TransformType::InputPointType p5, p6, p7;
    p5 = t1->TransformPoint( p1 );
    p6 = t4->TransformPoint( p5 );
    p7 = t5->TransformPoint( p1 );

    std::cout << "Test Compose(.,false): ";
    if( !CheckEqual( p6, p7 ) )
      {
      return EXIT_FAILURE;
      }

    t1->CloneTo( t5 );
    t5->Compose( t4, true );

    p5 = t4->TransformPoint( p1 );
    p6 = t1->TransformPoint( p5 );
    p7 = t5->TransformPoint( p1 );

    std::cout << "Test Compose(.,true): ";
    if( !CheckEqual( p6, p7 ) )
      {
      return EXIT_FAILURE;
      }

    // Really test the jacobian
    std::cout << "Testing Jacobian: ";
    TransformType::JacobianType jacobian2;
    t4->ComputeJacobianWithRespectToParameters( p1, jacobian2 );

    TransformType::JacobianType approxJacobian = jacobian2;
    for( unsigned int k = 0; k < t1->GetNumberOfParameters(); k++ )
      {
      const double                  delta = 0.001;
      TransformType::ParametersType plusParameters;
      TransformType::ParametersType minusParameters;

      plusParameters = parameters2;
      minusParameters = parameters2;
      plusParameters[k] += delta;
      minusParameters[k] -= delta;

      TransformType::OutputPointType plusPoint;
      TransformType::OutputPointType minusPoint;

      t4->SetParameters( plusParameters );
      plusPoint = t4->TransformPoint( p1 );
      t4->SetParameters( minusParameters );
      minusPoint = t4->TransformPoint( p1 );
      for( unsigned int j = 0; j < 2; j++ )
        {
        double approxDerivative = ( plusPoint[j] - minusPoint[j] ) / ( 2.0 * delta );
        double computedDerivative = jacobian2[j][k];
        approxJacobian[j][k] = approxDerivative;
        if( itk::Math::abs( approxDerivative - computedDerivative ) > 1e-4 )
          {
          std::cerr << "Error computing Jacobian [" << j << "][" << k << "]" << std::endl;
          std::cerr << "Result should be: " << approxDerivative << std::endl;
          std::cerr << "Reported result is: " << computedDerivative << std::endl;
          std::cerr << " [ FAILED ] " << std::endl;
          return EXIT_FAILURE;
          } // if
        }   // for j

      } // for k

    std::cout << " [ PASSED ] " << std::endl;

    }

    {
    // Test Set/Get Matrix and Set/Get Offset
    typedef EulerTransformType TransformType;
    TransformType::Pointer t1 = TransformType::New();
    TransformType::Pointer t23 = TransformType::New();

    TransformType::InputPointType center;
    center[0] = 9.0;
    center[1] = 10.0;

    TransformType::ParametersType parameters3( t1->GetNumberOfParameters() );
    for( unsigned int j = 0; j < t1->GetNumberOfParameters(); j++ )
      {
      parameters3[j] = static_cast<double>( j ) + 1.0;
      }
    parameters3[0] *= itk::Math::pi / 180.0;

    t1->SetCenter( center );
    t1->SetParameters( parameters3 );

    t23->SetCenter( center );
    t23->SetMatrix( t1->GetMatrix() );
    t23->SetOffset( t1->GetOffset() );

    // check the transformed points are the same
    TransformType::InputPointType ip;
    ip[0] = 8.0;
    ip[1] = 9.0;

    TransformType::OutputPointType op1, op2;
    op1 = t1->TransformPoint( ip );
    op2 = t23->TransformPoint( ip );

    std::cout << "Test Set/GetMatrix() and Set/GetOffset(): ";
    if( !CheckEqual( op1, op2 ) )
      {
      return EXIT_FAILURE;
      }

    // check that parameters are the same
    TransformType::ParametersType pdash = t23->GetParameters();

    std::cout << "Test Set/GetMatrix() and Set/GetOffset(): ";
    for( unsigned int j = 0; j < t1->GetNumberOfParameters(); j++ )
      {
      if( std::fabs( parameters3[j] - pdash[j] ) > epsilon )
        {
        std::cout << "Expected: " << parameters3 << std::endl;
        std::cout << "Got: " << pdash << std::endl;
        std::cout << " [ FAILED ] " << std::endl;
        return EXIT_FAILURE;
        }
      }
    std::cout << " [ PASSED ] " << std::endl;

    }

  return EXIT_SUCCESS;

}

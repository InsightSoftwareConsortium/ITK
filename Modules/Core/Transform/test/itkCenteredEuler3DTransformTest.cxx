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

#include "itkCenteredEuler3DTransform.h"

int itkCenteredEuler3DTransformTest(int, char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing Centered Euler Angles 3D Transform" << std::endl << std::endl;

  const double       epsilon = 1e-10;
  const unsigned int N = 3;
  bool               Ok = true;

  typedef itk::CenteredEuler3DTransform<double> EulerTransformType;
  EulerTransformType::Pointer eulerTransform = EulerTransformType::New();

  // Testing Identity
  std::cout << "Testing identity transform: ";
  eulerTransform->SetIdentity();

  EulerTransformType::OffsetType offset = eulerTransform->GetOffset();
  if( offset[0] != 0.0
      || offset[1] != 0.0
      || offset[2] != 0.0
      )
    {
    std::cout << "[ FAILED ]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[ PASSED ]" << std::endl;

  // 15 degrees in radians
  const double angleX = 15.0 * std::atan( 1.0f ) / 45.0;
  const double cx = std::cos(angleX);
  const double sx = std::sin(angleX);

  // 10 degrees in radians
  const double angleY = 10.0 * std::atan( 1.0f ) / 45.0;
  const double cy = std::cos(angleY);
  const double sy = std::sin(angleY);

  // 5 degrees in radians
  const double angleZ = 5.0 * std::atan( 1.0f ) / 45.0;
  const double cz = std::cos(angleZ);
  const double sz = std::sin(angleZ);

  std::cout << "Testing Rotation:";
  eulerTransform->SetRotation(angleX, angleY, angleZ);

  // Rotate an itk::Point
  EulerTransformType::InputPointType::ValueType pInit[3] = {10, -5, 3};
  EulerTransformType::InputPointType            p = pInit;
  EulerTransformType::InputPointType            q;

  itk::Matrix<double, 3, 3> RotationX;
  RotationX[0][0] = 1; RotationX[0][1] = 0; RotationX[0][2] = 0;
  RotationX[1][0] = 0; RotationX[1][1] = cx; RotationX[1][2] = -sx;
  RotationX[2][0] = 0; RotationX[2][1] = sx; RotationX[2][2] = cx;

  itk::Matrix<double, 3, 3> RotationY;
  RotationY[0][0] = cy; RotationY[0][1] = 0; RotationY[0][2] = sy;
  RotationY[1][0] = 0; RotationY[1][1] = 1; RotationY[1][2] = 0;
  RotationY[2][0] = -sy; RotationY[2][1] = 0; RotationY[2][2] = cy;

  itk::Matrix<double, 3, 3> RotationZ;
  RotationZ[0][0] = cz; RotationZ[0][1] = -sz; RotationZ[0][2] = 0;
  RotationZ[1][0] = sz; RotationZ[1][1] = cz; RotationZ[1][2] = 0;
  RotationZ[2][0] = 0; RotationZ[2][1] = 0; RotationZ[2][2] = 1;

  q = RotationZ * RotationX * RotationY * p; // standard transformation

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

  eulerTransform->SetRotation(0, 0, 0);

  EulerTransformType::OffsetType::ValueType ioffsetInit[3] = {1, -4, 8};
  EulerTransformType::OffsetType            ioffset = ioffsetInit;

  eulerTransform->SetOffset( ioffset );
  std::cout << "eulerTransform: " << eulerTransform;

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
  std::cout << "Testing Set/Get Parameters: ";
  EulerTransformType::ParametersType parameters(9);
  parameters.Fill(0);
  for( unsigned int i = 0; i < 3; i++ )
    {
    parameters[i] = i;
    }
  for( unsigned int i = 0; i < 3; i++ )
    {
    parameters[i + 6] = i + 3;
    }

  eulerTransform->SetParameters(parameters);
  EulerTransformType::ParametersType parameters_result =
    eulerTransform->GetParameters();

  if( parameters_result[0] != 0.0
      || parameters_result[1] != 1.0
      || parameters_result[2] != 2.0
      || parameters_result[6] != 3.0
      || parameters_result[7] != 4.0
      || parameters_result[8] != 5.0
      )
    {
    std::cout << " [ FAILED ] " << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;

  // Testing Jacobian
  std::cout << "Testing Jacobian: ";
  for( unsigned int i = 0; i < 3; i++ )
    {
    pInit[i] = 0;
    }

  EulerTransformType::JacobianType jacobian;
  eulerTransform->ComputeJacobianWithRespectToParameters( pInit, jacobian );

  if( jacobian[0][0] != 0.0 || jacobian[0][1] != 0.0
      || jacobian[0][2] != 0.0 || jacobian[0][3] != 1.0
      || jacobian[0][4] != 0.0 || jacobian[0][5] != 0.0
      || jacobian[1][0] != 0.0 || jacobian[1][1] != 0.0
      || jacobian[1][2] != 0.0 || jacobian[1][3] != 0.0
      || jacobian[1][4] != 1.0 || jacobian[1][5] != 0.0
      || jacobian[2][0] != 0.0 || jacobian[2][1] != 0.0
      || jacobian[2][2] != 0.0 || jacobian[2][3] != 0.0
      || jacobian[2][4] != 0.0 || jacobian[2][5] != 1.0 )
    {
    std::cout << " [ FAILED ] " << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;

  // Really test the Jacobian
  EulerTransformType::InputPointType center;
  center[0] = 0.2;
  center[1] = 7.0;
  center[2] = 4.0;
  eulerTransform->SetCenter( center );

  eulerTransform->Print( std::cout );
  for( unsigned int pp = 0; pp < 2; pp++ )
    {
    std::cout << "Testing Jacobian when ComputeZYX is ";
    if( pp == 0 )
      {
      std::cout << "true" << std::endl;
      eulerTransform->SetComputeZYX( true );
      }
    else
      {
      std::cout << "false" << std::endl;
      eulerTransform->SetComputeZYX( false );
      }

    parameters.Fill( 0.0 );
    parameters[0] = 0.2 / 180.0 * itk::Math::pi;
    parameters[1] = -1.0 / 180.0 * itk::Math::pi;
    parameters[2] = 2.4 / 180.0 * itk::Math::pi;
    parameters[3] = 0;
    parameters[4] = 0;
    parameters[5] = 0;
    parameters[6] = 5.0;
    parameters[7] = 6.0;
    parameters[8] = 8.0;

    eulerTransform->SetParameters( parameters );

    pInit[0] = 1.0;
    pInit[1] = 1.5;
    pInit[2] = 2.6;

    eulerTransform->ComputeJacobianWithRespectToParameters( pInit, jacobian );
    std::cout << jacobian << std::endl;

    EulerTransformType::JacobianType approxJacobian = jacobian;
    for( unsigned int k = 0; k < eulerTransform->GetNumberOfParameters(); k++ )
      {
      const double                       delta = 0.001;
      EulerTransformType::ParametersType plusParameters;
      EulerTransformType::ParametersType minusParameters;

      plusParameters = parameters;
      minusParameters = parameters;
      plusParameters[k] += delta;
      minusParameters[k] -= delta;

      EulerTransformType::OutputPointType plusPoint;
      EulerTransformType::OutputPointType minusPoint;

      eulerTransform->SetParameters( plusParameters );
      plusPoint = eulerTransform->TransformPoint( pInit );
      eulerTransform->SetParameters( minusParameters );
      minusPoint = eulerTransform->TransformPoint( pInit );

      if( k < 3 || k > 5 ) // Jacobian is approx as identity for center of rotation
        {
        for( unsigned int j = 0; j < 3; j++ )
          {
          double approxDerivative = ( plusPoint[j] - minusPoint[j] )
            / ( 2.0 * delta );
          double computedDerivative = jacobian[j][k];
          approxJacobian[j][k] = approxDerivative;
          if( itk::Math::abs( approxDerivative - computedDerivative ) > 1e-5 )
            {
            std::cerr << "Error computing Jacobian [" << j << "]["
                      << k << "]" << std::endl;
            std::cerr << "Result should be: " << approxDerivative << std::endl;
            std::cerr << "Reported result is: " << computedDerivative
                      << std::endl;
            std::cerr << " [ FAILED ] " << std::endl;
            return EXIT_FAILURE;
            }
          }
        }
      }

    std::cout << approxJacobian << std::endl;
    std::cout << " [ PASSED ] " << std::endl;

    }

  std::cout << "Testing Angle from matrix : ";
  eulerTransform->SetIdentity();

  eulerTransform->SetRotation(0.2, 0.1, 0.3);

  EulerTransformType::Pointer t2 = EulerTransformType::New();
  t2->SetIdentity();
  t2->Compose(eulerTransform);
  if( (std::fabs(t2->GetParameters()[0] - 0.2) > 0.0001)
      || (std::fabs(t2->GetParameters()[1] - 0.1) > 0.0001)
      || (std::fabs(t2->GetParameters()[2] - 0.3) > 0.0001)
      )
    {
    std::cout << " [ FAILED ] " << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;

  std::cout << "Testing Angle from matrix (ZYX) : ";
  eulerTransform->SetIdentity();
  eulerTransform->SetComputeZYX(true);
  eulerTransform->SetRotation(0.2, 0.1, 0.3);

  t2->SetIdentity();
  t2->SetComputeZYX(true);
  t2->Compose(eulerTransform);

  if( (std::fabs(t2->GetParameters()[0] - 0.2) > 0.0001)
      || (std::fabs(t2->GetParameters()[1] - 0.1) > 0.0001)
      || (std::fabs(t2->GetParameters()[2] - 0.3) > 0.0001)
      )
    {
    std::cout << " [ FAILED ] " << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;

  EulerTransformType::Pointer t3 = EulerTransformType::New();
  t2->GetInverse( t3 );

  t3 = dynamic_cast<EulerTransformType *>(t2->GetInverseTransform().GetPointer() );
  if( !t3 )
    {
    std::cout << "Cannot compute inverse transformation" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}

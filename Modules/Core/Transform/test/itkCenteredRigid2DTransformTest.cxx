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

#include "itkCenteredRigid2DTransform.h"


namespace
{
static bool CheckEqual(
  const itk::Point<double, 2> & p1,
  const itk::Point<double, 2> & p2 )
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

int itkCenteredRigid2DTransformTest(int argc, char *argv[] )
{
  if( argc < 1 )
    {
    std::cout << "Usage: " << argv[0] << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "==================================" << std::endl;
  std::cout << "Testing CenteredRigid 2D Transform" << std::endl << std::endl;

  const double       epsilon = 1e-10;
  const unsigned int N = 2;
  bool               Ok = true;

  typedef itk::CenteredRigid2DTransform<double> CenteredRigidTransformType;
  CenteredRigidTransformType::Pointer transform = CenteredRigidTransformType::New();

  // 15 degrees in radians
  const double angle = 15.0 * std::atan( 1.0f ) / 45.0;
  const double sinth = std::sin( angle );
  const double costh = std::cos( angle );

  std::cout << "Testing Rotation:";
  transform->SetAngle(angle);

  // Rotate an itk::Point
  CenteredRigidTransformType::InputPointType::ValueType pInit[2] = {10, 10};
  CenteredRigidTransformType::InputPointType            p = pInit;
  CenteredRigidTransformType::InputPointType            q;

  q[0] =  p[0] * costh - p[1] * sinth;
  q[1] =  p[0] * sinth + p[1] * costh;

  CenteredRigidTransformType::OutputPointType r = transform->TransformPoint( p );
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

  transform->SetAngle(0);

  CenteredRigidTransformType::OffsetType::ValueType ioffsetInit[2] = {1, 4};
  CenteredRigidTransformType::OffsetType            ioffset = ioffsetInit;

  transform->SetOffset( ioffset );

  q = p + ioffset;

  r = transform->TransformPoint( p );
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

    {
    std::cout << "Testing Inverse:";

    // Populate the transform with some parameters
    CenteredRigidTransformType::Pointer transform2 = CenteredRigidTransformType::New();
    const double                        a = 0.175;
    transform2->SetAngle( a );

    CenteredRigidTransformType::InputPointType c;
    c[0] = 13.456;
    c[1] = 45.890;
    transform2->SetCenter( c );

    CenteredRigidTransformType::OutputVectorType t;
    t[0] = 9.873;
    t[1] = 40.312;
    transform2->SetTranslation( t );

    // Transform point p1 to obtain p2
    CenteredRigidTransformType::InputPointType p1;
    p1[0] = 5.63;
    p1[1] = 9.02;

    const CenteredRigidTransformType::OutputPointType p2 =
      transform2->TransformPoint( p1 );

    // Get inverse transform and transform point p2 to obtain point p3
    CenteredRigidTransformType::Pointer inverse;
    transform2->CloneInverseTo( inverse );

    CenteredRigidTransformType::OutputPointType p3 =
      inverse->TransformPoint( p2 );

    // Check that point p3 is the same as point p1
    Ok = true;
    for( unsigned int i = 0; i < N; i++ )
      {
      if( std::fabs( p1[i] - p3[i] ) > epsilon )
        {
        Ok = false;
        break;
        }
      }
    if( !Ok )
      {
      std::cerr << "Error in inverse computation" << std::endl;
      std::cerr << "Result should be       : " << p1 << std::endl;
      std::cerr << "Reported Result is     : " << p3 << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << " [ PASSED ] " << std::endl;
      }

    // Get inverse transform and transform point p2 to obtain point p3
    CenteredRigidTransformType::Pointer inversebis
      = dynamic_cast<CenteredRigidTransformType *>(transform2->GetInverseTransform().GetPointer() );

    if( !inversebis )
      {
      std::cout << "Cannot compute inverse transformation" << std::endl;
      return EXIT_FAILURE;
      }

    p3 = inversebis->TransformPoint( p2 );

    // Check that point p3 is the same as point p1
    Ok = true;
    for( unsigned int i = 0; i < N; i++ )
      {
      if( std::fabs( p1[i] - p3[i] ) > epsilon )
        {
        Ok = false;
        break;
        }
      }
    if( !Ok )
      {
      std::cerr << "Error in inverse computation" << std::endl;
      std::cerr << "Result should be       : " << p1 << std::endl;
      std::cerr << "Reported Result is     : " << p3 << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << " [ PASSED ] " << std::endl;
      }
    }

    {
    // Test instantiation, inverse computation, back transform etc.
    typedef CenteredRigidTransformType TransformType;
    TransformType::Pointer t1 = TransformType::New();

    // Set parameters
    TransformType::ParametersType parameters( t1->GetNumberOfParameters() );

    parameters[0] = -21.0 / 180.0 * itk::Math::pi;
    parameters[1] = 12.0;
    parameters[2] = -8.9;
    parameters[3] = 67.8;
    parameters[4] = -0.2;

    t1->SetParameters( parameters );

    TransformType::InputPointType p1;
    p1[0] = 96.8;
    p1[1] = -3.2;

    const TransformType::InputPointType p2 = t1->TransformPoint( p1 );

    // Test inverse
    TransformType::Pointer t2;
    t1->CloneInverseTo( t2 );

    TransformType::InputPointType p3 = t2->TransformPoint( p2 );

    std::cout << "Test CloneInverseTo(): ";
    if( !CheckEqual( p1, p3 ) )
      {
      return EXIT_FAILURE;
      }

    TransformType::Pointer t2dash = TransformType::New();
    t1->GetInverse( t2dash );
    TransformType::InputPointType p3dash = t2dash->TransformPoint( p2 );

    std::cout << "Test GetInverseTransform(): ";
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

    TransformType::InputPointType p4 = t3->TransformPoint( p1 );

    std::cout << "Test Clone(): ";
    if( !CheckEqual( p2, p4 ) )
      {
      return EXIT_FAILURE;
      }

    // Test compose
    TransformType::Pointer t4 = TransformType::New();

    parameters[0] = 14.7 / 180.0 * itk::Math::pi;
    parameters[1] = 4.0;
    parameters[2] = 4.0;
    parameters[3] = 67.1;
    parameters[4] = 67.1;

    t4->SetParameters( parameters );

    TransformType::Pointer t5;
    t1->CloneTo( t5 );
    t5->Compose( t4, false );

    TransformType::InputPointType p5 = t1->TransformPoint( p1 );
    TransformType::InputPointType p6 = t4->TransformPoint( p5 );
    TransformType::InputPointType p7 = t5->TransformPoint( p1 );

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
    TransformType::JacobianType jacobian;
    t4->ComputeJacobianWithRespectToParameters( p1, jacobian );

    TransformType::JacobianType approxJacobian = jacobian;
    for( unsigned int k = 0; k < t1->GetNumberOfParameters(); k++ )
      {
      const double                  delta = 0.001;
      TransformType::ParametersType plusParameters;
      TransformType::ParametersType minusParameters;

      plusParameters = parameters;
      minusParameters = parameters;
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
        const double approxDerivative = ( plusPoint[j] - minusPoint[j] ) / ( 2.0 * delta );
        const double computedDerivative = jacobian[j][k];
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

  return EXIT_SUCCESS;

}

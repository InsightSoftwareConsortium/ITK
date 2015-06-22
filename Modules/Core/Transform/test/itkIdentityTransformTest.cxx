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

#include "itkIdentityTransform.h"
#include "itkTestingMacros.h"

int itkIdentityTransformTest(int, char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing Identity Transform " << std::endl << std::endl;

  const double       epsilon = 1e-10;
  const unsigned int N = 2;
  bool               Ok = true;

  typedef itk::IdentityTransform<double> IdentityTransformType;
  IdentityTransformType::Pointer transform = IdentityTransformType::New();

  std::cout << "Testing TransformPoint: ";
  IdentityTransformType::InputPointType   p( 10 );
  IdentityTransformType::OutputPointType  r;

  r = transform->TransformPoint( p );
  for( unsigned int i = 0; i < N; i++ )
    {
    if( std::fabs( p[i] - r[i] ) > epsilon )
      {
      Ok = false;
      break;
      }
    }
  if( !Ok )
    {
    std::cerr << "Error Transforming Point" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << " [ PASSED ] " << std::endl;
    }

  // Test TransformVector
  std::cout << "Testing TransformVector: ";
  IdentityTransformType::InputVectorType vin;
  vin[0] = 1;
  vin[1] = 2;
  IdentityTransformType::OutputVectorType vout;

  vout = transform->TransformVector( vin );
  for( unsigned int i = 0; i < N; i++ )
    {
    if( std::fabs( vout[i] - vin[i] ) > epsilon )
      {
      Ok = false;
      break;
      }
    }
  if( !Ok )
    {
    std::cerr << "Error with TransformVector itk::Vector" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << " [ PASSED ] " << std::endl;
    }

  // Test TransformVector vnl_vector
  std::cout << "Testing TransformVector (vnl): ";
  IdentityTransformType::InputVnlVectorType vnlin;
  vnlin[0] = 1;
  vnlin[1] = 2;
  IdentityTransformType::OutputVnlVectorType vnlout;

  vnlout = transform->TransformVector( vnlin );
  for( unsigned int i = 0; i < N; i++ )
    {
    if( std::fabs( vnlout[i] - vnlin[i] ) > epsilon )
      {
      Ok = false;
      break;
      }
    }
  if( !Ok )
    {
    std::cerr << "Error with TransformVector vnlVector" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << " [ PASSED ] " << std::endl;
    }

  // Test TransformCovariantVector
  std::cout << "Testing TransformCovariantVector: ";
  IdentityTransformType::InputCovariantVectorType vcin;
  vcin[0] = 1;
  vcin[1] = 2;
  IdentityTransformType::OutputCovariantVectorType vcout;

  vcout = transform->TransformCovariantVector( vcin );
  for( unsigned int i = 0; i < N; i++ )
    {
    if( std::fabs( vcout[i] - vcin[i] ) > epsilon )
      {
      Ok = false;
      break;
      }
    }
  if( !Ok )
    {
    std::cerr << "Error with TransformVector CovariantVector" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << " [ PASSED ] " << std::endl;
    }

  // Test the Set/Get Parameters
  std::cout << "Testing Set/GetParameters():";
  IdentityTransformType::ParametersType params(0);
  transform->SetParameters(params);
  std::cout << " [ PASSED ] " << std::endl;

  // Test the GetNumberOfParameters() method
  std::cout << "Testing GetNumberOfParameters():";
  unsigned int numParams = transform->GetNumberOfParameters();
  if( numParams != 0 )
    {
    std::cerr << "Error with GetNumberOfParameters" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << " [ PASSED ] " << std::endl;
    }

  // Testing the Jacobian
  std::cout << "Testing Jacobian: ";
  IdentityTransformType::JacobianType jacobian;
  transform->ComputeJacobianWithRespectToParameters(p, jacobian);

  if( jacobian.rows() != 3 || jacobian.columns() != 0 )
    {
    std::cerr << "Error with Jacobian: " << jacobian << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << " [ PASSED ] " << std::endl;
    }

  IdentityTransformType::Pointer inv = IdentityTransformType::New();
  TEST_EXPECT_TRUE(transform->GetInverse(inv.GetPointer()));
  TEST_EXPECT_TRUE(!transform->GetInverse(ITK_NULLPTR));

  return EXIT_SUCCESS;

}

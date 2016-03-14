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

#include "itkQuaternionRigidTransform.h"

int itkQuaternionRigidTransformTest(int, char * [] )
{

  typedef double                                        CoordinateType;
  typedef itk::QuaternionRigidTransform<CoordinateType> TransformType;

  const double       epsilon = 1e-10;
  const unsigned int N = 3;

  bool Ok = true;

  /* Create a 3D identity transformation and show its parameters */
    {
    TransformType::Pointer    identityTransform = TransformType::New();
    TransformType::OffsetType offset = identityTransform->GetOffset();
    std::cout << "Vector from instantiating an identity transform:  ";
    std::cout << offset << std::endl;
    for( unsigned int i = 0; i < N; i++ )
      {
      if( std::fabs( offset[i] - 0.0 ) > epsilon )
        {
        Ok = false;
        break;
        }
      }
    if( !Ok )
      {
      std::cerr << "Identity doesn't have a null offset" << std::endl;
      return EXIT_FAILURE;
      }
    }

  /* Create a Rigid 3D transform with translation */
    {
    TransformType::Pointer          translation = TransformType::New();
    TransformType::OutputVectorType itransVector;
    itransVector[0] = 1;
    itransVector[1] = 4;
    itransVector[2] = 9;

    translation->SetTranslation( itransVector );

    std::cout << "translation: " << translation;

    TransformType::OutputVectorType translationVector = translation->GetTranslation();
    std::cout << "pure Translation test:  ";
    std::cout << translationVector << std::endl;
    for( unsigned int i = 0; i < N; i++ )
      {
      if( std::fabs( translationVector[i] - itransVector[i] ) > epsilon )
        {
        Ok = false;
        break;
        }
      }
    if( !Ok )
      {
      std::cerr << "GetTranslation differs from SetTranslation value " << std::endl;
      return EXIT_FAILURE;
      }

      {
      // Translate an itk::Point
      TransformType::InputPointType::ValueType pInit[3] = {10, 10, 10};
      TransformType::InputPointType            p = pInit;
      TransformType::InputPointType            q;
      q = p + itransVector;
      TransformType::OutputPointType r;
      r = translation->TransformPoint( p );
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
        std::cout << "Ok translating an itk::Point " << std::endl;
        }
      }

      {
      // Translate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[3] = {10, 10, 10};
      TransformType::InputVectorType            p = pInit;
      TransformType::OutputVectorType           q;
      q = translation->TransformVector( p );
      for( unsigned int i = 0; i < N; i++ )
        {
        if( std::fabs( q[i] - p[i] ) > epsilon )
          {
          Ok = false;
          break;
          }
        }
      if( !Ok )
        {
        std::cerr << "Error translating vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok translating an itk::Vector " << std::endl;
        }
      }

      {
      // Translate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[3] = {10, 10, 10};
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q;
      q = translation->TransformCovariantVector( p );
      for( unsigned int i = 0; i < N; i++ )
        {
        if( std::fabs( q[i] - p[i] ) > epsilon )
          {
          Ok = false;
          break;
          }
        }
      if( !Ok )
        {
        std::cerr << "Error translating covariant vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok translating an itk::CovariantVector " << std::endl;
        }
      }

      {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] =  7;
      p[2] = 15;
      TransformType::OutputVnlVectorType q;
      q = translation->TransformVector( p );
      for( unsigned int i = 0; i < N; i++ )
        {
        if( std::fabs( q[i] - p[i] ) > epsilon )
          {
          Ok = false;
          break;
          }
        }
      if( !Ok )
        {
        std::cerr << "Error translating vnl_vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok translating an vnl_Vector " << std::endl;
        }
      }

    }

  /* Create a Rigid 3D transform with a rotation given by a Matrix */
    {
    TransformType::Pointer           rotation = TransformType::New();
    TransformType::VnlQuaternionType qrotation;

    // 15 degrees in radians
    const double angle = 15.0 * std::atan( 1.0f ) / 45.0;
    const double sinth2 = std::sin( angle / 2.0 );
    const double costh2 = std::cos( angle / 2.0 );

    const double sinth  = std::sin( angle );
    const double costh  = std::cos( angle );

    // around the positive Z axis
    qrotation[0] =     0.0;
    qrotation[1] =     0.0;
    qrotation[2] =  sinth2;
    qrotation[3] =  costh2;

    rotation->SetIdentity();
    rotation->SetRotation( qrotation );

    TransformType::OffsetType ioffset;
    ioffset.Fill( 0.0f );

    // rotation->ComputeOffset();

    std::cout << "rotation: " << rotation;

    // Verify the Offset content
    TransformType::OffsetType offset = rotation->GetOffset();
    std::cout << "pure Rotation test:  " << std::endl;
    std::cout << "Offset = " << offset << std::endl;
    for( unsigned int i = 0; i < N; i++ )
      {
      if( std::fabs( offset[i] - ioffset[i] ) > epsilon )
        {
        Ok = false;
        break;
        }
      }
    if( !Ok )
      {
      std::cerr << "Get Offset  differs from SetOffset value " << std::endl;
      return EXIT_FAILURE;
      }

    // VNL uses transposed matrices.
    vnl_matrix_fixed<double, 3, 3> mrotation = qrotation.rotation_matrix_transpose();

    // Verify the Matrix content
    TransformType::MatrixType matrix = rotation->GetMatrix();
    std::cout << "Rotation matrix:  " << std::endl;
    std::cout << matrix << std::endl;
    for( unsigned int i = 0; i < N; i++ )
      {
      for( unsigned int j = 0; j < N; j++ )
        {
        if( std::fabs( matrix[i][j] - mrotation[j][i] ) > epsilon )
          {
          Ok = false;
          break;
          }
        }
      }
    if( !Ok )
      {
      std::cerr << "Get Rotation Matrix  differs " << std::endl;
      std::cerr << "from SetMatrix value " << std::endl;
      return EXIT_FAILURE;
      }

      {
      // Rotate an itk::Point
      TransformType::InputPointType::ValueType pInit[3] = {10, 10, 10};
      TransformType::InputPointType            p = pInit;
      TransformType::InputPointType            q;

      q[0] =  p[0] * costh - p[1] * sinth;
      q[1] =  p[0] * sinth + p[1] * costh;
      q[2] =  p[2];

      TransformType::OutputPointType r;
      r = rotation->TransformPoint( p );
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
        std::cout << "Ok translating an itk::Point " << std::endl;
        }
      }

      {
      // Rotate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[3] = {10, 10, 10};
      TransformType::InputVectorType            p = pInit;

      TransformType::InputPointType q;
      q[0] =  p[0] * costh - p[1] * sinth;
      q[1] =  p[0] * sinth + p[1] * costh;
      q[2] =  p[2];

      TransformType::OutputVectorType r;
      r = rotation->TransformVector( p );
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
        std::cerr << "Error rotating vector  : " << p << std::endl;
        std::cerr << "Result should be       : " << q << std::endl;
        std::cerr << "Reported Result is     : " << r << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok rotating an itk::Vector " << std::endl;
        }
      }

      {
      // Rotate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[3] = {10, 10, 10};
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q;

      q[0] =  p[0] * costh - p[1] * sinth;
      q[1] =  p[0] * sinth + p[1] * costh;
      q[2] =  p[2];

      TransformType::OutputCovariantVectorType r;
      r = rotation->TransformCovariantVector( p );
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
        std::cerr << "Error Rotating covariant vector: " << p << std::endl;
        std::cerr << "Result should be               : " << q << std::endl;
        std::cerr << "Reported Result is             : " << r << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok translating an itk::CovariantVector " << std::endl;
        }
      }

      {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] =  7;
      p[2] = 15;

      TransformType::OutputVnlVectorType q;

      q[0] =  p[0] * costh - p[1] * sinth;
      q[1] =  p[0] * sinth + p[1] * costh;
      q[2] =  p[2];

      TransformType::OutputVnlVectorType r;
      r = rotation->TransformVector( p );
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
        std::cerr << "Error translating vnl_vector : " << p << std::endl;
        std::cerr << "Result should be             : " << q << std::endl;
        std::cerr << "Reported Result is           : " << r << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok translating an vnl_Vector " << std::endl;
        }
      }

    }

    {
    // Test the Jacobian
    std::cout << "Testing ComputeJacobianWithRespectToParameters()" << std::endl;

    TransformType::Pointer        quaternionRigid = TransformType::New();
    TransformType::ParametersType parameters( quaternionRigid->GetNumberOfParameters() );

    parameters.Fill( 0.0 );

    double angle = 0.62 / 180.0 * itk::Math::pi;

    parameters[0] =  2.0 * std::sin( 0.5 * angle );
    parameters[1] =  5.0 * std::sin( 0.5 * angle );
    parameters[2] = -4.0 * std::sin( 0.5 * angle );
    parameters[3] =        std::cos( 0.5 * angle );
    parameters[4] = 6.0;
    parameters[5] = 8.0;
    parameters[6] = 10.0;

    quaternionRigid->SetParameters( parameters );

    TransformType::InputPointType pInit;
    pInit[0] = 1.0;
    pInit[1] = 1.5;
    pInit[2] = 2.6;

    TransformType::JacobianType jacobian;
    quaternionRigid->ComputeJacobianWithRespectToParameters( pInit, jacobian );
    std::cout << jacobian << std::endl;

    TransformType::JacobianType approxJacobian = jacobian;
    for( unsigned int k = 0; k < quaternionRigid->GetNumberOfParameters(); k++ )
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

      quaternionRigid->SetParameters( plusParameters );
      plusPoint = quaternionRigid->TransformPoint( pInit );
      quaternionRigid->SetParameters( minusParameters );
      minusPoint = quaternionRigid->TransformPoint( pInit );
      for( unsigned int j = 0; j < 3; j++ )
        {
        double approxDerivative = ( plusPoint[j] - minusPoint[j] ) / ( 2.0 * delta );
        double computedDerivative = jacobian[j][k];
        approxJacobian[j][k] = approxDerivative;
        if( itk::Math::abs( approxDerivative - computedDerivative ) > 1e-5 )
          {
          std::cerr << "Error computing Jacobian [" << j << "][" << k << "]" << std::endl;
          std::cerr << "Result should be: " << approxDerivative << std::endl;
          std::cerr << "Reported result is: " << computedDerivative << std::endl;
          std::cerr << " [ FAILED ] " << std::endl;
          return EXIT_FAILURE;
          } // if
        }   // for j

      } // for k

    std::cout << approxJacobian << std::endl;
    std::cout << " [ PASSED ] " << std::endl;

    // Testing inverse transform
    std::cout << "Testing BackTransform()" << std::endl;
    TransformType::OutputPointType pOut;
    quaternionRigid->SetParameters( parameters );
      {
      TransformType::Pointer inverseQuaternionRigid = TransformType::New();
      const bool inverseIsValid=quaternionRigid->GetInverse(inverseQuaternionRigid);
      if( ! inverseIsValid )
        {
        std::cerr << "Error computing inverse transform" << std::endl;
        std::cerr << " [ FAILED ] " << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << " [ PASSED ] compute inverse transform" << std::endl;
        }
      pOut = inverseQuaternionRigid->TransformPoint( quaternionRigid->TransformPoint( pInit ) );
      }
    // pOut should equate pInit
    for( unsigned int j = 0; j < 3; j++ )
      {
      if( itk::Math::abs( pOut[j] - pInit[j] ) > 1e-5 )
        {
        std::cerr << "Error computing back transform" << std::endl;
        std::cerr << "Result should be: " << pInit << std::endl;
        std::cerr << "Reported result is: " << pOut << std::endl;
        std::cerr << " [ FAILED ] " << std::endl;
        return EXIT_FAILURE;
        }
      }

    std::cout << " [ PASSED ] " << std::endl;

    }

  /* Create a Rigid 3D transform with a defined center and a rotation given by a Matrix */
    {
    TransformType::Pointer           rotation = TransformType::New();
    TransformType::VnlQuaternionType qrotation;

    // 15 degrees in radians
    const double angle = 15.0 * std::atan( 1.0f ) / 45.0;
    const double sinth2 = std::sin( angle / 2.0 );
    const double costh2 = std::cos( angle / 2.0 );

    const double sinth  = std::sin( angle );
    const double costh  = std::cos( angle );

    // around the positive Z axis
    qrotation[0] =     0.0;
    qrotation[1] =     0.0;
    qrotation[2] =  sinth2;
    qrotation[3] =  costh2;

    rotation->SetIdentity();

    rotation->SetRotation( qrotation );

    TransformType::InputPointType center;
    center[0] = 17;
    center[1] = 19;
    center[2] = 23;

    TransformType::OutputVectorType itranslation;
    itranslation[0] = 13;
    itranslation[1] = 17;
    itranslation[2] = 19;

    rotation->SetTranslation( itranslation );
    rotation->SetCenter( center );

    // rotation->ComputeOffset();

    std::cout << "rotation: " << rotation;

    // Verify the Offset content
    TransformType::OffsetType offset = rotation->GetOffset();
    std::cout << "pure Rotation test:  " << std::endl;
    std::cout << "Offset = " << offset << std::endl;

    TransformType::OffsetType ioffset;

    ioffset[0]  = center[0] + itranslation[0];
    ioffset[1]  = center[1] + itranslation[1];
    ioffset[2]  = center[2] + itranslation[2];

    ioffset[0] -= costh * center[0] - sinth * center[1];
    ioffset[1] -= sinth * center[0] + costh * center[1];
    ioffset[2] -= center[2];

    std::cout << "iOffset = " << ioffset << std::endl;
    for( unsigned int i = 0; i < N; i++ )
      {
      if( std::fabs( offset[i] - ioffset[i] ) > epsilon )
        {
        Ok = false;
        break;
        }
      }
    if( !Ok )
      {
      std::cerr << "Get Offset  differs from SetOffset value " << std::endl;
      return EXIT_FAILURE;
      }

    // VNL uses transposed matrices.
    vnl_matrix_fixed<double, 3, 3> mrotation = qrotation.rotation_matrix_transpose();

    // Verify the Matrix content
    TransformType::MatrixType matrix = rotation->GetMatrix();
    std::cout << "Rotation matrix:  " << std::endl;
    std::cout << matrix << std::endl;
    for( unsigned int i = 0; i < N; i++ )
      {
      for( unsigned int j = 0; j < N; j++ )
        {
        if( std::fabs( matrix[i][j] - mrotation[j][i] ) > epsilon )
          {
          Ok = false;
          break;
          }
        }
      }
    if( !Ok )
      {
      std::cerr << "Get Rotation Matrix  differs " << std::endl;
      std::cerr << "from SetMatrix value " << std::endl;
      return EXIT_FAILURE;
      }

      {
      // Rotate an itk::Point
      TransformType::InputPointType::ValueType pInit[3] = {10, 10, 10};
      TransformType::InputPointType            p = pInit;
      TransformType::InputPointType            q;

      const CoordinateType x = p[0] - center[0];
      const CoordinateType y = p[1] - center[1];
      const CoordinateType z = p[2] - center[2];

      q[0] =  x * costh - y * sinth + center[0] + itranslation[0];
      q[1] =  x * sinth + y * costh + center[1] + itranslation[1];
      q[2] =  z                     + center[2] + itranslation[2];

      TransformType::OutputPointType r;
      r = rotation->TransformPoint( p );
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
        std::cout << "Ok translating an itk::Point " << std::endl;
        }
      }

      {
      // Rotate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[3] = {10, 10, 10};
      TransformType::InputVectorType            p = pInit;

      TransformType::OutputVectorType q;
      q[0] =  p[0] * costh - p[1] * sinth;
      q[1] =  p[0] * sinth + p[1] * costh;
      q[2] =  p[2];

      TransformType::OutputVectorType r;
      r = rotation->TransformVector( p );
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
        std::cerr << "Error rotating vector  : " << p << std::endl;
        std::cerr << "Result should be       : " << q << std::endl;
        std::cerr << "Reported Result is     : " << r << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok rotating an itk::Vector " << std::endl;
        }
      }

      {
      // Rotate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[3] = {10, 10, 10};
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q;

      q[0] =  p[0] * costh - p[1] * sinth;
      q[1] =  p[0] * sinth + p[1] * costh;
      q[2] =  p[2];

      TransformType::OutputCovariantVectorType r;
      r = rotation->TransformCovariantVector( p );
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
        std::cerr << "Error Rotating covariant vector: " << p << std::endl;
        std::cerr << "Result should be               : " << q << std::endl;
        std::cerr << "Reported Result is             : " << r << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok translating an itk::CovariantVector " << std::endl;
        }
      }

      {
      // Rotate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] =  7;
      p[2] = 15;

      TransformType::OutputVnlVectorType q;

      q[0] =  p[0] * costh - p[1] * sinth;
      q[1] =  p[0] * sinth + p[1] * costh;
      q[2] =  p[2];

      TransformType::OutputVnlVectorType r;
      r = rotation->TransformVector( p );
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
        std::cerr << "Error translating vnl_vector : " << p << std::endl;
        std::cerr << "Result should be             : " << q << std::endl;
        std::cerr << "Reported Result is           : " << r << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok translating an vnl_Vector " << std::endl;
        }
      }

    }

  std::cout << std::endl;
  std::cout << "Test PASSED !" << std::endl;
  std::cout << std::endl;

    {
    // Testing SetMatrix()
    std::cout << "Testing SetMatrix() ... ";
    unsigned int par;

    typedef TransformType::MatrixType MatrixType;
    MatrixType matrix;

    TransformType::Pointer t = TransformType::New();

    // attempt to set an non-orthogonal matrix
    par = 0;
    for( unsigned int row = 0; row < 3; row++ )
      {
      for( unsigned int col = 0; col < 3; col++ )
        {
        matrix[row][col] = static_cast<double>( par + 1 );
        ++par;
        }
      }

    Ok = false;
    try
      {
      t->SetMatrix( matrix );
      }
    catch( itk::ExceptionObject & itkNotUsed(err) )
      {
      Ok = true;
      }
    catch( ... )
      {
      std::cout << "Caught unknown exception" << std::endl;
      }

    if( !Ok )
      {
      std::cerr << "Error: expected to catch an exception when attempting";
      std::cerr << " to set an non-orthogonal matrix." << std::endl;
      return EXIT_FAILURE;
      }

    t = TransformType::New();

    // attempt to set an orthogonal matrix
    matrix.GetVnlMatrix().set_identity();

    double a = 1.0 / 180.0 * itk::Math::pi;
    matrix[0][0] =        std::cos( a );
    matrix[0][1] = -1.0 * std::sin( a );
    matrix[1][0] =        std::sin( a );
    matrix[1][1] =        std::cos( a );

    Ok = true;
    try
      {
      t->SetMatrix( matrix );
      }
    catch( itk::ExceptionObject & err )
      {
      std::cout << err << std::endl;
      Ok = false;
      }
    catch( ... )
      {
      std::cout << "Caught unknown exception" << std::endl;
      Ok = false;
      }

    if( !Ok )
      {
      std::cerr << "Error: caught unexpected exception" << std::endl;
      return EXIT_FAILURE;
      }

    // Check the computed parameters

    typedef TransformType::ParametersType ParametersType;
    ParametersType e( t->GetNumberOfParameters() );
    e.Fill( 0.0 );
    e[2] = std::sin(0.5 * a);
    e[3] = std::cos(0.5 * a );

    t = TransformType::New();
    t->SetParameters( e );

    TransformType::Pointer t2 = TransformType::New();
    t2->SetMatrix( t->GetMatrix() );

    ParametersType p = t2->GetParameters();
    for( unsigned int k = 0; k < e.GetSize(); k++ )
      {
      if( std::fabs( e[k] - p[k] ) > epsilon )
        {
        std::cout << " [ FAILED ] " << std::endl;
        std::cout << "Expected parameters: " << e << std::endl;
        std::cout << "but got: " << p << std::endl;
        return EXIT_FAILURE;
        }
      }

    std::cout << "[ PASSED ]" << std::endl;
    }

  return EXIT_SUCCESS;

}

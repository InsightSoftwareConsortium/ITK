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

/**
 *
 *  This program illustrates the use of VersorsRigid3DTransform
 *
 *  Versors are Unit Quaternions used to represent rotations.
 *  VersorRigid3DTransform is a Rigid 3D Transform that support
 *  Versors and Vectors in its interface.
 *
 */

#include "itkVersorRigid3DTransform.h"
#include <iostream>

// -------------------------
//
//   Main code
//
// -------------------------
int itkVersorRigid3DTransformTest(int, char * [] )
{

  typedef   double ValueType;

  const ValueType epsilon = 1e-12;

  //  Versor Transform type
  typedef    itk::VersorRigid3DTransform<ValueType> TransformType;

  //  Versor type
  typedef    TransformType::VersorType VersorType;

  //  Vector type
  typedef    TransformType::InputVectorType VectorType;

  //  Parameters type
  typedef    TransformType::ParametersType ParametersType;

  //  Jacobian type
  typedef    TransformType::JacobianType JacobianType;

  //  Rotation Matrix type
  typedef    TransformType::MatrixType MatrixType;

    {
    std::cout << "Test default constructor... ";

    TransformType::Pointer transform = TransformType::New();

    VectorType axis(1.5);

    ValueType angle = 120.0 * std::atan(1.0) / 45.0;

    VersorType versor;
    versor.Set( axis, angle );

    ParametersType parameters( transform->GetNumberOfParameters() ); // Number of parameters

    parameters[0] = versor.GetX();
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = 0.0;             // Translation
    parameters[4] = 0.0;
    parameters[5] = 0.0;

    transform->SetParameters( parameters );

    if( 0.0 > epsilon )
      {
      std::cout << "Error ! " << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << " PASSED !" << std::endl;

    }

    {
    std::cout << "Test initial rotation matrix " << std::endl;
    TransformType::Pointer transform = TransformType::New();
    MatrixType             matrix = transform->GetMatrix();
    std::cout << "Matrix = " << std::endl;
    std::cout <<    matrix   << std::endl;
    }

  /* Create a Rigid 3D transform with rotation */

    {
    bool Ok = true;

    TransformType::Pointer rotation = TransformType::New();

    itk::Vector<double, 3> axis(1);

    const double angle = (std::atan(1.0) / 45.0) * 120.0; // turn 120 degrees

    // this rotation will permute the axis x->y, y->z, z->x
    rotation->SetRotation( axis, angle );

    TransformType::OffsetType offset = rotation->GetOffset();
    std::cout << "pure Rotation test:  ";
    std::cout << offset << std::endl;
    for( unsigned int i = 0; i < 3; i++ )
      {
      if( std::fabs( offset[i] - 0.0 ) > epsilon )
        {
        Ok = false;
        break;
        }
      }

    if( !Ok )
      {
      std::cerr << "Get Offset  differs from null in rotation " << std::endl;
      return EXIT_FAILURE;
      }

    VersorType versor;
    versor.Set( axis, angle );

      {
      // Rotate an itk::Point
      TransformType::InputPointType::ValueType pInit[3] = {1, 4, 9};
      TransformType::InputPointType            p = pInit;
      TransformType::OutputPointType           q;
      q = versor.Transform( p );

      TransformType::OutputPointType r;
      r = rotation->TransformPoint( p );
      for( unsigned int i = 0; i < 3; i++ )
        {
        if( std::fabs( q[i] - r[i] ) > epsilon )
          {
          Ok = false;
          break;
          }
        }
      if( !Ok )
        {
        std::cerr << "Error rotating point : " << p << std::endl;
        std::cerr << "Result should be     : " << q << std::endl;
        std::cerr << "Reported Result is   : " << r << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok rotating an itk::Point " << std::endl;
        }
      }

      {
      // Translate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[3] = {1, 4, 9};
      TransformType::InputVectorType            p = pInit;
      TransformType::OutputVectorType           q;
      q = versor.Transform( p );

      TransformType::OutputVectorType r;
      r = rotation->TransformVector( p );
      for( unsigned int i = 0; i < 3; i++ )
        {
        if( std::fabs( q[i] - r[i] ) > epsilon )
          {
          Ok = false;
          break;
          }
        }
      if( !Ok )
        {
        std::cerr << "Error rotating vector : " << p << std::endl;
        std::cerr << "Result should be      : " << q << std::endl;
        std::cerr << "Reported Result is    : " << r << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok rotating an itk::Vector " << std::endl;
        }
      }

      {
      // Translate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[3] = {1, 4, 9};
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q;
      q = versor.Transform( p );

      TransformType::OutputCovariantVectorType r;
      r = rotation->TransformCovariantVector( p );
      for( unsigned int i = 0; i < 3; i++ )
        {
        if( std::fabs( q[i] - r[i] ) > epsilon )
          {
          Ok = false;
          break;
          }
        }
      if( !Ok )
        {
        std::cerr << "Error rotating covariant vector : " << p << std::endl;
        std::cerr << "Result should be                : " << q << std::endl;
        std::cerr << "Reported Result is              : " << r << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok rotating an itk::CovariantVector " << std::endl;
        }
      }

      {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 1;
      p[1] = 4;
      p[2] = 9;

      TransformType::OutputVnlVectorType q;
      q = versor.Transform( p );

      TransformType::OutputVnlVectorType r;
      r = rotation->TransformVector( p );
      for( unsigned int i = 0; i < 3; i++ )
        {
        if( std::fabs( q[i] - r[i] ) > epsilon )
          {
          Ok = false;
          break;
          }
        }
      if( !Ok )
        {
        std::cerr << "Error rotating vnl_vector : " << p << std::endl;
        std::cerr << "Result should be          : " << q << std::endl;
        std::cerr << "Reported Result is        : " << r << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cout << "Ok rotating an vnl_Vector " << std::endl;
        }
      }

    }

  /**  Exercise the SetCenter method  */
    {
    bool Ok = true;

    TransformType::Pointer transform = TransformType::New();

    itk::Vector<double, 3> axis(1);

    const double angle = (std::atan(1.0) / 45.0) * 30.0; // turn 30 degrees

    transform->SetRotation( axis, angle );

    TransformType::InputPointType center;
    center[0] = 31;
    center[1] = 62;
    center[2] = 93;

    transform->SetCenter( center );

    TransformType::OutputPointType transformedPoint;
    transformedPoint = transform->TransformPoint( center );
    for( unsigned int i = 0; i < 3; i++ )
      {
      if( std::fabs( center[i] - transformedPoint[i] ) > epsilon )
        {
        Ok = false;
        break;
        }
      }

    if( !Ok )
      {
      std::cerr << "The center point was not invariant to rotation " << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "Ok center is invariant to rotation." << std::endl;
      }

    const unsigned int np = transform->GetNumberOfParameters();

    ParametersType parameters( np ); // Number of parameters

    VersorType versor;

    parameters[0] = versor.GetX();   // Rotation axis * std::sin(t/2)
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = 8.0;             // Translation
    parameters[4] = 7.0;
    parameters[5] = 6.0;

    transform->SetParameters( parameters );

    ParametersType parameters2 = transform->GetParameters();

    const double tolerance = 1e-8;
    for( unsigned int p = 0; p < np; p++ )
      {
      if( std::fabs( parameters[p] - parameters2[p] ) > tolerance )
        {
        std::cerr << "Output parameter does not match input " << std::endl;
        return EXIT_FAILURE;
        }
      }
    std::cout << "Input/Output parameter check Passed !"  << std::endl;

    // Try the ComputeJacobianWithRespectToParameters method
    TransformType::InputPointType aPoint;
    aPoint[0] = 10.0;
    aPoint[1] = 20.0;
    aPoint[2] = -10.0;
    JacobianType jacobian;
    transform->ComputeJacobianWithRespectToParameters( aPoint, jacobian );
    std::cout << "Jacobian: "  << std::endl;
    std::cout << jacobian << std::endl;

    // copy the read one just for getting the right matrix size
    JacobianType TheoreticalJacobian = jacobian;

    TheoreticalJacobian[0][0] =    0.0;
    TheoreticalJacobian[1][0] =  206.0;
    TheoreticalJacobian[2][0] =  -84.0;

    TheoreticalJacobian[0][1] = -206.0;
    TheoreticalJacobian[1][1] =    0.0;
    TheoreticalJacobian[2][1] =   42.0;

    TheoreticalJacobian[0][2] =   84.0;
    TheoreticalJacobian[1][2] =  -42.0;
    TheoreticalJacobian[2][2] =    0.0;

    TheoreticalJacobian[0][3] = 1.0;
    TheoreticalJacobian[1][3] = 0.0;
    TheoreticalJacobian[2][3] = 0.0;

    TheoreticalJacobian[0][4] = 0.0;
    TheoreticalJacobian[1][4] = 1.0;
    TheoreticalJacobian[2][4] = 0.0;

    TheoreticalJacobian[0][5] = 0.0;
    TheoreticalJacobian[1][5] = 0.0;
    TheoreticalJacobian[2][5] = 1.0;
    for( unsigned int ii = 0; ii < 3; ii++ )
      {
      for( unsigned int jj = 0; jj < 6; jj++ )
        {
        if( itk::Math::abs( TheoreticalJacobian[ii][jj] - jacobian[ii][jj] ) > 1e-5 )
          {
          std::cerr << "Jacobian components differ from expected values ";
          std::cerr << std::endl << std::endl;
          std::cerr << "Expected Jacobian = " << std::endl;
          std::cerr << TheoreticalJacobian << std::endl << std::endl;
          std::cerr << "Computed Jacobian = " << std::endl;
          std::cerr << jacobian << std::endl << std::endl;
          std::cerr << std::endl << "Test FAILED ! " << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    }

    {
    std::cout << " Exercise the SetIdentity() method " << std::endl;
    TransformType::Pointer transform = TransformType::New();

    itk::Vector<double, 3> axis(1);

    const double angle = (std::atan(1.0) / 45.0) * 30.0; // turn 30 degrees

    transform->SetRotation( axis, angle );

    TransformType::InputPointType center;
    center[0] = 31;
    center[1] = 62;
    center[2] = 93;

    transform->SetCenter( center );

    transform->SetIdentity();

    const unsigned int np = transform->GetNumberOfParameters();

    ParametersType parameters( np ); // Number of parameters

    VersorType versor;

    parameters[0] = versor.GetX(); // Rotation axis * std::sin(t/2)
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = 0.0;           // Translation
    parameters[4] = 0.0;
    parameters[5] = 0.0;

    ParametersType parameters2 = transform->GetParameters();

    const double tolerance = 1e-8;
    for( unsigned int p = 0; p < np; p++ )
      {
      if( std::fabs( parameters[p] - parameters2[p] ) > tolerance )
        {
        std::cerr << "Output parameter does not match input " << std::endl;
        return EXIT_FAILURE;
        }
      }
    std::cout << "Input/Output parameter check Passed !"  << std::endl;
    }

    {
    // Testing SetMatrix()
    std::cout << "Testing SetMatrix() ... ";
    unsigned int par;
    bool         Ok;

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

    ParametersType e( t->GetNumberOfParameters() );
    e.Fill( 0.0 );
    e[2] = std::sin(0.5 * a);

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
      {
      TransformType::Pointer tInverse = TransformType::New();
      if(!t->GetInverse(tInverse))
        {
        std::cout << "Cannot create inverse transform" << std::endl;
        return EXIT_FAILURE;
        }
      std::cout << "translation: " << t;
      std::cout << "translationInverse: " << tInverse;
      }
    std::cout << "[ PASSED ]" << std::endl;
    }

  std::cout << std::endl << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}

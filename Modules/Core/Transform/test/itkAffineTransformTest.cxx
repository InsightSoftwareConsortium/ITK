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

#include "itkMath.h"
#include "itkAffineTransform.h"
#include "itkStdStreamStateSave.h"

typedef  itk::Matrix<double, 2, 2> Matrix2Type;
typedef  itk::Vector<double, 2>    Vector2Type;

namespace
{

void PrintVector( const Vector2Type & v )
{
  for( unsigned int i = 0; i < Vector2Type::Dimension; i++ )
    {
    std::cout << v[i] << ", ";
    }
  std::cout << std::endl;
}

bool testValue( const double v1, const double v2, int maxUlps=4 )
{
  return itk::Math::FloatAlmostEqual( v1, v2, maxUlps );
}

template <typename TMatrix>
bool testMatrix( const TMatrix & m1, const TMatrix & m2, int maxUlps=4 )
{
  bool pass = true;

  for( unsigned int  i = 0; i < TMatrix::RowDimensions; i++ )
    {
    for( unsigned int j = 0; j < TMatrix::ColumnDimensions; j++ )
      {
      if( !testValue( m1[i][j], m2[i][j], maxUlps ) )
        {
        pass = false;
        }
      }
    }
  return pass;
}

template <typename TVector>
bool testVector( const TVector & v1, const TVector & v2, int maxUlps=4 )
{
  bool pass = true;

  for( unsigned int i = 0; i < TVector::Dimension; i++ )
    {
    if( !testValue( v1[i], v2[i], maxUlps ) )
      {
      pass = false;
      }
    }
  return pass;
}

template <typename TVector>
bool testVariableVector( const TVector & v1, const TVector & v2, int maxUlps=4 )
{
  bool               pass = true;
  const unsigned int D1 = v1.Size();
  const unsigned int D2 = v2.Size();

  if( D1 != D2 )
    {
    return false;
    }
  for( unsigned int i = 0; i < D1; i++ )
    {
    if( !testValue( v1[i], v2[i], maxUlps ) )
      {
      pass = false;
      }
    }
  return pass;
}

} // namespace

int itkAffineTransformTest(int, char *[])
{
  /* NOTE: The truth values for tests were taken from the output
      of the tests themselves. The assumption is that this code
      has been well-tested by other means already. */

// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  /* Set outstream precision */
  std::cout.precision(20);
  std::cerr.precision(20);

  int any = 0;         // Any errors detected in testing?

  Matrix2Type matrix2, matrix2Truth;
  Matrix2Type inverse2;
  Vector2Type vector2, vector2Truth;

  /* Create a 2D identity transformation and show its parameters */
  typedef itk::AffineTransform<double, 2> Affine2DType;
  Affine2DType::Pointer id2 = Affine2DType::New();
  matrix2 = id2->GetMatrix();
  vector2 = id2->GetOffset();
  std::cout << "Matrix from instantiating an identity transform:"
            << std::endl << matrix2;
  std::cout << "Vector from instantiating an identity transform:"
            << std::endl;
  PrintVector( vector2 );

  /* Test identity transform against truth */
  matrix2Truth[0][0] = 1;
  matrix2Truth[0][1] = 0;
  matrix2Truth[1][0] = 0;
  matrix2Truth[1][1] = 1;
  vector2Truth[0] = 0;
  vector2Truth[1] = 0;

  if( !testMatrix( matrix2, matrix2Truth ) ||
      !testVector( vector2, vector2Truth ) )
    {
    std::cout << "Default identity transformation test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Create and show a simple 2D transform from given parameters */
  matrix2[0][0] = 1;
  matrix2[0][1] = 2;
  matrix2[1][0] = 3;
  matrix2[1][1] = 4;
  vector2[0] = 5;
  vector2[1] = 6;

  Affine2DType::Pointer aff2 = Affine2DType::New();
  aff2->SetMatrix( matrix2 );
  aff2->SetOffset( vector2 );
  for( unsigned int i = 0; i < 2; i++ )
    {
    for( unsigned int j = 0; j < 2; j++ )
      {
      matrix2[i][j] = 0.0;
      }
    vector2[i]    = 0.0;
    }
  std::cout << "Instantiation of a given 2D transform:" << std::endl;
  aff2->Print( std::cout );

  /* Get and test inverse of whole transform */
  Affine2DType::Pointer affInv2 = Affine2DType::New();
  if( !aff2->GetInverse(affInv2) )
    {
    std::cout << "Test transform does not have an inverse when expected."
              << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Inverse transform for the given transform:"
            << std::endl << affInv2;

  matrix2Truth[0][0] = -2;
  matrix2Truth[0][1] = 1;
  matrix2Truth[1][0] = 1.5;
  matrix2Truth[1][1] = -0.5;
  vector2Truth[0] = 4;
  vector2Truth[1] = -4.5;

  if( !testMatrix( affInv2->GetMatrix(), matrix2Truth, 7 ) ||
      !testVector( affInv2->GetOffset(), vector2Truth, 6 ) )
    {
    std::cout << "Inverse transform test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Set parameters of a 2D transform */
  matrix2[0][0] = 6;
  matrix2[0][1] = 5;
  matrix2[1][0] = 4;
  matrix2[1][1] = 3;
  vector2[0] = 2;
  vector2[1] = 1;
  aff2->SetMatrix(matrix2);
  aff2->SetOffset(vector2);
  for( unsigned int i = 0; i < 2; i++ )
    {
    for( unsigned int j = 0; j < 2; j++ )
      {
      matrix2[i][j] = 0.0;
      }
    vector2[i]    = 0.0;
    }
  matrix2 = aff2->GetMatrix();
  vector2 = aff2->GetOffset();
  std::cout << "Setting the matrix in an existing transform:"
            << std::endl << matrix2;
  std::cout << "Setting the offset in an existing  transform:"
            << std::endl;
  PrintVector( vector2 );

  /* Try composition of two transformations */
  aff2->Compose( aff2 );
  std::cout << "Result of a composition:" << std::endl;
  aff2->Print( std::cout );

  matrix2Truth[0][0] = 56;
  matrix2Truth[0][1] = 45;
  matrix2Truth[1][0] = 36;
  matrix2Truth[1][1] = 29;
  vector2Truth[0] = 19;
  vector2Truth[1] = 12;

  if( !testMatrix( aff2->GetMatrix(), matrix2Truth ) ||
      !testVector( aff2->GetOffset(), vector2Truth ) )
    {
    std::cout << "Composition of two transformations test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Compose with a translation */
  Vector2Type trans;
  trans[0] = 1;
  trans[1] = 2;
  aff2->Translate(trans);
  std::cout << "Result of a translation:" << std::endl;
  aff2->Print( std::cout );

  matrix2Truth[0][0] = 56;
  matrix2Truth[0][1] = 45;
  matrix2Truth[1][0] = 36;
  matrix2Truth[1][1] = 29;
  vector2Truth[0] = 20;
  vector2Truth[1] = 14;

  if( !testMatrix( aff2->GetMatrix(), matrix2Truth ) ||
      !testVector( aff2->GetOffset(), vector2Truth ) )
    {
    std::cout << "Composition with a translation test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Compose with an isotropic scaling */
  aff2->Scale(.3, 1);
  std::cout << "Result of isotropic scaling:" << std::endl;
  aff2->Print( std::cout );

  matrix2Truth[0][0] = 16.8;
  matrix2Truth[0][1] = 13.5;
  matrix2Truth[1][0] = 10.8;
  matrix2Truth[1][1] = 8.7;
  vector2Truth[0] = 20;
  vector2Truth[1] = 14;

  if( !testMatrix( aff2->GetMatrix(), matrix2Truth ) ||
      !testVector( aff2->GetOffset(), vector2Truth ) )
    {
    std::cout << "Composition with isotropic scaling test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Compose with an anisotropic scaling */
  Vector2Type scale;
  scale[0] = .3;
  scale[1] = .2;
  aff2->Scale(scale);
  std::cout << "Result of anisotropic scaling:" << std::endl;
  aff2->Print( std::cout );

  matrix2Truth[0][0] = 5.04;
  matrix2Truth[0][1] = 4.05;
  matrix2Truth[1][0] = 2.16;
  matrix2Truth[1][1] = 1.74;
  vector2Truth[0] = 6;
  vector2Truth[1] = 2.8;

  if( !testMatrix( aff2->GetMatrix(), matrix2Truth ) ||
      !testVector( aff2->GetOffset(), vector2Truth ) )
    {
    std::cout << "Composition with anisotropic scaling test failed."
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Compose with a general N-D rotation */
  aff2->Rotate(0, 1, 0.57, 1);
  std::cout << "Result of general rotation:" << std::endl;
  aff2->Print( std::cout );

  matrix2Truth[0][0] = 2.0576711174453;
  matrix2Truth[0][1] = 6.1294444750264;
  matrix2Truth[1][0] = 0.87954634155339;
  matrix2Truth[1][1] = 2.6305129220477;
  vector2Truth[0] = 6;
  vector2Truth[1] = 2.8;

  if( !testMatrix( aff2->GetMatrix(), matrix2Truth, 100 ) ||
      !testVector( aff2->GetOffset(), vector2Truth ) )
    {
    std::cout << "Composition with a general N-D rotation test failed."
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Compose with a 2-D rotation */
  aff2->Rotate(0, 1, -0.57, 1);
  std::cout << "Result of 2-D rotation:" << std::endl;
  aff2->Print( std::cout );

  matrix2Truth[0][0] = 5.04;
  matrix2Truth[0][1] = 4.05;
  matrix2Truth[1][0] = 2.16;
  matrix2Truth[1][1] = 1.74;
  vector2Truth[0] = 6;
  vector2Truth[1] = 2.8;

  if( !testMatrix( aff2->GetMatrix(), matrix2Truth ) ||
      !testVector( aff2->GetOffset(), vector2Truth ) )
    {
    std::cout << "Composition with a 2-D rotation test failed."
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Compose with a shear */
  aff2->Shear(1, 0, .2);
  std::cout << "Result of shear:" << std::endl;
  aff2->Print( std::cout );

  matrix2Truth[0][0] = 5.04;
  matrix2Truth[0][1] = 4.05;
  matrix2Truth[1][0] = 3.168;
  matrix2Truth[1][1] = 2.55;
  vector2Truth[0] = 6;
  vector2Truth[1] = 4;

  if( !testMatrix( aff2->GetMatrix(), matrix2Truth ) ||
      !testVector( aff2->GetOffset(), vector2Truth ) )
    {
    std::cout << "Composition with a shear test failed."
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Transform a point */
  itk::Point<double, 2> u2, v2, v2T;
  u2[0] = 3;
  u2[1] = 5;
  v2 = aff2->TransformPoint(u2);
  std::cout << "Transform a point:" << std::endl
            << v2[0] << " , " << v2[1] << std::endl;

  v2T[0] = 41.37;
  v2T[1] = 26.254;
  if( !testValue( v2[0], v2T[0] ) || !testValue( v2[1], v2T[1] ) )
    {
    std::cout << "Transform a point test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Back transform a point */
  // v2 = aff2->BackTransform(u2);
  // std::cout << "Back transform a point:" << std::endl
  // << v2[0] << " , " << v2[1] << std::endl;

  /* Transform a vnl_vector */
  vnl_vector_fixed<double, 2> x2, y2, y2T;
  x2[0] = 1;
  x2[1] = 2;
  y2 = aff2->TransformVector(x2);
  std::cout << "Transform a vnl_vector:" << std::endl
            << y2[0] << " , " << y2[1] << std::endl;

  y2T[0] = 13.14;
  y2T[1] = 8.268;
  if( !testValue( y2[0], y2T[0] ) || !testValue( y2[1], y2T[1] ) )
    {
    std::cout << "Transform a vnl_vector test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Back transform a vector */
  // y2 = aff2->BackTransform(x2);
  // std::cout << "Back transform a vnl_vector:" << std::endl
  // << y2[0] << " , " << y2[1] << std::endl;

  /* Transform a vector */
  itk::Vector<double, 2> u3, v3, v3T;
  u3[0] = 3;
  u3[1] = 5;
  v3 = aff2->TransformVector(u3);
  std::cout << "Transform a vector:" << std::endl
            << v3[0] << " , " << v3[1] << std::endl;

  v3T[0] = 35.37;
  v3T[1] = 22.254;
  if( !testVector( v3, v3T ) )
    {
    std::cout << "Transform a vector test failed." << std::endl;
    return EXIT_FAILURE;
    }

  v3 = aff2->TransformVector(u3, u2);
  std::cout << "Transform a vector with a point:" << std::endl
            << v3[0] << " , " << v3[1] << std::endl;

  v3T[0] = 35.37;
  v3T[1] = 22.254;
  if( !testVector( v3, v3T ) )
    {
    std::cout << "Transform a vector with a point test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Transform a variable length vector */
  itk::VariableLengthVector<double> l3, m3, m3T;
  l3.SetSize(2);
  m3T.SetSize(2);
  l3[0] = 3;
  l3[1] = 5;
  m3 = aff2->TransformVector(l3);
  std::cout << "Transform a variable length vector:" << std::endl
            << m3[0] << " , " << m3[1] << std::endl;

  m3T[0] = 35.37;
  m3T[1] = 22.254;
  if( !testVariableVector( m3, m3T ) )
    {
    std::cout << "Transform a variable length vector test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Back transform a vector */
  // v3 = aff2->BackTransform(u3);
  // std::cout << "Back transform a vector :" << std::endl
  // << v3[0] << " , " << v3[1] << std::endl;

  /* Transform a Covariant vector */
  itk::CovariantVector<double, 2> u4, v4, v4T;
  u4[0] = 3;
  u4[1] = 5;
  v4 = aff2->TransformCovariantVector(u4);
  std::cout << "Transform a Covariant vector:" << std::endl
            << v4[0] << " , " << v4[1] << std::endl;

  v4T[0] = -379.16666666679;
  v4T[1] = 604.16666666687;
  if( !testVector( v4, v4T, 4000 ) )
    {
    std::cout << "Transform a covariant vector test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Transform a variable length vector as covariant vector */
  itk::VariableLengthVector<double> l4, m4, m4T;
  l4.SetSize(2);
  m4T.SetSize(2);
  l4[0] = 3;
  l4[1] = 5;
  m4 = aff2->TransformCovariantVector(l4);
  std::cout << "Transform a variable length covariant vector:" << std::endl
            << m4[0] << " , " << m4[1] << std::endl;

  m4T[0] = -379.16666666679;
  m4T[1] = 604.16666666687;
  if( !testVariableVector( m4, m4T, 4000) )
    {
    std::cout << "Transform a variable length covariant vector test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Back transform a vector */
  // v4 = aff2->BackTransform(u4);
  // std::cout << "Back transform a vector :" << std::endl
  // << v4[0] << " , " << v4[1] << std::endl;

  typedef itk::AffineTransform<double, 3> Affine3DType;
  Affine3DType::MatrixType matrix3Truth;

  /* Create a 3D transform and rotate in 3D */
  Affine3DType::Pointer  aff3 = Affine3DType::New();
  itk::Vector<double, 3> axis;
  axis[0] = .707;
  axis[1] = .707;
  axis[2] = .707;
  aff3->Rotate3D(axis, 1.0, 1);
  std::cout << "Create and rotate a 3D transform:" << std::endl;
  aff3->Print( std::cout );

  matrix3Truth[0][0] = 0.69353487057876;
  matrix3Truth[0][1] = -0.33259093488348;
  matrix3Truth[0][2] = 0.63905606430472;
  matrix3Truth[1][0] = 0.63905606430472;
  matrix3Truth[1][1] = 0.69353487057876;
  matrix3Truth[1][2] = -0.33259093488348;
  matrix3Truth[2][0] = -0.33259093488348;
  matrix3Truth[2][1] = 0.63905606430472;
  matrix3Truth[2][2] = 0.69353487057876;

  if( !testMatrix( aff3->GetMatrix(), matrix3Truth, 30 ) )
    {
    std::cout << "3D transform rotation test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Generate inverse transform */
  Affine3DType::Pointer inv3 = Affine3DType::New();
  if( !aff3->GetInverse(inv3) )
    {
    std::cout << "Cannot compute inverse transformation" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Create an inverse transformation:" << std::endl;
  inv3->Print( std::cout );

  matrix3Truth[0][0] = 0.69353487057876;
  matrix3Truth[0][1] = 0.63905606430472;
  matrix3Truth[0][2] = -0.33259093488348;
  matrix3Truth[1][0] = -0.33259093488348;
  matrix3Truth[1][1] = 0.69353487057876;
  matrix3Truth[1][2] = 0.63905606430472;
  matrix3Truth[2][0] = 0.63905606430472;
  matrix3Truth[2][1] = -0.33259093488348;
  matrix3Truth[2][2] = 0.69353487057876;

  if( !testMatrix( inv3->GetMatrix(), matrix3Truth, 40 ) )
    {
    std::cout << "Compute inverse test failed." << std::endl;
    return EXIT_FAILURE;
    }

  Affine3DType::Pointer inv4 =
    dynamic_cast<Affine3DType *>(aff3->GetInverseTransform().GetPointer() );
  if( !inv4 )
    {
    std::cout << "Cannot compute inverse transformation inv4" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Create an inverse transformation:" << std::endl;
  inv4->Print( std::cout );

  if( !testMatrix( inv4->GetMatrix(), matrix3Truth, 35 ) )
    {
    std::cout << "Compute inverse test failed - inv4." << std::endl;
    return EXIT_FAILURE;
    }

  /* Test output of ComputeJacobianWithRespectToParameters */
  Affine3DType::Pointer jaff = Affine3DType::New();

  Affine3DType::InputPointType jpoint;
  jpoint[0] = 5.0;
  jpoint[1] = 10.0;
  jpoint[2] = 15.0;
  Affine3DType::JacobianType jaffJacobian;
  jaff->ComputeJacobianWithRespectToParameters( jpoint, jaffJacobian );

  std::cout << "ComputeJacobianWithRespectToParameters: " << std::endl;
  std::cout << jaffJacobian << std::endl;

  double data[] =
        {5, 10, 15, 0, 0, 0, 0, 0, 0, 1, 0, 0,
        0, 0, 0, 5, 10, 15, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 5, 10, 15, 0, 0, 1};
  vnl_matrix<double>         vnlData( data, 3, 12 );
  Affine3DType::JacobianType expectedJacobian(vnlData);
  for( unsigned int i = 0; i < 3; i++ )
    {
    for( unsigned int j = 0; j < 12; j++ )
      {
      if( !testValue( expectedJacobian[i][j], jaffJacobian[i][j] ) )
        {
        std::cout << "ComputeJacobianWithRespectToParameters test failed." << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  /* Test ComputeJacobianWithRespectToPosition. Should return Matrix. */
  Affine3DType::MatrixType jaffMatrix = jaff->GetMatrix();
  jaff->ComputeJacobianWithRespectToPosition( jpoint, jaffJacobian );
  for( unsigned int i = 0; i < Affine3DType::MatrixType::RowDimensions; i++ )
    {
    for( unsigned int j = 0;
         j < Affine3DType::MatrixType::ColumnDimensions; j++ )
      {
      if( !testValue( jaffJacobian[i][j], jaffMatrix[i][j] ) )
        {
        std::cout << "Failed ComputeJacobianWithRespectToPosition." << std::endl
                  << "jaffJacobian: " << jaffJacobian << std::endl
                  << "jaffMatrix: " << jaffMatrix << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  /* Test SetParameters */
  Affine3DType::Pointer        paff = Affine3DType::New();
  paff->Print( std::cout );
  Affine3DType::ParametersType parameters1( paff->GetNumberOfParameters() );
  Affine3DType::ParametersType fixed_parameters = paff->GetFixedParameters();
  const size_t fixed_params_size = fixed_parameters.Size();
  for(unsigned int q=0; q < fixed_params_size; ++q)
  {
   fixed_parameters[q] = 100.0+q;
  }
  paff->SetFixedParameters( fixed_parameters );

  /* set up a 3x3 magic square matrix */
  parameters1[0] = 8;
  parameters1[1] = 1;
  parameters1[2] = 6;
  parameters1[3] = 3;
  parameters1[4] = 5;
  parameters1[5] = 7;
  parameters1[6] = 4;
  parameters1[7] = 9;
  parameters1[8] = 2;

  parameters1[9] = 5;
  parameters1[10] = 5;
  parameters1[11] = 5;

  paff->SetParameters( parameters1 );
  paff->Print( std::cout );

  // TEST INVERSE OF INVERSE
  Affine3DType::Pointer        paff_inv = Affine3DType::New();
  paff->GetInverse(paff_inv);
  Affine3DType::Pointer        paff_inv_inv = Affine3DType::New();
  paff_inv->GetInverse(paff_inv_inv);

  std::cout << "TEST INVERSE" << std::endl;
  paff_inv->Print(std::cout);
  std::cout << "TEST INVERSE OF INVERSE" << std::endl;
  paff_inv_inv->Print(std::cout);

  bool found_inv_inv_descrepancies = false;
    {
    Affine3DType::ParametersType parameters1_inv_inv = paff_inv_inv->GetParameters();
    // Check that Inv(Inv(T)) ~= T
    double mag_error = 0;
    for( unsigned int q = 0; q < parameters1_inv_inv.size(); ++q)
      {
      const double v = ( parameters1[q] - parameters1_inv_inv[q]);
      mag_error += sqrt(v);
      }
    if(mag_error > 1e-4 )
      {
      std::cout << "ERROR: Moving Parameters do not match!" << std::endl;
      std::cout << parameters1 << std::endl;
      std::cout << parameters1_inv_inv << std::endl;
      found_inv_inv_descrepancies = true;
      }
    }
    {
    Affine3DType::ParametersType fixed_parameters_inv_inv = paff_inv_inv->GetFixedParameters();
    double mag_error = 0;
    for( unsigned int q = 0; q < fixed_parameters_inv_inv.size(); ++q)
      {
      const double v = ( fixed_parameters[q] - fixed_parameters_inv_inv[q]);
      mag_error += sqrt(v);
      }
    if(mag_error > 1e-4 )
      {
      std::cout << "ERROR: Fixed Parameters do not match!" << std::endl;
      std::cout << fixed_parameters << std::endl;
      std::cout << fixed_parameters_inv_inv << std::endl;
      found_inv_inv_descrepancies = true;
      }
    }
    if( found_inv_inv_descrepancies )
    {
      std::cout << "ERROR: Inverse of Inverse does not match original!" << std::endl;
      return EXIT_FAILURE;
    }

  Affine3DType::ParametersType parametersRead( paff->GetNumberOfParameters() );
  parametersRead = paff->GetParameters();
  for( unsigned int k = 0; k < paff->GetNumberOfParameters(); k++ )
    {
    if( !testValue( parameters1[k], parametersRead[k] ) )
      {
      std::cout << "SetParameters test failed." << std::endl;
      return EXIT_FAILURE;
      }
    }

  /* Test UpdateTransformParameters */
  Affine3DType::DerivativeType update( paff->GetNumberOfParameters() );
  Affine3DType::ParametersType updateTruth;
  updateTruth = parameters1;
  for( unsigned int i = 0; i < paff->GetNumberOfParameters(); i++ )
    {
    update[i] = i / 10.0;
    updateTruth[i] += update[i];
    }
  /* Update all the parameters, with default scaling factor of 1 */
  paff->UpdateTransformParameters( update );
  parametersRead = paff->GetParameters();
  for( unsigned int k = 0; k < paff->GetNumberOfParameters(); k++ )
    {
    if( itk::Math::NotAlmostEquals( updateTruth[k], parametersRead[k] ) )
      {
      std::cout << "UpdateTransformParameters 1 failed." << std::endl;
      std::cout << "updateTruth: " << std::endl
                << updateTruth << std::endl
                << "parametersRead: " << std::endl
                << parametersRead << std::endl;
      return EXIT_FAILURE;
      }
    }
  /* Update with a non-unit scaling factor */
  double factor = 0.5;
  for( unsigned int i = 0; i < paff->GetNumberOfParameters(); i++ )
    {
    update[i] = i;
    updateTruth[i] += update[i] * factor;
    }
  paff->UpdateTransformParameters( update, factor );
  parametersRead = paff->GetParameters();
  for( unsigned int k = 0; k < paff->GetNumberOfParameters(); k++ )
    {
    if( itk::Math::NotAlmostEquals( updateTruth[k], parametersRead[k] ) )
      {
      std::cout << "UpdateTransformParameters 2 failed." << std::endl;
      return EXIT_FAILURE;
      }
    }

  paff->SetIdentity();
  paff->Print( std::cout );

    {
    // Test SetParameters and GetInverse
    typedef itk::AffineTransform<double, 2> TransformType;
    TransformType::Pointer transform = TransformType::New();

    TransformType::ParametersType parameters2;
    TransformType::ParametersType expectedParameters;
    expectedParameters.SetSize( transform->GetNumberOfParameters() );

    // check the returned parameters

    // Test 1: SetIdentity
    transform->SetIdentity();
    parameters2 = transform->GetParameters();

    expectedParameters.Fill( 0.0 );
    expectedParameters[0] = 1.0;
    expectedParameters[3] = 1.0;
    for( unsigned int k = 0; k < transform->GetNumberOfParameters(); k++ )
      {
      if( ! testValue( parameters2[k], expectedParameters[k] ) )
        {
        std::cout << "Test failed:" << std::endl;
        std::cout << "Results=" << parameters2 << std::endl;
        std::cout << "Expected=" << expectedParameters << std::endl;
        any = true;
        break;
        }
      }

    // Test 2: SetParameters
    expectedParameters.Fill( 0.0 );
    expectedParameters[0] = 2.0;
    expectedParameters[3] = 2.0;

    transform->SetParameters( expectedParameters );
    parameters2 = transform->GetParameters();
    for( unsigned int k = 0; k < transform->GetNumberOfParameters(); k++ )
      {
      if( !testValue( parameters2[k], expectedParameters[k] ) )
        {
        std::cout << "Test failed:" << std::endl;
        std::cout << "Results=" << parameters2 << std::endl;
        std::cout << "Expected=" << expectedParameters << std::endl;
        any = true;
        break;
        }
      }

    // Test 3: GetInverse
    expectedParameters.Fill( 0.0 );
    expectedParameters[0] = 2.0;
    expectedParameters[3] = 2.0;

    transform->SetParameters( expectedParameters );

    TransformType::Pointer other = TransformType::New();
    transform->GetInverse( other );

    TransformType::Pointer otherbis =
      dynamic_cast<TransformType *>(transform->GetInverseTransform().GetPointer() );

    parameters2 = other->GetParameters();
    TransformType::ParametersType parameters2bis = otherbis->GetParameters();

    expectedParameters.Fill( 0.0 );
    expectedParameters[0] = 0.5;
    expectedParameters[3] = 0.5;

    other->Print( std::cout );
    otherbis->Print( std::cout );
    for( unsigned int k = 0; k < transform->GetNumberOfParameters(); k++ )
      {
      if( !testValue( parameters2[k], expectedParameters[k] ) )
        {
        std::cout << "Test failed:" << std::endl;
        std::cout << "Results=" << parameters2 << std::endl;
        std::cout << "Expected=" << expectedParameters << std::endl;
        any = true;
        break;
        }
      }
    for( unsigned int k = 0; k < transform->GetNumberOfParameters(); k++ )
      {
      if( !testValue( parameters2bis[k], expectedParameters[k] ) )
        {
        std::cout << "Test failed:" << std::endl;
        std::cout << "Results=" << parameters2bis << std::endl;
        std::cout << "Expected=" << expectedParameters << std::endl;
        any = true;
        break;
        }
      }

    // Try to invert a singular transform
    TransformType::Pointer singularTransform = TransformType::New();
    TransformType::Pointer singularTransformInverse = TransformType::New();
    singularTransform->Scale(0.0);
    if( !singularTransform->GetInverse(singularTransformInverse) )
      {
      std::cout << "Detected an attempt to invert a singular transform as expected" << std::endl;
      }
    else
      {
      std::cout << "Failed to detect an attempt to invert a singular transform!" << std::endl;
      return EXIT_FAILURE;
      }

    TransformType::Pointer singularTransformInverse2 =
      dynamic_cast<TransformType *>(singularTransform->GetInverseTransform().GetPointer() );
    if( !singularTransformInverse2 )
      {
      std::cout << "Detected an attempt to invert a singular transform as expected" << std::endl;
      }
    else
      {
      std::cout << "Failed to detect an attempt to invert a singular transform!" << std::endl;
      return EXIT_FAILURE;
      }
    }

  return any;
}

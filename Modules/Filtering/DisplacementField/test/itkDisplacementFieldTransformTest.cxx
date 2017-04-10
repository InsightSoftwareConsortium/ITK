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

#include "itkDisplacementFieldTransform.h"
#include "itkCenteredAffineTransform.h"
#include "itkStdStreamStateSave.h"
#include "itkMath.h"
#include "itkNumericTraits.h"
#include "itkTestingMacros.h"
#include "itkVectorLinearInterpolateImageFunction.h"


template <typename TPoint>
bool samePoint( const TPoint & p1, const TPoint & p2, double epsilon = 1e-8 )
{
  bool pass = true;

  for( unsigned int i = 0; i < TPoint::PointDimension; i++ )
    {
    if( !itk::Math::FloatAlmostEqual( p1[i], p2[i], 10, epsilon ) )
      {
      std::cout << "Error at index [" << i << "]" << std::endl;
      std::cout << "Expected: "
        << static_cast< typename itk::NumericTraits<
        typename TPoint::ValueType >::PrintType >( p1[i] )
        << ", but got: "
        << static_cast< typename itk::NumericTraits<
        typename TPoint::ValueType >::PrintType >( p2[i] )
        << std::endl;
      pass = false;
      }
    }
  return pass;
}

template <typename TVector>
bool sameVector( const TVector & v1, const TVector & v2, double epsilon = 1e-8 )
{
  bool pass = true;

  for( unsigned int i = 0; i < TVector::Dimension; i++ )
    {
    if( !itk::Math::FloatAlmostEqual( v1[i], v2[i], 10, epsilon ) )
      {
      std::cout << "Error at index [" << i << "]" << std::endl;
      std::cout << "Expected: "
        << static_cast< typename itk::NumericTraits<
        typename TVector::ValueType >::PrintType >( v1[i] )
        << ", but got: "
        << static_cast< typename itk::NumericTraits<
        typename TVector::ValueType >::PrintType >( v2[i] )
        << std::endl;
      pass = false;
      }
    }
  return pass;
}

template <typename TVector>
bool sameVariableVector( const TVector & v1, const TVector & v2, double epsilon = 1e-8 )
{
  bool pass = true;

  const unsigned int D1 = v1.Size();
  const unsigned int D2 = v2.Size();

  if( D1 != D2 )
    {
    return false;
    }
  for( unsigned int i = 0; i < D1; i++ )
    {
    if( !itk::Math::FloatAlmostEqual( v1[i], v2[i], 10, epsilon ) )
      {
      std::cout << "Error at index [" << i << "]" << std::endl;
      std::cout << "Expected: "
        << static_cast< typename itk::NumericTraits<
        typename TVector::ValueType >::PrintType >( v1[i] )
        << ", but got: "
        << static_cast< typename itk::NumericTraits<
        typename TVector::ValueType >::PrintType >( v2[i] )
        << std::endl;
      pass = false;
      }
    }
  return pass;
}

template <typename TTensor>
bool sameTensor( const TTensor & t1, const TTensor & t2, double epsilon = 1e-8 )
{
  bool pass = true;

  for( unsigned int i = 0; i < TTensor::InternalDimension; i++ )
    {
    if( !itk::Math::FloatAlmostEqual( t1[i], t2[i], 10, epsilon ) )
      {
      std::cout << "Error at index [" << i << "]" << std::endl;
      std::cout << "Expected: "
        << static_cast< typename itk::NumericTraits<
        typename TTensor::ValueType >::PrintType >( t1[i] )
        << ", but got: "
        << static_cast< typename itk::NumericTraits<
        typename TTensor::ValueType >::PrintType >( t2[i] )
        << std::endl;
      pass = false;
      }
    }
  return pass;
}

template <typename TArray2D>
bool sameArray2D( const TArray2D & a1, const TArray2D & a2, double epsilon = 1e-8 )
{
  bool pass = true;

  if( (a1.rows() != a2.rows() ) || (a1.cols() != a2.cols() ) )
    {
    return false;
    }
  for( unsigned int i = 0; i < a1.cols(); i++ )
    {
    for( unsigned int j = 0; j < a1.rows(); j++ )
      {
      if( !itk::Math::FloatAlmostEqual( a1(j, i), a2(j, i), 10, epsilon ) )
        {
        std::cout << "Error at index (" << j << ", " << i << ")" << std::endl;
        std::cout << "Expected: "
          << static_cast< typename itk::NumericTraits<
          typename TArray2D::ValueType >::PrintType >( a1(j, i) )
          << ", but got: "
          << static_cast< typename itk::NumericTraits<
          typename TArray2D::ValueType >::PrintType >( a2(j, i) )
          << std::endl;
        pass = false;
        }
      }
    }
  return pass;
}

int itkDisplacementFieldTransformTest( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " coordinateTolerance directionTolerance";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimensions = 2;

  typedef double ParametersValueType;

  typedef itk::DisplacementFieldTransform< ParametersValueType, Dimensions >
                                                            DisplacementTransformType;
  typedef DisplacementTransformType::ScalarType             ScalarType;
  typedef DisplacementTransformType::DisplacementFieldType  FieldType;
  typedef DisplacementTransformType::DisplacementFieldType  DisplacementFieldType;


  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope.
  itk::StdStreamStateSave coutState(std::cout);

  // Create a displacement field transform
  DisplacementTransformType::Pointer displacementTransform =
    DisplacementTransformType::New();

  EXERCISE_BASIC_OBJECT_METHODS( displacementTransform,
    DisplacementFieldTransform, Transform );


  DisplacementTransformType::DisplacementFieldType::Pointer displacementField =
    DisplacementTransformType::DisplacementFieldType::New();
  displacementTransform->SetDisplacementField( displacementField );
  TEST_SET_GET_VALUE( displacementField,
    displacementTransform->GetDisplacementField() );

  DisplacementTransformType::DisplacementFieldType::Pointer inverseDisplacementField =
    DisplacementTransformType::DisplacementFieldType::New();
  displacementTransform->SetInverseDisplacementField( inverseDisplacementField );
  TEST_SET_GET_VALUE( inverseDisplacementField,
    displacementTransform->GetInverseDisplacementField() );

  typedef itk::VectorLinearInterpolateImageFunction<
    DisplacementTransformType::DisplacementFieldType,
    DisplacementTransformType::ScalarType> InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  displacementTransform->SetInterpolator( interpolator );
  TEST_SET_GET_VALUE( interpolator,
    displacementTransform->GetInterpolator() );

  InterpolatorType::Pointer inverseInterpolator = InterpolatorType::New();
  displacementTransform->SetInverseInterpolator( inverseInterpolator );
  TEST_SET_GET_VALUE( inverseInterpolator,
    displacementTransform->GetInverseInterpolator() );

  double coordinateTolerance = atof( argv[1] );
  displacementTransform->SetCoordinateTolerance( coordinateTolerance );
  TEST_SET_GET_VALUE( coordinateTolerance,
    displacementTransform->GetCoordinateTolerance() );

  double directionTolerance = atof( argv[2] );
  displacementTransform->SetDirectionTolerance( directionTolerance );
  TEST_SET_GET_VALUE( directionTolerance,
    displacementTransform->GetDirectionTolerance() );


  FieldType::Pointer field = FieldType::New();

  FieldType::SizeType   size;
  FieldType::IndexType  start;
  FieldType::RegionType region;
  int                   dimLength = 20;
  size.Fill( dimLength );
  start.Fill( 0 );
  region.SetSize( size );
  region.SetIndex( start );
  field->SetRegions( region );
  field->Allocate();

  DisplacementTransformType::OutputVectorType zeroVector;
  zeroVector.Fill( 0 );
  field->FillBuffer( zeroVector );

  displacementTransform->SetDisplacementField( field );
  TEST_SET_GET_VALUE( field, displacementTransform->GetDisplacementField() );


  // Test the fixed parameters

  DisplacementTransformType::ParametersType fixedParameters =
    displacementTransform->GetFixedParameters();
  displacementTransform->SetFixedParameters( fixedParameters );
  TEST_SET_GET_VALUE( fixedParameters, displacementTransform->GetFixedParameters() );

  DisplacementFieldType::SizeType size2 =
    displacementTransform->GetDisplacementField()->GetLargestPossibleRegion().GetSize();
  DisplacementFieldType::PointType origin2 =
    displacementTransform->GetDisplacementField()->GetOrigin();
  DisplacementFieldType::DirectionType direction2 =
    displacementTransform->GetDisplacementField()->GetDirection();
  DisplacementFieldType::SpacingType spacing2 =
    displacementTransform->GetDisplacementField()->GetSpacing();

  size = field->GetLargestPossibleRegion().GetSize();
  DisplacementFieldType::PointType origin = field->GetOrigin();
  DisplacementFieldType::DirectionType direction = field->GetDirection();
  DisplacementFieldType::SpacingType spacing = field->GetSpacing();

  if( size != size2 )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Incorrect size from fixed parameters." << std::endl;
    return EXIT_FAILURE;
    }
  if( origin != origin2 )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Incorrect origin from fixed parameters." << std::endl;
    return EXIT_FAILURE;
    }
  if( spacing != spacing2 )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Incorrect spacing from fixed parameters." << std::endl;
    return EXIT_FAILURE;
    }
  if( direction != direction2 )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Incorrect direction from fixed parameters." << std::endl;
    return EXIT_FAILURE;
    }


  // Initialize Affine transform and use it to create the displacement field
  typedef itk::CenteredAffineTransform< ParametersValueType, Dimensions >
    AffineTransformType;

  typedef AffineTransformType::MatrixType AffineMatrixType;
  AffineMatrixType affineMatrix;
  affineMatrix( 0, 0 ) = 1.0;
  affineMatrix( 1, 0 ) = 0.01;
  affineMatrix( 0, 1 ) = 0.02;
  affineMatrix( 1, 1 ) = 1.1;

  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();
  affineTransform->SetMatrix( affineMatrix );

  DisplacementTransformType::JacobianType fieldJTruth;
  fieldJTruth.SetSize( DisplacementTransformType::Dimension, DisplacementTransformType::Dimension );
  fieldJTruth( 0, 0 ) = 1.0;
  fieldJTruth( 1, 0 ) = 0.01;
  fieldJTruth( 0, 1 ) = 0.02;
  fieldJTruth( 1, 1 ) = 1.1;

  itk::ImageRegionIteratorWithIndex< FieldType > it( field, field->GetLargestPossibleRegion() );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    FieldType::PointType pt;
    field->TransformIndexToPhysicalPoint( it.GetIndex(), pt );
    FieldType::PointType             pt2 = affineTransform->TransformPoint( pt );
    FieldType::PointType::VectorType vec = pt2 - pt;
    FieldType::PixelType             v;
    v[0] = vec[0];
    v[1] = vec[1];
    field->SetPixel( it.GetIndex(), v );
    ++it;
    }

  displacementTransform->SetDisplacementField( field );
  TEST_SET_GET_VALUE( field, displacementTransform->GetDisplacementField() );

  DisplacementTransformType::InputPointType testPoint;
  testPoint[0] = 10;
  testPoint[1] = 8;

  // Test LocalJacobian methods
  DisplacementTransformType::JacobianType jacobian( DisplacementTransformType::Dimension,
    DisplacementTransformType::Dimension );

  displacementTransform->ComputeJacobianWithRespectToPosition( testPoint, jacobian );

  double tolerance = 1e-6;
  if( !sameArray2D( jacobian, fieldJTruth, tolerance ) )
    {
    std::cout << "Failed getting local Jacobian: "
      << "ComputeJacobianWithRespectToPosition(...)" << std::endl;
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }

  DisplacementTransformType::JacobianType invfieldJTruth;
  invfieldJTruth.SetSize( DisplacementTransformType::Dimension,
    DisplacementTransformType::Dimension);
  invfieldJTruth( 0, 0 ) = affineTransform->GetInverseTransform()->GetParameters()[0];
  invfieldJTruth( 1, 0 ) = affineTransform->GetInverseTransform()->GetParameters()[1];
  invfieldJTruth( 0, 1 ) = affineTransform->GetInverseTransform()->GetParameters()[2];
  invfieldJTruth( 1, 1 ) = affineTransform->GetInverseTransform()->GetParameters()[3];

  displacementTransform->GetInverseJacobianOfForwardFieldWithRespectToPosition( testPoint, jacobian );

  tolerance = 1e-1;
  if( !sameArray2D( jacobian, invfieldJTruth, tolerance ) )
    {
    std::cout << "Error getting local inverse Jacobian: "
      << "GetInverseJacobianOfForwardFieldWithRespectToPosition(...)" << std::endl;
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }

  displacementTransform->GetInverseJacobianOfForwardFieldWithRespectToPosition( testPoint, jacobian, true );

  if( !sameArray2D( jacobian, invfieldJTruth, tolerance ) )
    {
    std::cout << "Error getting local inverse Jacobian with SVD: "
      << "GetInverseJacobianOfForwardFieldWithRespectToPosition(...)" << std::endl;
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }

  // Test ComputeJacobianWithRespectToParameters: should return identity

  DisplacementTransformType::JacobianType identity( Dimensions, Dimensions ), testIdentity;

  identity.Fill( 0 );
  for( unsigned int i = 0; i < Dimensions; i++ )
    {
    identity[i][i] = 1.0;
    }

  displacementTransform->ComputeJacobianWithRespectToParameters(
    testPoint, testIdentity );

  tolerance = 1e-10;
  if( !sameArray2D( identity, testIdentity, tolerance ) )
    {
    std::cout << "Error returning identity for "
      << "ComputeJacobianWithRespectToParameters(...)" << std::endl;
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }

  DisplacementTransformType::IndexType testIndex;
  testIdentity.SetSize( 1, 1 ); // make sure it gets resized properly
  displacementTransform->ComputeJacobianWithRespectToParameters(
    testIndex, testIdentity );

  if( !sameArray2D( identity, testIdentity, tolerance ) )
    {
    std::cout << "Error returning identity for "
      << "ComputeJacobianWithRespectToParameters(...)" << std::endl;
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }

  // Test transforming of points
  //

  DisplacementTransformType::OutputPointType deformOutput, deformTruth;

  // Test a point with non-zero displacement
  FieldType::IndexType idx;
  field->TransformPhysicalPointToIndex( testPoint, idx );
  deformTruth = testPoint + field->GetPixel( idx );

  deformOutput = displacementTransform->TransformPoint( testPoint );

  if( !samePoint( deformOutput, deformTruth ) )
    {
    std::cout << "Error transforming point: TransformPoint(...)" << std::endl;
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }

  DisplacementTransformType::InputVectorType  testVector;
  DisplacementTransformType::OutputVectorType deformVector, deformVectorTruth;
  testVector[0] = 0.5;
  testVector[1] = 0.5;

  deformVectorTruth = affineTransform->TransformVector( testVector );
  deformVector = displacementTransform->TransformVector( testVector, testPoint );

  tolerance = 1e-4;
  if( !sameVector( deformVector, deformVectorTruth, tolerance ) )
    {
    std::cout << "Error transforming vector: TransformVector(...)" << std::endl;
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }

  TRY_EXPECT_EXCEPTION( deformVector =
    displacementTransform->TransformVector( testVector ) );


  // Test VectorTransform for variable length vector
  DisplacementTransformType::InputVectorPixelType
    testVVector( DisplacementTransformType::Dimension );
  DisplacementTransformType::OutputVectorPixelType deformVVector,
    deformVVectorTruth( DisplacementTransformType::Dimension );
  testVVector[0] = 0.5;
  testVVector[1] = 0.5;

  deformVVectorTruth = affineTransform->TransformVector( testVVector );
  deformVVector = displacementTransform->TransformVector( testVVector, testPoint );

  if( !sameVariableVector( deformVVector, deformVVectorTruth, tolerance ) )
    {
    std::cout << "Error transforming variable length vector: "
      << "TransformVector(...)" << std::endl;
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }


  TRY_EXPECT_EXCEPTION( deformVVector =
    displacementTransform->TransformVector( testVVector ) );


  DisplacementTransformType::InputCovariantVectorType  testcVector;
  DisplacementTransformType::OutputCovariantVectorType deformcVector, deformcVectorTruth;
  testcVector[0] = 0.5;
  testcVector[1] = 0.5;

  deformcVectorTruth = affineTransform->TransformCovariantVector( testcVector );
  deformcVector = displacementTransform->TransformCovariantVector( testcVector, testPoint );

  tolerance = 1e-1;
  if( !sameVector( deformcVector, deformcVectorTruth, tolerance ) )
    {
    std::cout << "Error transforming covariant vector: "
      << "TransformCovariantVector(...)" << std::endl;
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }


  TRY_EXPECT_EXCEPTION( deformcVector =
    displacementTransform->TransformCovariantVector( testcVector ) );


  DisplacementTransformType::InputVectorPixelType
    testcVVector( DisplacementTransformType::Dimension );
  DisplacementTransformType::OutputVectorPixelType deformcVVector,
    deformcVVectorTruth( DisplacementTransformType::Dimension );
  testcVVector[0] = 0.5;
  testcVVector[1] = 0.5;

  deformcVVectorTruth = affineTransform->TransformCovariantVector( testcVVector );
  deformcVVector = displacementTransform->TransformCovariantVector( testcVVector, testPoint );

  if( !sameVariableVector( deformcVVector, deformcVVectorTruth, tolerance ) )
    {
    std::cout << "Error transforming variable length covariant vector: "
      <<  "TransformCovariantVector(...)" << std::endl;
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }


  TRY_EXPECT_EXCEPTION( deformcVVector =
    displacementTransform->TransformCovariantVector( testcVVector ) );


  DisplacementTransformType::InputDiffusionTensor3DType  testTensor;
  DisplacementTransformType::OutputDiffusionTensor3DType deformTensor,
    deformTensorTruth;
  testTensor[0] = 3;
  testTensor[1] = 0.01;
  testTensor[2] = 0.01;
  testTensor[3] = 2;
  testTensor[4] = 0.01;
  testTensor[5] = 1;

  // Pass thru functionality only for now
  deformTensorTruth = affineTransform->TransformDiffusionTensor3D( testTensor );
  deformTensor = displacementTransform->TransformDiffusionTensor3D(
      testTensor, testPoint );

  tolerance = 1e-4;
  if( !sameTensor( deformTensor, deformTensorTruth, tolerance ) )
    {
    std::cout << "Error transforming tensor: TransformDiffusionTensor3D(...)"
      << std::endl;
    std::cout << "Test failed!" << std::endl;
    // ToDo
    // Check this case. See
    // https://issues.itk.org/jira/browse/ITK-3537
    //return EXIT_FAILURE;
    }


  TRY_EXPECT_EXCEPTION( deformTensor =
    displacementTransform->TransformDiffusionTensor( testTensor ) );


  // Test setting parameters with wrong size

  DisplacementTransformType::ParametersType paramsWrongSize( 1 );
  paramsWrongSize.Fill( 0 );

  TRY_EXPECT_EXCEPTION( displacementTransform->SetParameters( paramsWrongSize ) );


  // Test UpdateTransformParameters

  DisplacementTransformType::DerivativeType derivative(
    displacementTransform->GetNumberOfParameters() );

  DisplacementTransformType::DerivativeType updateTruth(
    displacementTransform->GetNumberOfParameters() );

  DisplacementTransformType::ParametersType params(
    displacementTransform->GetNumberOfParameters() );

  derivative.Fill( 1.2 );

  ScalarType testFactor = 1.5;

  for( unsigned int i = 0;
       i < displacementTransform->GetNumberOfParameters(); i++ )
    {
    params[i] = i;
    updateTruth[i] = params[i] + derivative[i] * testFactor;
    }
  displacementTransform->SetParameters( params );
  displacementTransform->UpdateTransformParameters( derivative, testFactor );
  params = displacementTransform->GetParameters();

  for( unsigned int i = 0;
       i < displacementTransform->GetNumberOfParameters(); i++ )
    {
    if( itk::Math::NotExactlyEquals( params[i], updateTruth[i] ) )
      {
      std::cout << "Test failed!" << std::endl;
      std::cout << "Error in UpdateTransformParameters(...) at index ["
        << i << "]" << std::endl;
      std::cout << "Expected: "
        << static_cast< itk::NumericTraits<
        DisplacementTransformType::DerivativeType::ValueType >::PrintType >( updateTruth[i] )
        << ", but got: "
        << static_cast< itk::NumericTraits<
        DisplacementTransformType::ParametersType::ValueType >::PrintType >( params[i] )
        << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Test IsLinear(): should always return false
  if( displacementTransform->IsLinear() )
    {
    std::cout << "DisplacementFieldTransform returned 'true' for IsLinear()."
      << " Expected 'false'." << std::endl;
    return EXIT_FAILURE;
    }


  // Exercise other methods to improve coverage
  //

  std::cout << "DisplacementFieldSetTime: " <<
    static_cast< itk::NumericTraits< itk::ModifiedTimeType >::PrintType >(
    displacementTransform->GetDisplacementFieldSetTime() ) << std::endl;


  // The inverse displacement field for the inverse displacement transform must
  // have been set to ITK_NULLPTR when calling SetDisplacementField(), so
  // 'false' should be returned here
  DisplacementTransformType::Pointer inverseTransform = DisplacementTransformType::New();
  if( displacementTransform->GetInverse( inverseTransform ) )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Expected GetInverse() to return 'false'." << std::endl;
    return EXIT_FAILURE;
    }

  // Set the inverse displacement field
  displacementTransform->SetInverseDisplacementField( field );

  displacementTransform->SetIdentity();

  // Create a new one with null for both fields
  displacementTransform = DisplacementTransformType::New();

  displacementTransform->SetIdentity();

  displacementTransform->SetDisplacementField( ITK_NULLPTR );
  displacementTransform->SetInverseDisplacementField( ITK_NULLPTR );

  // Check setting all zero for fixed parameters
  displacementTransform = DisplacementTransformType::New();
  fixedParameters = displacementTransform->GetFixedParameters();
  fixedParameters.Fill( 0.0 );
  displacementTransform->SetFixedParameters( fixedParameters );


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}

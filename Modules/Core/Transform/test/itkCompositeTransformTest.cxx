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

#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
#include "itkTranslationTransform.h"
#include "itkMath.h"

namespace
{

const double epsilon = 1e-10;

template <typename TPoint>
bool testPoint( const TPoint & p1, const TPoint & p2 )
{
  bool pass = true;

  for( unsigned int i = 0; i < TPoint::PointDimension; i++ )
    {
    if( std::fabs( p1[i] - p2[i] ) > epsilon )
      {
      pass = false;
      }
    }
  return pass;
}

template <typename TMatrix>
bool testMatrix( const TMatrix & m1, const TMatrix & m2 )
{
  unsigned int i, j;
  bool         pass = true;

  for( i = 0; i < TMatrix::RowDimensions; i++ )
    {
    for( j = 0; j < TMatrix::ColumnDimensions; j++ )
      {
      if( std::fabs( m1[i][j] - m2[i][j] ) > epsilon )
        {
        pass = false;
        }
      }
    }
  return pass;
}

template <typename TArray2D>
bool testJacobian( const TArray2D & m1, const TArray2D & m2 )
{
  unsigned int i, j;
  bool         pass = true;

  for( i = 0; i < m1.rows(); i++ )
    {
    for( j = 0; j < m1.cols(); j++ )
      {
      if( std::fabs( m1[i][j] - m2[i][j] ) > epsilon )
        {
        pass = false;
        }
      }
    }
  return pass;
}

template <typename TVector>
bool testVectorArray( const TVector & v1, const TVector & v2 )
{
  bool pass = true;

  for( unsigned int i = 0; i < v1.Size(); i++ )
    {
    if( std::fabs( v1[i] - v2[i] ) > epsilon )
      {
      pass = false;
      }
    }
  return pass;
}

} // namespace

/******/

int itkCompositeTransformTest(int, char *[] )
{
  const unsigned int NDimensions = 2;

  /* Create composite transform */
  typedef itk::CompositeTransform<double, NDimensions> CompositeType;
  typedef CompositeType::ScalarType                    ScalarType;

  CompositeType::Pointer compositeTransform = CompositeType::New();

  /* Test obects */
  typedef  itk::Matrix<ScalarType, NDimensions, NDimensions> Matrix2Type;
  typedef  itk::Vector<ScalarType, NDimensions>              Vector2Type;

  /* Test that we have an empty the queue */
  if( compositeTransform->GetNumberOfTransforms() != 0 )
    {
    std::cout << "Failed. Expected GetNumberOfTransforms to return 0." << std::endl;
    return EXIT_FAILURE;
    }

  /* Add an affine transform */
  typedef itk::AffineTransform<ScalarType, NDimensions> AffineType;
  AffineType::Pointer affine = AffineType::New();
  Matrix2Type         matrix2;
  Vector2Type         vector2;
  matrix2[0][0] = 1;
  matrix2[0][1] = 2;
  matrix2[1][0] = 3;
  matrix2[1][1] = 4;
  vector2[0] = 5;
  vector2[1] = 6;
  affine->SetMatrix(matrix2);
  affine->SetOffset(vector2);

  compositeTransform->AddTransform( affine );

  /* Test that we have one transform in the queue */
  if( compositeTransform->GetNumberOfTransforms() != 1 )
    {
    std::cout << "Failed adding transform to queue." << std::endl;
    return EXIT_FAILURE;
    }

  //std::cout << "Composite Transform:" << std::endl << compositeTransform;

  /* Retrieve the transform and check that it's the same */
  std::cout << "Retrieve 1st transform." << std::endl;
  AffineType::ConstPointer affineGet = dynamic_cast<AffineType const *>( compositeTransform->GetNthTransformConstPointer(0) );
  if( affineGet.IsNull() )
    {
    std::cout << "Failed retrieving transform from queue." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Retrieve matrix and offset. " << std::endl;
  Matrix2Type matrix2Get = affineGet->GetMatrix();
  Vector2Type vector2Get = affineGet->GetOffset();
  if( !testMatrix(matrix2, matrix2Get) || !testVectorArray(vector2, vector2Get ) )
    {
    std::cout << "Failed retrieving correct transform data." << std::endl;
    return EXIT_FAILURE;
    }

  /* Get parameters with single transform.
   * Should be same as GetParameters from affine transform. */
  std::cout << "Get Parameters: " << std::endl;
  CompositeType::ParametersType parametersTest, parametersTruth;
  parametersTest = compositeTransform->GetParameters();
  parametersTruth = affine->GetParameters();
  std::cout << "affine parametersTruth: " << std::endl << parametersTruth
            << std::endl
            << "parametersTest from Composite: " << std::endl << parametersTest
            << std::endl;

  if( !testVectorArray( parametersTest, parametersTruth ) )
    {
    std::cout << "Failed GetParameters() for single transform." << std::endl;
    return EXIT_FAILURE;
    }

  /* Set parameters with single transform. */
  CompositeType::ParametersType parametersNew(6), parametersReturned;
  parametersNew[0] = 0;
  parametersNew[1] = 10;
  parametersNew[2] = 20;
  parametersNew[3] = 30;
  parametersNew[4] = 40;
  parametersNew[5] = 50;
  std::cout << "Set Parameters: " << std::endl;
  compositeTransform->SetParameters( parametersNew );
  std::cout << "retrieving... " << std::endl;
  parametersReturned = compositeTransform->GetParameters();
  std::cout << "parametersNew: " << std::endl << parametersNew << std::endl
            << "parametersReturned: " << std::endl << parametersReturned
            << std::endl;
  if( !testVectorArray( parametersNew, parametersReturned ) )
    {
    std::cout << "Failed SetParameters() for single transform." << std::endl;
    return EXIT_FAILURE;
    }

  /* Test fixed parameters set/get */
  parametersTest = compositeTransform->GetFixedParameters();
  parametersTruth = affine->GetFixedParameters();
  std::cout << "Get Fixed Parameters: " << std::endl
            << "affine parametersTruth: " << std::endl << parametersTruth
            << std::endl
            << "parametersTest from Composite: " << std::endl << parametersTest
            << std::endl;

  if( !testVectorArray( parametersTest, parametersTruth ) )
    {
    std::cout << "Failed GetFixedParameters() for single transform." << std::endl;
    return EXIT_FAILURE;
    }

  parametersNew.SetSize( parametersTruth.Size() );
  parametersNew.Fill(1);
  parametersNew[0] = 42;

  std::cout << "Set Fixed Parameters: " << std::endl;
  compositeTransform->SetFixedParameters( parametersNew );
  std::cout << "retrieving... " << std::endl;
  parametersReturned = compositeTransform->GetFixedParameters();
  std::cout << "parametersNew: " << std::endl << parametersNew << std::endl
            << "parametersReturned: " << std::endl << parametersReturned
            << std::endl;
  if( !testVectorArray( parametersNew, parametersReturned ) )
    {
    std::cout << "Failed SetFixedParameters() for single transform." << std::endl;
    return EXIT_FAILURE;
    }

  /* Reset affine transform to original values */
  compositeTransform->ClearTransformQueue();

  affine = AffineType::New();
  affine->SetMatrix(matrix2);
  affine->SetOffset(vector2);
  compositeTransform->AddTransform( affine );

  /* Setup test point and truth value for tests */
  CompositeType::InputPointType  inputPoint;
  CompositeType::OutputPointType outputPoint, affineTruth;
  inputPoint[0] = 2;
  inputPoint[1] = 3;
  affineTruth[0] = 13;
  affineTruth[1] = 24;

  CompositeType::InputVectorType inputVector;
  CompositeType::OutputVectorType outputVector;
  inputVector[0] = 0.4;
  inputVector[1] = 0.6;

  CompositeType::InputCovariantVectorType inputCVector;
  CompositeType::OutputCovariantVectorType outputCVector;
  inputCVector[0] = 0.4;
  inputCVector[1] = 0.6;

  /* Test transforming the point with just the single affine transform */
  outputPoint = compositeTransform->TransformPoint( inputPoint );
  if( !testPoint( outputPoint, affineTruth) )
    {
    std::cout << "Failed transforming point with single transform."
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Test inverse */
  CompositeType::Pointer         inverseTransform = CompositeType::New();
  CompositeType::OutputPointType inverseTruth, inverseOutput;
  if( !compositeTransform->GetInverse( inverseTransform ) )
    {
    std::cout << "ERROR: GetInverse() failed." << std::endl;
    return EXIT_FAILURE;
    }
  inverseTruth = inputPoint;
  inverseOutput = inverseTransform->TransformPoint( affineTruth );
  std::cout << "Transform point with inverse composite transform: "
            << std::endl
            << "  Test point: " << affineTruth << std::endl
            << "  Truth: " << inverseTruth << std::endl
            << "  Output: " << inverseOutput << std::endl;
  if( !testPoint( inverseOutput, inverseTruth ) )
    {
    std::cout << "Failed transform point with inverse composite transform (1)."
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Test ComputeJacobianWithRespectToParameters */

  CompositeType::JacobianType   jacComposite, jacSingle;
  CompositeType::InputPointType jacPoint;
  jacPoint[0] = 1;
  jacPoint[1] = 2;
  affine->ComputeJacobianWithRespectToParameters( jacPoint, jacSingle );
  std::cout << "Single jacobian:" << std::endl << jacSingle << std::endl;
  compositeTransform->ComputeJacobianWithRespectToParameters( jacPoint, jacComposite );
  std::cout << "Composite jacobian:" << std::endl << jacComposite << std::endl;
  if( !testJacobian( jacComposite, jacSingle ) )
    {
    std::cout << "Failed getting jacobian for single transform." << std::endl;
    return EXIT_FAILURE;
    }

  /*
   * Create and add 2nd transform
   */
  AffineType::Pointer affine2 = AffineType::New();
  matrix2[0][0] = 11;
  matrix2[0][1] = 22;
  matrix2[1][0] = 33;
  matrix2[1][1] = 44;
  vector2[0] = 55;
  vector2[1] = 65;
  affine2->SetMatrix(matrix2);
  affine2->SetOffset(vector2);

  compositeTransform->ClearTransformQueue();
  compositeTransform->AppendTransform( affine2 );
  compositeTransform->PrependTransform( affine );

  std::cout << std::endl << "Two-component Composite Transform:"
            << std::endl << compositeTransform;
  std::cout << std::endl << "Transform at queue position 0: "
            << std::endl << compositeTransform->GetNthTransformConstPointer( 0 );

  /* Test that we have two tranforms in the queue */
  if( compositeTransform->GetNumberOfTransforms() != 2 )
    {
    std::cout << "Failed adding 2nd transform to queue." << std::endl;
    return EXIT_FAILURE;
    }

  /* Transform a point with both transforms. Remember that transforms
   * are applied in *reverse* queue order, with most-recently added transform first. */
  CompositeType::OutputPointType compositeTruth;
  compositeTruth = affine2->TransformPoint( inputPoint );
  compositeTruth = affine->TransformPoint( compositeTruth );

  outputPoint = compositeTransform->TransformPoint( inputPoint );
  std::cout << "Transform point with two-component composite transform: "
            << std::endl
            << "  Test point: " << inputPoint << std::endl
            << "  Truth: " << compositeTruth << std::endl
            << "  Output: " << outputPoint << std::endl;

  if( !testPoint( outputPoint, compositeTruth) )
    {
    std::cout << "Failed transforming point with two transforms."
              << std::endl;
    return EXIT_FAILURE;
    }

  CompositeType::OutputVectorType compositeTruthVector;
  compositeTruthVector = affine2->TransformVector( inputVector );
  compositeTruthVector = affine->TransformVector( compositeTruthVector );
  outputVector = compositeTransform->TransformVector( inputVector );
  std::cout << "Transform vector with two-component composite transform: "
            << std::endl
            << "  Test vector: " << inputVector << std::endl
            << "  Truth: " << compositeTruthVector << std::endl
            << "  Output: " << outputVector << std::endl;

  CompositeType::OutputCovariantVectorType compositeTruthCVector;
  compositeTruthCVector = affine2->TransformCovariantVector( inputCVector );
  compositeTruthCVector = affine->TransformCovariantVector( compositeTruthCVector );
  outputCVector = compositeTransform->TransformCovariantVector( inputCVector );
  std::cout << "Transform covariant vector with two-component composite transform: "
            << std::endl
            << "  Test vector: " << inputCVector << std::endl
            << "  Truth: " << compositeTruthCVector << std::endl
            << "  Output: " << outputCVector << std::endl;


  CompositeType::InputDiffusionTensor3DType inputTensor;
  CompositeType::OutputDiffusionTensor3DType outputTensor;
  inputTensor[0] = 3.0;
  inputTensor[1] = 0.3;
  inputTensor[2] = 0.2;
  inputTensor[3] = 2.0;
  inputTensor[4] = 0.1;
  inputTensor[5] = 1.0;
  CompositeType::OutputDiffusionTensor3DType compositeTruthTensor;
  compositeTruthTensor = affine2->TransformDiffusionTensor3D( inputTensor );
  compositeTruthTensor = affine->TransformDiffusionTensor3D( compositeTruthTensor );
  outputTensor = compositeTransform->TransformDiffusionTensor3D( inputTensor );
  std::cout << "Transform tensor with two-component composite transform: "
            << std::endl
            << "  Test tensor: " << inputTensor << std::endl
            << "  Truth: " << compositeTruthTensor << std::endl
            << "  Output: " << outputTensor << std::endl;

  CompositeType::InputSymmetricSecondRankTensorType inputSTensor;
  CompositeType::OutputSymmetricSecondRankTensorType outputSTensor;
  inputSTensor(1,0) = 0.5;
  inputSTensor(0,0) = 3.0;
  inputSTensor(1,1) = 2.0;

  CompositeType::OutputSymmetricSecondRankTensorType compositeTruthSTensor;
  compositeTruthSTensor = affine2->TransformSymmetricSecondRankTensor( inputSTensor );
  compositeTruthSTensor = affine->TransformSymmetricSecondRankTensor( compositeTruthSTensor );
  outputSTensor = compositeTransform->TransformSymmetricSecondRankTensor( inputSTensor );
  std::cout << "Transform tensor with two-component composite transform: "
            << std::endl
            << "  Test tensor: " << inputSTensor << std::endl
            << "  Truth: " << compositeTruthSTensor << std::endl
            << "  Output: " << outputSTensor << std::endl;

  /* Test inverse with two transforms, with only one set to optimize. */
  compositeTransform->SetAllTransformsToOptimize( false );
  compositeTransform->SetNthTransformToOptimizeOn( 0 );
  if( !compositeTransform->GetInverse( inverseTransform ) )
    {
    std::cout << "Expected GetInverse() to succeed." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Inverse two-component transform: " << inverseTransform;

  /* Check that optimization flag inverse worked */
  if( inverseTransform->GetNthTransformToOptimize( 0 ) ||
      !inverseTransform->GetNthTransformToOptimize( 1 ) )
    {
    std::cout << "GetInverse failed for TransformsToOptimize flags."
              << std::endl;
    return EXIT_FAILURE;
    }
  compositeTransform->SetAllTransformsToOptimizeOn(); // Set back to do all.
  inverseTransform->SetAllTransformsToOptimizeOn();

  /* Transform point with inverse */
  inverseTruth = inputPoint;
  inverseOutput = inverseTransform->TransformPoint( compositeTruth );
  std::cout << "Transform point with two-component inverse composite transform: "
            << std::endl
            << "  Test point: " << compositeTruth << std::endl
            << "  Truth: " << inverseTruth << std::endl
            << "  Output: " << inverseOutput << std::endl;
  if( !testPoint( inverseOutput, inverseTruth ) )
    {
    std::cout << "Failed transform point with two-component inverse composite transform."
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Get inverse transform again, but using other accessor. */
  CompositeType::ConstPointer inverseTransform2;
  std::cout << "Call GetInverseTransform():" << std::endl;
  inverseTransform2 = dynamic_cast<const CompositeType *>( compositeTransform->GetInverseTransform().GetPointer() );
  if( !inverseTransform2 )
    {
    std::cout << "Failed calling GetInverseTransform()." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Transform point: " << std::endl;
  inverseOutput = inverseTransform2->TransformPoint( compositeTruth );
  if( !testPoint( inverseOutput, inverseTruth ) )
    {
    std::cout << "Failed transform point with two-component inverse composite transform (2)." << std::endl;
    return EXIT_FAILURE;
    }

  /* Test IsLinear() by calling on each sub transform */
  std::cout << "Test IsLinear" << std::endl;
  bool allAreLinear = true;
  for( unsigned int n = 0; n < compositeTransform->GetNumberOfTransforms(); n++ )
    {
    if( !compositeTransform->GetNthTransformConstPointer( n )->IsLinear() )
      {
      allAreLinear = false;
      }
    }
  if( compositeTransform->IsLinear() != allAreLinear )
    {
    std::cout << "compositeTransform returned unexpected value for IsLinear(). Expected " << allAreLinear << std::endl;
    return EXIT_FAILURE;
    }

  /* Test GetNumberOfParameters */
  std::cout << "GetNumberOfParameters: " << std::endl;
  unsigned int affineParamsN = affine->GetNumberOfParameters();
  unsigned int affine2ParamsN = affine2->GetNumberOfParameters();
  unsigned int nParameters = compositeTransform->GetNumberOfParameters();
  std::cout << "Number of parameters: " << nParameters << std::endl;
  if( nParameters != affineParamsN + affine2ParamsN )
    {
    std::cout << "GetNumberOfParameters failed for multi-transform."
              << std::endl << "Expected " << affineParamsN + affine2ParamsN
              << std::endl;
    }

  /* Get parameters with multi-transform. They're filled from transforms in
   * same order as transforms are applied, from back of queue to front. */
  parametersTest = compositeTransform->GetParameters();
  parametersTruth.SetSize( affine2ParamsN + affineParamsN );
  /* Fill using different method than is used in the class.
     Remember we added affine2 2nd, so it's at front of queue */
  for( unsigned int n = 0; n < affine2ParamsN; n++ )
    {
    parametersTruth.SetElement(
      n, affine2->GetParameters().GetElement( n ) );
    }
  for( unsigned int n = 0; n < affineParamsN; n++ )
    {
    parametersTruth.SetElement( n + affine2ParamsN,
                                affine->GetParameters().GetElement( n ) );
    }
  std::cout << "Get Multi-transform Parameters: " << std::endl
            << "parametersTruth: " << std::endl << parametersTruth
            << std::endl
            << "parametersTest from Composite: " << std::endl << parametersTest
            << std::endl;

  if( !testVectorArray( parametersTest, parametersTruth ) )
    {
    std::cout << "Failed GetParameters() for multi transform." << std::endl;
    return EXIT_FAILURE;
    }

  /* Set parameters with multi transform. */
  parametersNew.SetSize( parametersTruth.Size() );
  parametersNew.Fill( 3.14 );
  parametersNew[0] = 19;
  parametersNew[parametersTruth.Size() - 1] = 71;
  std::cout << "Set Multi-transform Parameters: " << std::endl;
  compositeTransform->SetParameters( parametersNew );
  std::cout << "retrieving... " << std::endl;
  parametersReturned = compositeTransform->GetParameters();
  std::cout << "parametersNew: " << std::endl << parametersNew << std::endl
            << "parametersReturned: " << std::endl << parametersReturned
            << std::endl;
  if( !testVectorArray( parametersNew, parametersReturned ) )
    {
    std::cout << "Failed SetParameters() for multi transform." << std::endl;
    return EXIT_FAILURE;
    }

  /* Test get fixed parameters with multi-transform */
  parametersTest = compositeTransform->GetFixedParameters();
  affineParamsN = affine->GetFixedParameters().Size();
  affine2ParamsN = affine2->GetFixedParameters().Size();
  parametersTruth.SetSize( affine2ParamsN + affineParamsN );
  parametersTruth.Fill(0); // Try this to quiet valgrind
  for( unsigned int n = 0; n < affine2ParamsN; n++ )
    {
    parametersTruth.SetElement(
      n, affine2->GetFixedParameters().GetElement( n ) );
    }
  for( unsigned int n = 0; n < affineParamsN; n++ )
    {
    parametersTruth.SetElement( n + affine2ParamsN,
                                affine->GetFixedParameters().GetElement( n ) );
    }
  std::cout << "Get Multi-transform Fixed Parameters: " << std::endl
            << "parametersTruth: " << std::endl << parametersTruth
            << std::endl
            << "parametersTest: " << std::endl << parametersTest
            << std::endl;

  if( !testVectorArray( parametersTest, parametersTruth ) )
    {
    std::cout << "Failed GetFixedParameters() for multi transform." << std::endl;
    return EXIT_FAILURE;
    }

  /* Test set fixed parameters with multi-transform */
  std::cout << "Set Multi-transform Fixed Parameters: " << std::endl;
  compositeTransform->SetFixedParameters( parametersTruth );
  std::cout << "retrieving... " << std::endl;
  parametersReturned = compositeTransform->GetFixedParameters();
  std::cout << "parametersTruth: " << std::endl << parametersTruth << std::endl
            << "parametersReturned: " << std::endl << parametersReturned
            << std::endl;
  // std::cout << "Composite Transform: " << std::endl << compositeTransform;
  if( !testVectorArray( parametersTruth, parametersReturned ) )
    {
    std::cout << "Failed SetFixedParameters() for multi transform." << std::endl;
    return EXIT_FAILURE;
    }

  /*
   * Add a third transform
   */

  /* Add yet another affine transform */
  AffineType::Pointer affine3 = AffineType::New();
  matrix2[0][0] = 1.1;
  matrix2[0][1] = 2.2;
  matrix2[1][0] = 3.3;
  matrix2[1][1] = 4.4;
  vector2[0] = 5.5;
  vector2[1] = 6.5;
  affine3->SetMatrix(matrix2);
  affine3->SetOffset(vector2);

  compositeTransform->AddTransform( affine3 );
  // std::cout << "compositeTransform with 3 subs: "
  //          << std::endl << compositeTransform << std::endl;

  /* Reset first affine to non-singular values */
  matrix2[0][0] = 1;
  matrix2[0][1] = 2;
  matrix2[1][0] = 3;
  matrix2[1][1] = 4;
  vector2[0] = 5;
  vector2[1] = 6;
  affine->SetMatrix(matrix2);
  affine->SetOffset(vector2);

  /* Test TransformsToOptimize flags */
  compositeTransform->SetAllTransformsToOptimizeOff();
  if( compositeTransform->GetNthTransformToOptimize(0) ||
      compositeTransform->GetNthTransformToOptimize(1) ||
      compositeTransform->GetNthTransformToOptimize(2) )
    {
    std::cout << "Failed clearing all TransformToOptimize flags. " << std::endl;
    return EXIT_FAILURE;
    }

  compositeTransform->SetOnlyMostRecentTransformToOptimizeOn();
  if( compositeTransform->GetNthTransformToOptimize(0) ||
      compositeTransform->GetNthTransformToOptimize(1) ||
      !compositeTransform->GetNthTransformToOptimize(2) )
    {
    std::cout << "Failed setting only most recent TransformsToOptimize flag. "
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Test accessors */
  CompositeType::TransformQueueType transformQueue =
    compositeTransform->GetTransformQueue();
  if( transformQueue.size() != 3 )
    {
    std::cout << "Failed getting transform queue." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Got TransformQueue." << std::endl;

  CompositeType::TransformsToOptimizeFlagsType flagsQueue =
    compositeTransform->GetTransformsToOptimizeFlags();
  if( flagsQueue.size() != 3 )
    {
    std::cout << "Failed getting optimize flags queue." << std::endl;
    return EXIT_FAILURE;
    }

  /* Get inverse and check TransformsToOptimize flags are correct */
  CompositeType::ConstPointer inverseTransform3;
  inverseTransform3 = dynamic_cast<const CompositeType *>
    ( compositeTransform->GetInverseTransform().GetPointer() );
  if( !inverseTransform3 )
    {
    std::cout << "Failed calling GetInverseTransform() (3)." << std::endl;
    return EXIT_FAILURE;
    }
  if( !inverseTransform3->GetNthTransformToOptimize(0) ||
      inverseTransform3->GetNthTransformToOptimize(1) ||
      inverseTransform3->GetNthTransformToOptimize(2) )
    {
    std::cout << "Failed checking TransformsToOptimize flags on inverse. "
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Test get params with only 1st and last transforms set to optimize.
   * This implicitly tests the m_PreviousTransformsToOptimizeUpdateTime mechanism
   * for updating m_TransformsToOptimizeQueue.
   * This includes the affine and affine3 transforms */

  compositeTransform->SetNthTransformToOptimize(0, true);
  if( !compositeTransform->GetNthTransformToOptimize(0) ||
      compositeTransform->GetNthTransformToOptimize(1) ||
      !compositeTransform->GetNthTransformToOptimize(2) )
    {
    std::cout << "Failed setting last TransformToOptimize flag. "
              << "Composite Transform: " << std::endl << compositeTransform
              << std::endl;
    return EXIT_FAILURE;
    }

  parametersTest = compositeTransform->GetParameters();
  affineParamsN = affine->GetNumberOfParameters();
  unsigned int affine3ParamsN = affine3->GetNumberOfParameters();
  parametersTruth.SetSize( affineParamsN + affine3ParamsN );
  for( unsigned int n = 0; n < affine3ParamsN; n++ )
    {
    parametersTruth.SetElement(
      n, affine3->GetParameters().GetElement( n ) );
    }
  for( unsigned int n = 0; n < affineParamsN; n++ )
    {
    parametersTruth.SetElement( n + affine3ParamsN,
                                affine->GetParameters().GetElement( n ) );
    }
  std::cout << "Get 1st and 3rd transform Parameters: " << std::endl
            << "parametersTruth: " << std::endl << parametersTruth
            << std::endl
            << "parametersTest from Composite: " << std::endl << parametersTest
            << std::endl;

  if( !testVectorArray( parametersTest, parametersTruth ) )
    {
    std::cout << "Failed GetParameters() for 1st and 3rd transforms." << std::endl;
    return EXIT_FAILURE;
    }

  /* Test ComputeJacobianWithRespectToParameters with three transforms, two of which (1st and 3rd) are active.
   * Remember that the point gets transformed by preceding transforms
   * before its used for individual Jacobian. */
  std::cout << "Test ComputeJacobianWithRespectToParameters with three transforms: " << std::endl;
  CompositeType::JacobianType   jacTruth, jacComposite2, jacAffine, jacAffine3;
  CompositeType::InputPointType jacPoint2;
  jacPoint2[0] = 1;
  jacPoint2[1] = 2;
  compositeTransform->ComputeJacobianWithRespectToParameters( jacPoint2, jacComposite2 );
  affine3->ComputeJacobianWithRespectToParameters( jacPoint2, jacAffine3 );
  jacPoint2 = affine3->TransformPoint( jacPoint2 );
  jacPoint2 = affine2->TransformPoint( jacPoint2 );
  affine->ComputeJacobianWithRespectToParameters( jacPoint2, jacAffine );
  jacTruth.SetSize( jacAffine3.rows(), jacAffine.cols() + jacAffine3.cols() );
  jacTruth.update( affine->GetMatrix() * affine2->GetMatrix() * jacAffine3, 0, 0 );
  jacTruth.update( jacAffine, 0, jacAffine3.cols() );
  std::cout << "transformed jacPoint: " << jacPoint2 << std::endl;
  std::cout << "Affine jacobian:" << std::endl << jacAffine;
  std::cout << "affine3 jacobian:" << std::endl << jacAffine3;
  std::cout << "Truth jacobian:" << std::endl << jacTruth;
  std::cout << "Composite jacobian:" << std::endl << jacComposite2;
  if( !testJacobian( jacComposite2, jacTruth ) )
    {
    std::cout << "Failed getting jacobian for two active transforms." << std::endl;
    return EXIT_FAILURE;
    }

  /* Test UpdateTransformParameters.
   * NOTE Once there are transforms that do something other than simple
   * addition in TransformUpdateParameters, this should be updated here.
   */
    {
    /* Single transform full update, of last transform only */
    compositeTransform->SetOnlyMostRecentTransformToOptimizeOn();
    CompositeType::ParametersType truth = compositeTransform->GetParameters();
    CompositeType::DerivativeType
    update( compositeTransform->GetNumberOfParameters() );
    update.Fill(10);
    truth += update;
    compositeTransform->UpdateTransformParameters( update );
    CompositeType::ParametersType
      updateResult = compositeTransform->GetParameters();
    std::cout << "Testing UpdateTransformParameters 1. "
              << std::endl;
    if( !testVectorArray( truth, updateResult ) )
      {
      std::cout << "UpdateTransformParameters 1 failed. " << std::endl
                << " truth:  " << truth << std::endl
                << " result: " << updateResult << std::endl;
      return EXIT_FAILURE;
      }

    /* Update partially two transforms, with a scaling factor */
    compositeTransform->SetNthTransformToOptimizeOn(0);
    truth = compositeTransform->GetParameters();
    update.SetSize( compositeTransform->GetNumberOfParameters() );
    AffineType::ScalarType factor = 0.5;
    for( unsigned int i = 0;
         i < compositeTransform->GetNumberOfParameters(); i++ )
      {
      update[i] = i;
      truth[i] += update[i] * factor;
      }
    compositeTransform->UpdateTransformParameters( update, factor );
    updateResult = compositeTransform->GetParameters();
    std::cout << "Testing UpdateTransformParameters 3. "
              << std::endl;
    if( !testVectorArray( truth, updateResult ) )
      {
      std::cout << "UpdateTransformParameters 3 failed. " << std::endl
                << " truth:  " << truth << std::endl
                << " result: " << updateResult << std::endl;
      return EXIT_FAILURE;
      }
    }

  /* Test RemoveTransform */
  bool opt1 = compositeTransform->GetTransformsToOptimizeFlags()[0];
  bool opt2 = compositeTransform->GetTransformsToOptimizeFlags()[1];
  compositeTransform->RemoveTransform();
  if( compositeTransform->GetNumberOfTransforms() != 2 )
    {
    std::cout << "ERROR: expected 2 transforms, got " << compositeTransform->GetNumberOfTransforms() << std::endl;
    return EXIT_FAILURE;
    }
  if( affine != compositeTransform->GetNthTransformConstPointer( 0 ) )
    {
    std::cout << "ERROR: 1st transform is not affine" << std::endl;
    return EXIT_FAILURE;
    }
  if( affine2 != compositeTransform->GetNthTransformConstPointer( 1 ) )
    {
    std::cout << "ERROR: 2nd transform is not affine2" << std::endl;
    return EXIT_FAILURE;
    }
  if( compositeTransform->GetTransformsToOptimizeFlags().size() != 2 )
    {
    std::cout << "ERROR: TransformsToOptimizeQueue is not length 2. It is " << compositeTransform->GetTransformsToOptimizeFlags().size() << std::endl;
    return EXIT_FAILURE;
    }
  if( compositeTransform->GetNthTransformToOptimize(0) != opt1 )
    {
    std::cout << "ERROR: TransformsToOptimizeFlags[0] is not " << opt1 << std::endl;
    return EXIT_FAILURE;
    }
  if( compositeTransform->GetNthTransformToOptimize(1) != opt2 )
    {
    std::cout << "ERROR: TransformsToOptimizeFlags[1] is not " << opt2 << std::endl;
    return EXIT_FAILURE;
    }

  /*
   * Test flattening transform queue in the case of nested composite
   * transforms
   */

  CompositeType::Pointer nestedCompositeTransform = CompositeType::New();
  CompositeType::Pointer compositeTransform1 = CompositeType::New();
  CompositeType::Pointer compositeTransform2 = CompositeType::New();
  CompositeType::Pointer compositeTransform3 = CompositeType::New();
  CompositeType::Pointer compositeTransform4 = CompositeType::New();

  typedef itk::TranslationTransform<double, NDimensions>  TranslationTransformType;
  typedef TranslationTransformType::Pointer               TranslationTransformPointer;
  typedef std::vector<TranslationTransformPointer>        TranslationTransformVector;
  TranslationTransformVector  translationTransformVector(12);
  for( itk::SizeValueType n=0; n < 12; n++ )
    {
    translationTransformVector[n] = TranslationTransformType::New();
    TranslationTransformType::ParametersType params(NDimensions);
    params.Fill(n);
    translationTransformVector[n]->SetParameters( params );
    }

  compositeTransform1->AddTransform( translationTransformVector[0] );
  compositeTransform1->AddTransform( translationTransformVector[1] );
  compositeTransform1->AddTransform( translationTransformVector[2] );

  compositeTransform2->AddTransform( translationTransformVector[3] );
  compositeTransform2->AddTransform( translationTransformVector[4] );

  compositeTransform3->AddTransform( translationTransformVector[5] );
  compositeTransform3->AddTransform( translationTransformVector[6] );

  compositeTransform4->AddTransform( translationTransformVector[7] );
  compositeTransform4->AddTransform( translationTransformVector[8] );
  compositeTransform4->AddTransform( translationTransformVector[9] );
  compositeTransform4->AddTransform( compositeTransform3 );

  nestedCompositeTransform->AddTransform( compositeTransform1 );
  nestedCompositeTransform->AddTransform( translationTransformVector[10] );
  nestedCompositeTransform->AddTransform( compositeTransform2 );
  nestedCompositeTransform->AddTransform( compositeTransform4 );
  nestedCompositeTransform->AddTransform( translationTransformVector[11] );

  std::cout << "Number of transforms before flattening = " << nestedCompositeTransform->GetNumberOfTransforms() << std::endl;
  if( nestedCompositeTransform->GetNumberOfTransforms() != 5 )
    {
    std::cerr << "Error.  Should be 5." << std::endl;
    return EXIT_FAILURE;
    }

  nestedCompositeTransform->FlattenTransformQueue();
  std::cout << "Number of transforms after flattening = " << nestedCompositeTransform->GetNumberOfTransforms() << std::endl;
  if( nestedCompositeTransform->GetNumberOfTransforms() != 12 )
    {
    std::cerr << "Error.  Should be 12." << std::endl;
    return EXIT_FAILURE;
    }

  /* Verify the transform order */
  bool passed = true;
  for( itk::SizeValueType n=0; n < 12; n++ )
    {
    const TranslationTransformType::ParametersType & params = translationTransformVector[n]->GetParameters();
    if( itk::Math::NotExactlyEquals(params[0], n) )
      {
      passed = false;
      }
    }
  if( !passed )
    {
    std::cout << "Transform are not in correct order after flattening: " << std::endl;
    for( itk::SizeValueType n=0; n < 12; n++ )
      {
      const TranslationTransformType::ParametersType & params = translationTransformVector[n]->GetParameters();
      std::cout << " " << params[0];
      }
    std::cout << std::endl;
    return EXIT_FAILURE;
    }

  /* Test SetParameters with wrong size array */
  std::cout << "Test SetParameters with wrong size array." << std::endl;
  parametersTruth.SetSize(1);
  bool caught = false;
  try
    {
    compositeTransform->SetParameters( parametersTruth );
    }
  catch( itk::ExceptionObject & excp )
    {
    caught = true;
    std::cout << "\nCaught expected exception:" << std::endl;
    std::cout << excp << std::endl;
    }
  if( !caught )
    {
    std::cerr << "Expected exception calling SetParameters with wrong size"
              << std::endl;

    return EXIT_FAILURE;
    }

  /* Test printing */
  compositeTransform->Print(std::cout);

  std::cout << "Passed test!" << std::endl;
  return EXIT_SUCCESS;

}

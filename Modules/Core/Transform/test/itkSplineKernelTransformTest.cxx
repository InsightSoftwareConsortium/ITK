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
 * This tests the elastic body spline and thin plate spline
 * transform classes by warping a unit cube into a cube with side length 3.
 * It performs the test for 2D, 3D, and 4D to ensure that the
 * class works in N dimensions
 */
#include "itkElasticBodySplineKernelTransform.h"
#include "itkElasticBodyReciprocalSplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkThinPlateR2LogRSplineKernelTransform.h"
#include "itkVolumeSplineKernelTransform.h"
#include "itkMath.h"


int itkSplineKernelTransformTest(int , char* [] )
{

  const double epsilon = 1e-12;

  // 2-D case
  int i, j;

  typedef itk::ElasticBodySplineKernelTransform<double, 2>           EBSTransform2DType;
  typedef itk::ElasticBodyReciprocalSplineKernelTransform<double, 2> EBRSTransform2DType;
  typedef itk::ThinPlateSplineKernelTransform<double, 2>             TPSTransform2DType;
  typedef itk::ThinPlateR2LogRSplineKernelTransform<double, 2>       TPR2LRSTransform2DType;
  typedef itk::VolumeSplineKernelTransform<double, 2>                VSTransform2DType;

  typedef EBSTransform2DType::InputPointType PointType2D;
  typedef EBSTransform2DType::PointsIterator Points2DIteratorType;
  typedef EBSTransform2DType::PointSetType   PointSetType2D;

  PointType2D sourcePoint2D;
  PointType2D targetPoint2D;
  PointType2D mappedPoint2D;

  EBSTransform2DType::Pointer     ebs2D       = EBSTransform2DType::New();
  EBRSTransform2DType::Pointer    ebrs2D      = EBRSTransform2DType::New();
  TPSTransform2DType::Pointer     tps2D       = TPSTransform2DType::New();
  TPR2LRSTransform2DType::Pointer tpr2lrs2D   = TPR2LRSTransform2DType::New();
  VSTransform2DType::Pointer      vs2D        = VSTransform2DType::New();

  // Reserve memory for the number of points
  PointSetType2D::Pointer sourceLandmarks2D = PointSetType2D::New();
  PointSetType2D::Pointer targetLandmarks2D = PointSetType2D::New();

  sourceLandmarks2D->GetPoints()->Reserve( 4 );
  targetLandmarks2D->GetPoints()->Reserve( 4 );

  // Create landmark sets
  Points2DIteratorType source2Dit = sourceLandmarks2D->GetPoints()->Begin();
  Points2DIteratorType target2Dit = targetLandmarks2D->GetPoints()->Begin();

  Points2DIteratorType source2Dend = sourceLandmarks2D->GetPoints()->End();

  for (i = 0; i < 2; i++)
    {
    for (j = 0; j < 2; j++)
      {
      sourcePoint2D[0] = j;
      sourcePoint2D[1] = i;
      source2Dit.Value() = sourcePoint2D;
      targetPoint2D[0] = 3*j;
      targetPoint2D[1] = 3*i;
      target2Dit.Value() = targetPoint2D;
      source2Dit++;
      target2Dit++;
      }
    }


  std::cout << "EBS 2D Test:" << std::endl;
  // Poisson's ration = 0.25, Alpha = 12.0 * ( 1 - \nu ) - 1
  ebs2D->SetSourceLandmarks( sourceLandmarks2D );
  ebs2D->SetTargetLandmarks( targetLandmarks2D );
  ebs2D->SetAlpha( 12.0 * ( 1 -  0.25) - 1.0 );
  ebs2D->ComputeWMatrix();

  { // Testing the number of parameters
  EBSTransform2DType::ParametersType parameters1 = ebs2D->GetParameters();
  const unsigned int numberOfParameters = parameters1.Size();
  if( numberOfParameters != 4 * 2 )
    {
    std::cerr << "Number of parameters was not updated after" << std::endl;
    std::cerr << "invoking SetSourceLandmarks and SetTargetLandmarks" << std::endl;
    std::cerr << "Number of parameters is = " << numberOfParameters << std::endl;
    std::cerr << "While we were expecting = " << 4 * 2 << std::endl;
    return EXIT_FAILURE;
    }
  }

  source2Dit = sourceLandmarks2D->GetPoints()->Begin();
  target2Dit = targetLandmarks2D->GetPoints()->Begin();

  source2Dend = sourceLandmarks2D->GetPoints()->End();
  while( source2Dit != source2Dend )
    {
    sourcePoint2D = source2Dit.Value();
    targetPoint2D = target2Dit.Value();
    mappedPoint2D = ebs2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " : " << targetPoint2D;
    std::cout << " warps to: " << mappedPoint2D << std::endl;
    if( mappedPoint2D.EuclideanDistanceTo( targetPoint2D ) > epsilon )
      {
      return EXIT_FAILURE;
      }
    source2Dit++;
    target2Dit++;
    }
  std::cout << std::endl;


  std::cout << "EBRS 2D Test:" << std::endl;
  ebrs2D->SetSourceLandmarks( sourceLandmarks2D );
  ebrs2D->SetTargetLandmarks( targetLandmarks2D );
  ebrs2D->SetAlpha( 12.0 * ( 1 -  0.25) - 1.0 );
  ebrs2D->ComputeWMatrix();

  source2Dit = sourceLandmarks2D->GetPoints()->Begin();
  target2Dit = targetLandmarks2D->GetPoints()->Begin();

  source2Dend = sourceLandmarks2D->GetPoints()->End();
  while( source2Dit != source2Dend )
    {
    sourcePoint2D = source2Dit.Value();
    targetPoint2D = target2Dit.Value();
    mappedPoint2D = ebrs2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " : " << targetPoint2D;
    std::cout << " warps to: " << mappedPoint2D << std::endl;
    if( mappedPoint2D.EuclideanDistanceTo( targetPoint2D ) > epsilon )
      {
      return EXIT_FAILURE;
      }
    source2Dit++;
    target2Dit++;
    }
  std::cout << std::endl;

  std::cout << "TPS 2D Test:" << std::endl;
  tps2D->SetSourceLandmarks( sourceLandmarks2D );
  tps2D->SetTargetLandmarks( targetLandmarks2D );

  tps2D->ComputeWMatrix();

  source2Dit = sourceLandmarks2D->GetPoints()->Begin();
  target2Dit = targetLandmarks2D->GetPoints()->Begin();

  source2Dend = sourceLandmarks2D->GetPoints()->End();
  while( source2Dit != source2Dend )
    {
    sourcePoint2D = source2Dit.Value();
    targetPoint2D = target2Dit.Value();
    mappedPoint2D = tps2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " : " << targetPoint2D;
    std::cout << " warps to: " << mappedPoint2D << std::endl;
    if( mappedPoint2D.EuclideanDistanceTo( targetPoint2D ) > epsilon )
      {
      return EXIT_FAILURE;
      }
    source2Dit++;
    target2Dit++;
    }
  if ( tps2D->IsLinear() == true ) //NOTE TPS is never linear!
    {
    std::cout << "ERROR:  2D TPS reports as being a linear transform." << std::endl;
    return EXIT_FAILURE;
    }


  //NOTE: The following should set the default values explicitly
    {
    const double TestValue = 0.012345;
    tps2D->SetStiffness(TestValue); //This value should not change the result at all.

    if ( itk::Math::NotExactlyEquals(tps2D->GetStiffness(), TestValue) )
      {
      std::cout << "ERROR:  Explicitly set stiffness value not retained." << std::endl;
      return EXIT_FAILURE;
      }
    }
    { //Just for code coverage
    TPSTransform2DType::VectorSetType::ConstPointer tempDisplacements=tps2D->GetDisplacements();

      {
      TPSTransform2DType::InputVectorType  testVector;
      testVector[0] = 0.0;
      testVector[1] = 1.0;
      bool exceptionCaught=false;
      try
        {
        tps2D->TransformVector(testVector);
        }
      catch(...)
        {
        exceptionCaught = true;
        }
      if ( exceptionCaught != true )
        {
        return EXIT_FAILURE;
        }
      }
      {
      TPSTransform2DType::InputVnlVectorType  testVector;
      testVector[0] = 0.0;
      testVector[1] = 1.0;
      bool exceptionCaught=false;
      try
        {
        tps2D->TransformVector(testVector);
        }
      catch(...)
        {
        exceptionCaught = true;
        }
      if ( exceptionCaught != true )
        {
        return EXIT_FAILURE;
        }
      }
      {
      TPSTransform2DType::InputCovariantVectorType testVector;
      testVector[0] = 0.0;
      testVector[1] = 1.0;
      bool exceptionCaught=false;
      try
        {
        tps2D->TransformCovariantVector(testVector);
        }
      catch(...)
        {
        exceptionCaught = true;
        }
      if ( exceptionCaught != true )
        {
        return EXIT_FAILURE;
        }
      }
      {
      TPSTransform2DType::JacobianType   testJacobian;
      TPSTransform2DType::InputPointType testVector;
      testVector[0] = 0.0;
      testVector[1] = 1.0;
      bool exceptionCaught=false;
      try
        {
        tps2D->ComputeJacobianWithRespectToPosition(testVector,testJacobian);
        }
      catch(...)
        {
        exceptionCaught = true;
        }
      if ( exceptionCaught != true )
        {
        return EXIT_FAILURE;
        }
      }
    }

  std::cout << std::endl;

  std::cout << "TPR2LR 2D Test:" << std::endl;
  tpr2lrs2D->SetSourceLandmarks( sourceLandmarks2D );
  tpr2lrs2D->SetTargetLandmarks( targetLandmarks2D );

  tpr2lrs2D->ComputeWMatrix();

  source2Dit = sourceLandmarks2D->GetPoints()->Begin();
  target2Dit = targetLandmarks2D->GetPoints()->Begin();

  source2Dend = sourceLandmarks2D->GetPoints()->End();
  while( source2Dit != source2Dend )
    {
    sourcePoint2D = source2Dit.Value();
    targetPoint2D = target2Dit.Value();
    mappedPoint2D = tpr2lrs2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " : " << targetPoint2D;
    std::cout << " warps to: " << mappedPoint2D << std::endl;
    if( mappedPoint2D.EuclideanDistanceTo( targetPoint2D ) > epsilon )
      {
      return EXIT_FAILURE;
      }
    source2Dit++;
    target2Dit++;
    }
  std::cout << std::endl;

  // volume spline transform
  std::cout << "VS 2D Test:" << std::endl;
  vs2D->SetSourceLandmarks( sourceLandmarks2D );
  vs2D->SetTargetLandmarks( targetLandmarks2D );

  vs2D->ComputeWMatrix();

  source2Dit = sourceLandmarks2D->GetPoints()->Begin();
  target2Dit = targetLandmarks2D->GetPoints()->Begin();

  source2Dend = sourceLandmarks2D->GetPoints()->End();
  while( source2Dit != source2Dend )
    {
    sourcePoint2D = source2Dit.Value();
    targetPoint2D = target2Dit.Value();
    mappedPoint2D = vs2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " : " << targetPoint2D;
    std::cout << " warps to: " << mappedPoint2D << std::endl;
    if( mappedPoint2D.EuclideanDistanceTo( targetPoint2D ) > epsilon )
      {
      return EXIT_FAILURE;
      }
    source2Dit++;
    target2Dit++;
    }
  std::cout << std::endl;


   // 3-D case
  int k;
  typedef itk::ElasticBodySplineKernelTransform<double, 3> EBSTransform3DType;
  typedef itk::ThinPlateSplineKernelTransform<double, 3>   TPSTransform3DType;

  typedef EBSTransform3DType::InputPointType PointType3D;
  typedef EBSTransform3DType::PointsIterator Points3DIteratorType;

  PointType3D sourcePoint3D;
  PointType3D targetPoint3D;
  PointType3D mappedPoint3D;

  // Reserve memory for the number of points
  EBSTransform3DType::Pointer ebs3D = EBSTransform3DType::New();
  ebs3D->GetModifiableTargetLandmarks()->GetPoints()->Reserve( 8 );
  ebs3D->GetModifiableSourceLandmarks()->GetPoints()->Reserve( 8 );

  TPSTransform3DType::Pointer tps3D = TPSTransform3DType::New();
  tps3D->GetModifiableTargetLandmarks()->GetPoints()->Reserve( 8 );
  tps3D->GetModifiableSourceLandmarks()->GetPoints()->Reserve( 8 );


  // Create landmark sets
  Points3DIteratorType ebs3Ds = ebs3D->GetModifiableSourceLandmarks()->GetPoints()->Begin();
  Points3DIteratorType ebs3Dt = ebs3D->GetModifiableTargetLandmarks()->GetPoints()->Begin();
  Points3DIteratorType tps3Ds = tps3D->GetModifiableSourceLandmarks()->GetPoints()->Begin();
  Points3DIteratorType tps3Dt = tps3D->GetModifiableTargetLandmarks()->GetPoints()->Begin();

  Points3DIteratorType ebs3DsEnd  = ebs3D->GetModifiableSourceLandmarks()->GetPoints()->End();
  Points3DIteratorType tps3DsEnd  = tps3D->GetModifiableSourceLandmarks()->GetPoints()->End();

  for (i = 0; i < 2; i++)
    {
    for (j = 0; j < 2; j++)
      {
      for (k = 0; k < 2; k++)
        {
        sourcePoint3D[0] = k;
        sourcePoint3D[1] = j;
        sourcePoint3D[2] = i;
        ebs3Ds.Value() = sourcePoint3D;
        tps3Ds.Value() = sourcePoint3D;
        targetPoint3D[0] = 3*k;
        targetPoint3D[1] = 3*j;
        targetPoint3D[2] = 3*i;
        ebs3Dt.Value() = targetPoint3D;
        tps3Dt.Value() = targetPoint3D;
        ebs3Ds++;
        ebs3Dt++;
        tps3Ds++;
        tps3Dt++;
        }
      }
    }

  std::cout << "EBS 3D Test:" << std::endl;
  // Poisson's ration = 0.25, Alpha = 12.0 * ( 1 - \nu ) - 1
  ebs3D->SetAlpha( 12.0 * ( 1 -  0.25) - 1.0 );
  ebs3D->ComputeWMatrix();

  ebs3Ds     = ebs3D->GetModifiableSourceLandmarks()->GetPoints()->Begin();
  ebs3Dt     = ebs3D->GetModifiableTargetLandmarks()->GetPoints()->Begin();
  ebs3DsEnd  = ebs3D->GetModifiableSourceLandmarks()->GetPoints()->End();

  while( ebs3Ds != ebs3DsEnd )
  {
    sourcePoint3D = ebs3Ds.Value();
    targetPoint3D = ebs3Dt.Value();
    mappedPoint3D = ebs3D->TransformPoint(sourcePoint3D);
    std::cout << sourcePoint3D << " : " << targetPoint3D;
    std::cout << " warps to: " << mappedPoint3D << std::endl;
    if( mappedPoint3D.EuclideanDistanceTo( targetPoint3D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
    ebs3Ds++;
    ebs3Dt++;
  }
  std::cout << std::endl;

  std::cout << "TPS 3D Test:" << std::endl;

  tps3D->ComputeWMatrix();

  tps3Ds = tps3D->GetModifiableSourceLandmarks()->GetPoints()->Begin();
  tps3Dt = tps3D->GetModifiableTargetLandmarks()->GetPoints()->Begin();
  tps3DsEnd  = tps3D->GetModifiableSourceLandmarks()->GetPoints()->End();

  while( tps3Ds != tps3DsEnd )
  {
    sourcePoint3D = tps3Ds.Value();
    targetPoint3D = tps3Dt.Value();
    mappedPoint3D = tps3D->TransformPoint(sourcePoint3D);
    std::cout << sourcePoint3D << " : " << targetPoint3D;
    std::cout << " warps to: " << mappedPoint3D << std::endl;
    if( mappedPoint3D.EuclideanDistanceTo( targetPoint3D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
    tps3Ds++;
    tps3Dt++;
  }
  std::cout << std::endl;

  std::cout << "Get/Set Parameters test " << std::endl;
  TPSTransform3DType::ParametersType parameters1 = tps3D->GetParameters();
  tps3D->SetParameters( parameters1 );
  TPSTransform3DType::ParametersType parameters2 = tps3D->GetParameters();
  const unsigned int numberOfParameters = parameters1.Size();
  const double tolerance = 1e-7;
  for(unsigned int pr = 0; pr < numberOfParameters; pr++)
    {
    if( itk::Math::abs( parameters1[pr] - parameters2[pr] ) > tolerance )
      {
      std::cout << "Parameters were not correctly recovered " << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << "Get/Set Parameters Passed" << std::endl << std::endl;


  // 4-D case
  int l;
  typedef itk::ElasticBodySplineKernelTransform<double, 4> EBSTransform4DType;
  typedef itk::ThinPlateSplineKernelTransform<double, 4>   TPSTransform4DType;

  typedef EBSTransform4DType::InputPointType PointType4D;
  typedef EBSTransform4DType::PointsIterator Points4DIteratorType;

  PointType4D sourcePoint4D;
  PointType4D targetPoint4D;
  PointType4D mappedPoint4D;

  EBSTransform4DType::Pointer ebs4D = EBSTransform4DType::New();
  TPSTransform4DType::Pointer tps4D = TPSTransform4DType::New();

  // Reserve memory for the number of points
  ebs4D->GetModifiableTargetLandmarks()->GetPoints()->Reserve( 16 );
  tps4D->GetModifiableTargetLandmarks()->GetPoints()->Reserve( 16 );

  ebs4D->GetModifiableSourceLandmarks()->GetPoints()->Reserve( 16 );
  tps4D->GetModifiableSourceLandmarks()->GetPoints()->Reserve( 16 );

  // Create landmark sets
  Points4DIteratorType ebs4Ds = ebs4D->GetModifiableSourceLandmarks()->GetPoints()->Begin();
  Points4DIteratorType ebs4Dt = ebs4D->GetModifiableTargetLandmarks()->GetPoints()->Begin();
  Points4DIteratorType tps4Ds = tps4D->GetModifiableSourceLandmarks()->GetPoints()->Begin();
  Points4DIteratorType tps4Dt = tps4D->GetModifiableTargetLandmarks()->GetPoints()->Begin();

  Points4DIteratorType ebs4DsEnd  = ebs4D->GetModifiableSourceLandmarks()->GetPoints()->End();
  Points4DIteratorType tps4DsEnd  = tps4D->GetModifiableSourceLandmarks()->GetPoints()->End();

  for (i = 0; i < 2; i++)
    {
    for (j = 0; j < 2; j++)
      {
      for (k = 0; k < 2; k++)
        {
        for (l = 0; l < 2; l++)
          {
          sourcePoint4D[0] = l;
          sourcePoint4D[1] = k;
          sourcePoint4D[2] = j;
          sourcePoint4D[3] = i;
          ebs4Ds.Value() = sourcePoint4D;
          tps4Ds.Value() = sourcePoint4D;
          targetPoint4D[0] = 3*l;
          targetPoint4D[1] = 3*k;
          targetPoint4D[2] = 3*j;
          targetPoint4D[3] = 3*i;
          ebs4Dt.Value() = targetPoint4D;
          tps4Dt.Value() = targetPoint4D;
          ebs4Ds++;
          ebs4Dt++;
          tps4Ds++;
          tps4Dt++;
          }
        }
      }
    }
  std::cout << "EBS 4D Test:" << std::endl;
  // Poisson's ration = 0.25, Alpha = 12.0 * ( 1 - \nu ) - 1
  ebs4D->SetAlpha( 12.0 * ( 1 -  0.25) - 1.0 );
  ebs4D->ComputeWMatrix();

  ebs4Ds = ebs4D->GetModifiableSourceLandmarks()->GetPoints()->Begin();
  ebs4Dt = ebs4D->GetModifiableTargetLandmarks()->GetPoints()->Begin();
  ebs4DsEnd  = ebs4D->GetModifiableSourceLandmarks()->GetPoints()->End();

  while( ebs4Ds != ebs4DsEnd )
  {
    sourcePoint4D = ebs4Ds.Value();
    targetPoint4D = ebs4Dt.Value();
    mappedPoint4D = ebs4D->TransformPoint(sourcePoint4D);
    std::cout << sourcePoint4D << " : " << targetPoint4D;
    std::cout << " warps to: " << mappedPoint4D << std::endl;
    if( mappedPoint4D.EuclideanDistanceTo( targetPoint4D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
    ebs4Ds++;
    ebs4Dt++;
  }
  std::cout << std::endl;

  std::cout << "TPS 4D Test:" << std::endl;
  tps4D->ComputeWMatrix();

  tps4Ds = tps4D->GetModifiableSourceLandmarks()->GetPoints()->Begin();
  tps4Dt = tps4D->GetModifiableTargetLandmarks()->GetPoints()->Begin();
  tps4DsEnd  = tps4D->GetModifiableSourceLandmarks()->GetPoints()->End();
  while( tps4Ds != tps4DsEnd )
  {
    sourcePoint4D = tps4Ds.Value();
    targetPoint4D = tps4Dt.Value();
    mappedPoint4D = tps4D->TransformPoint(sourcePoint4D);
    std::cout << sourcePoint4D << " : " << targetPoint4D;
    std::cout << " warps to: " << mappedPoint4D << std::endl;
    if( mappedPoint4D.EuclideanDistanceTo( targetPoint4D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
    tps4Ds++;
    tps4Dt++;
  }
  std::cout << std::endl;

  std::cout << ebs2D << std::endl;

  std::cout << "TEST DONE" << std::endl;

  return EXIT_SUCCESS;

}

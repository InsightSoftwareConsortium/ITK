/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSplineKernelTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

/**
 * This tests the elastic body spline and thin plate spline 
 * transform classes by warping a unit cube into a cube with side length 3.
 * It performs the test for 2D, 3D, and 4D to ensure that the
 * class works in N dimensions
 */
#include "itkElasticBodySplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"

using namespace itk;

int itkSplineKernelTransformTest(int , char* [] )
{

  const double epsilon = 1e-12;
  
  // 2-D case
  int i, j;

  typedef ElasticBodySplineKernelTransform<double, 2>   EBSTransform2DType;
  typedef ThinPlateSplineKernelTransform<double, 2>     TPSTransform2DType;

  typedef EBSTransform2DType::InputPointType PointType2D;
  typedef EBSTransform2DType::PointsIterator Points2DIteratorType;

  PointType2D sourcePoint2D;
  PointType2D targetPoint2D;
  PointType2D mappedPoint2D;

  EBSTransform2DType::Pointer ebs2D = EBSTransform2DType::New();
  TPSTransform2DType::Pointer tps2D = TPSTransform2DType::New();

  // Reserve memory for the number of points
  ebs2D->GetTargetLandmarks()->GetPoints()->Reserve( 4 );  
  tps2D->GetTargetLandmarks()->GetPoints()->Reserve( 4 );
  
  ebs2D->GetSourceLandmarks()->GetPoints()->Reserve( 4 );  
  tps2D->GetSourceLandmarks()->GetPoints()->Reserve( 4 );

  // Create landmark sets
  Points2DIteratorType ebs2Ds = ebs2D->GetSourceLandmarks()->GetPoints()->Begin();
  Points2DIteratorType ebs2Dt = ebs2D->GetTargetLandmarks()->GetPoints()->Begin();
  Points2DIteratorType tps2Ds = tps2D->GetSourceLandmarks()->GetPoints()->Begin();
  Points2DIteratorType tps2Dt = tps2D->GetTargetLandmarks()->GetPoints()->Begin();
  
  Points2DIteratorType ebs2DsEnd  = ebs2D->GetSourceLandmarks()->GetPoints()->End();
  Points2DIteratorType tps2DsEnd  = tps2D->GetSourceLandmarks()->GetPoints()->End();
  
  for (i = 0; i < 2; i++)
    {
    for (j = 0; j < 2; j++)
      {
      sourcePoint2D[0] = j;
      sourcePoint2D[1] = i;
      ebs2Ds.Value() = sourcePoint2D;
      tps2Ds.Value() = sourcePoint2D;
      targetPoint2D[0] = 3*j;
      targetPoint2D[1] = 3*i;
      ebs2Dt.Value() = targetPoint2D;
      tps2Dt.Value() = targetPoint2D;
      ebs2Ds++;
      ebs2Dt++;
      tps2Ds++;
      tps2Dt++;
      }
    }

  
  std::cout << "EBS 2D Test:" << std::endl;
  // Poisson's ration = 0.25, Alpha = 12.0 * ( 1 - \nu ) - 1
  ebs2D->SetAlpha( 12.0 * ( 1 -  0.25) - 1.0 );
  ebs2D->ComputeWMatrix();
  ebs2Ds = ebs2D->GetSourceLandmarks()->GetPoints()->Begin();
  ebs2Dt = ebs2D->GetTargetLandmarks()->GetPoints()->Begin();
  ebs2DsEnd  = ebs2D->GetSourceLandmarks()->GetPoints()->End();
  while( ebs2Ds != ebs2DsEnd )
  {
    sourcePoint2D = ebs2Ds.Value();
    targetPoint2D = ebs2Dt.Value();
    mappedPoint2D = ebs2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " : " << targetPoint2D;
    std::cout << " warps to: " << mappedPoint2D << std::endl;
    if( mappedPoint2D.EuclideanDistanceTo( targetPoint2D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
    ebs2Ds++;
    ebs2Dt++;
  }
  std::cout << std::endl;


 
  std::cout << "TPS 2D Test:" << std::endl;
  tps2D->ComputeWMatrix();
  tps2Ds = tps2D->GetSourceLandmarks()->GetPoints()->Begin();
  tps2Dt = tps2D->GetTargetLandmarks()->GetPoints()->Begin();
  tps2DsEnd  = tps2D->GetSourceLandmarks()->GetPoints()->End();
  while( tps2Ds != tps2DsEnd )
  {
    sourcePoint2D = tps2Ds.Value();
    targetPoint2D = tps2Dt.Value();
    mappedPoint2D = tps2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " : " << targetPoint2D;
    std::cout << " warps to: " << mappedPoint2D << std::endl;
    if( mappedPoint2D.EuclideanDistanceTo( targetPoint2D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
    tps2Ds++;
    tps2Dt++;
  }
  std::cout << std::endl;



   // 3-D case
  int k;
  typedef ElasticBodySplineKernelTransform<double, 3> EBSTransform3DType;
  typedef ThinPlateSplineKernelTransform<double, 3>   TPSTransform3DType;

  typedef EBSTransform3DType::InputPointType PointType3D;
  typedef EBSTransform3DType::PointsIterator Points3DIteratorType;

  PointType3D sourcePoint3D;
  PointType3D targetPoint3D;
  PointType3D mappedPoint3D;

  EBSTransform3DType::Pointer ebs3D = EBSTransform3DType::New();
  TPSTransform3DType::Pointer tps3D = TPSTransform3DType::New();

  // Reserve memory for the number of points
  ebs3D->GetTargetLandmarks()->GetPoints()->Reserve( 8 );  
  tps3D->GetTargetLandmarks()->GetPoints()->Reserve( 8 );
  ebs3D->GetSourceLandmarks()->GetPoints()->Reserve( 8 );  
  tps3D->GetSourceLandmarks()->GetPoints()->Reserve( 8 );


  // Create landmark sets
  Points3DIteratorType ebs3Ds = ebs3D->GetSourceLandmarks()->GetPoints()->Begin();
  Points3DIteratorType ebs3Dt = ebs3D->GetTargetLandmarks()->GetPoints()->Begin();
  Points3DIteratorType tps3Ds = tps3D->GetSourceLandmarks()->GetPoints()->Begin();
  Points3DIteratorType tps3Dt = tps3D->GetTargetLandmarks()->GetPoints()->Begin();

  Points3DIteratorType ebs3DsEnd  = ebs3D->GetSourceLandmarks()->GetPoints()->End();
  Points3DIteratorType tps3DsEnd  = tps3D->GetSourceLandmarks()->GetPoints()->End();
  
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
  
  ebs3Ds     = ebs3D->GetSourceLandmarks()->GetPoints()->Begin();
  ebs3Dt     = ebs3D->GetTargetLandmarks()->GetPoints()->Begin();
  ebs3DsEnd  = ebs3D->GetSourceLandmarks()->GetPoints()->End();

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

  tps3Ds = tps3D->GetSourceLandmarks()->GetPoints()->Begin();
  tps3Dt = tps3D->GetTargetLandmarks()->GetPoints()->Begin();
  tps3DsEnd  = tps3D->GetSourceLandmarks()->GetPoints()->End();

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
    if( vnl_math_abs( parameters1[pr] - parameters2[pr] ) > tolerance )
      {
      std::cout << "Parameters were not correctly recovered " << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << "Get/Set Parameters Passed" << std::endl << std::endl;


  // 4-D case
  int l;
  typedef ElasticBodySplineKernelTransform<double, 4> EBSTransform4DType;
  typedef ThinPlateSplineKernelTransform<double, 4>   TPSTransform4DType;
  
  typedef EBSTransform4DType::InputPointType PointType4D;
  typedef EBSTransform4DType::PointsIterator Points4DIteratorType;

  PointType4D sourcePoint4D;
  PointType4D targetPoint4D;
  PointType4D mappedPoint4D;

  EBSTransform4DType::Pointer ebs4D = EBSTransform4DType::New();
  TPSTransform4DType::Pointer tps4D = TPSTransform4DType::New();

  // Reserve memory for the number of points
  ebs4D->GetTargetLandmarks()->GetPoints()->Reserve( 16 );  
  tps4D->GetTargetLandmarks()->GetPoints()->Reserve( 16 );
  
  ebs4D->GetSourceLandmarks()->GetPoints()->Reserve( 16 );  
  tps4D->GetSourceLandmarks()->GetPoints()->Reserve( 16 );

  // Create landmark sets
  Points4DIteratorType ebs4Ds = ebs4D->GetSourceLandmarks()->GetPoints()->Begin();
  Points4DIteratorType ebs4Dt = ebs4D->GetTargetLandmarks()->GetPoints()->Begin();
  Points4DIteratorType tps4Ds = tps4D->GetSourceLandmarks()->GetPoints()->Begin();
  Points4DIteratorType tps4Dt = tps4D->GetTargetLandmarks()->GetPoints()->Begin();
  
  Points4DIteratorType ebs4DsEnd  = ebs4D->GetSourceLandmarks()->GetPoints()->End();
  Points4DIteratorType tps4DsEnd  = tps4D->GetSourceLandmarks()->GetPoints()->End();
 
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

  ebs4Ds = ebs4D->GetSourceLandmarks()->GetPoints()->Begin();
  ebs4Dt = ebs4D->GetTargetLandmarks()->GetPoints()->Begin();
  ebs4DsEnd  = ebs4D->GetSourceLandmarks()->GetPoints()->End();

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

  tps4Ds = tps4D->GetSourceLandmarks()->GetPoints()->Begin();
  tps4Dt = tps4D->GetTargetLandmarks()->GetPoints()->Begin();
  tps4DsEnd  = tps4D->GetSourceLandmarks()->GetPoints()->End();
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

  return EXIT_SUCCESS;

}


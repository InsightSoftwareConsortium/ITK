/**
 * This tests the elastic body spline and thin plate spline 
 * transform classes by warping a unit cube into a cube with side length 3.
 * It performs the test for 2D, 3D, and 4D to ensure that the
 * class works in N dimensions
 */
#include "itkElasticBodySplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"

using namespace itk;

int main(int argc, char* argv[])
{

  const double epsilon = 1e-14;
  
  int pointNumber;

  // 2-D case
  int i, j;
  typedef ElasticBodySplineKernelTransform<double, 2> EBSTransform2DType;
  typedef ThinPlateSplineKernelTransform<double, 2> TPSTransform2DType;

  typedef EBSTransform2DType::InputPointType PointType2D;

  PointType2D sourcePoint2D;
  PointType2D targetPoint2D;
  PointType2D mappedPoint2D;

  EBSTransform2DType::Pointer ebs2D = EBSTransform2DType::New();
  TPSTransform2DType::Pointer tps2D = TPSTransform2DType::New();

  // Create landmark sets
  pointNumber = 0;
  for (i = 0; i < 2; i++)
    {
    for (j = 0; j < 2; j++)
      {
      sourcePoint2D[0] = j;
      sourcePoint2D[1] = i;
      ebs2D->GetSourceLandmarks()->SetPoint(pointNumber, sourcePoint2D);
      tps2D->GetSourceLandmarks()->SetPoint(pointNumber, sourcePoint2D);
      targetPoint2D[0] = 3*j;
      targetPoint2D[1] = 3*i;
      ebs2D->GetTargetLandmarks()->SetPoint(pointNumber, targetPoint2D);
      tps2D->GetTargetLandmarks()->SetPoint(pointNumber, targetPoint2D);
      pointNumber++;
      }
    }
  std::cout << "EBS 2D Test:" << std::endl;
  ebs2D->SetAlpha(0.25);
  ebs2D->ComputeWMatrix();
  for (i = 0; i < 4; i++)
  {
    ebs2D->GetSourceLandmarks()->GetPoint(i, &sourcePoint2D);
    ebs2D->GetTargetLandmarks()->GetPoint(i, &targetPoint2D);
    mappedPoint2D = ebs2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " warps to: " << 
      mappedPoint2D << std::endl;
    if( mappedPoint2D.EuclideanDistanceTo( targetPoint2D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
  }
  std::cout << std::endl;
  
  std::cout << "TPS 2D Test:" << std::endl;
  tps2D->ComputeWMatrix();
  for (i = 0; i < 4; i++)
  {
    tps2D->GetSourceLandmarks()->GetPoint(i, &sourcePoint2D);
    tps2D->GetTargetLandmarks()->GetPoint(i, &targetPoint2D);
    mappedPoint2D = tps2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " warps to: " << 
      mappedPoint2D << std::endl;
    if( mappedPoint2D.EuclideanDistanceTo( targetPoint2D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
  }
  std::cout << std::endl;

  // 3-D case
  int k;
  typedef ElasticBodySplineKernelTransform<double, 3> EBSTransform3DType;
  typedef ThinPlateSplineKernelTransform<double, 3> TPSTransform3DType;

  typedef EBSTransform3DType::InputPointType PointType3D;
  PointType3D sourcePoint3D;
  PointType3D targetPoint3D;
  PointType3D mappedPoint3D;

  EBSTransform3DType::Pointer ebs3D = EBSTransform3DType::New();
  TPSTransform3DType::Pointer tps3D = TPSTransform3DType::New();

  // Create landmark sets
  pointNumber = 0;
  for (i = 0; i < 2; i++)
    {
    for (j = 0; j < 2; j++)
      {
      for (k = 0; k < 2; k++)
        {
        sourcePoint3D[0] = k;
        sourcePoint3D[1] = j;
        sourcePoint3D[2] = i;
        ebs3D->GetSourceLandmarks()->SetPoint(pointNumber, sourcePoint3D);
        tps3D->GetSourceLandmarks()->SetPoint(pointNumber, sourcePoint3D);
        targetPoint3D[0] = 3*k;
        targetPoint3D[1] = 3*j;
        targetPoint3D[2] = 3*i;
        ebs3D->GetTargetLandmarks()->SetPoint(pointNumber, targetPoint3D);
        tps3D->GetTargetLandmarks()->SetPoint(pointNumber, targetPoint3D);
        pointNumber++;
        }
      }
    }
  std::cout << "EBS 3D Test:" << std::endl;
  ebs3D->SetAlpha(0.25);
  ebs3D->ComputeWMatrix();
  for (i = 0; i < 8; i++)
  {
    ebs3D->GetSourceLandmarks()->GetPoint(i, &sourcePoint3D);
    ebs3D->GetTargetLandmarks()->GetPoint(i, &targetPoint3D);
    mappedPoint3D = ebs3D->TransformPoint(sourcePoint3D);
    std::cout << sourcePoint3D << " warps to: " << 
      mappedPoint3D << std::endl;
    if( mappedPoint3D.EuclideanDistanceTo( targetPoint3D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
  }
  std::cout << std::endl;
  
  std::cout << "TPS 3D Test:" << std::endl;
  tps3D->ComputeWMatrix();
  for (i = 0; i < 8; i++)
  {
    tps3D->GetSourceLandmarks()->GetPoint(i, &sourcePoint3D);
    tps3D->GetTargetLandmarks()->GetPoint(i, &targetPoint3D);
    mappedPoint3D = tps3D->TransformPoint(sourcePoint3D);
    std::cout << sourcePoint3D << " warps to: " << 
      mappedPoint3D << std::endl;
    if( mappedPoint3D.EuclideanDistanceTo( targetPoint3D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
  }
  std::cout << std::endl;

  // 4-D case
  int l;
  typedef ElasticBodySplineKernelTransform<double, 4> EBSTransform4DType;
  typedef ThinPlateSplineKernelTransform<double, 4> TPSTransform4DType;
  
  typedef EBSTransform4DType::InputPointType PointType4D;
  PointType4D sourcePoint4D;
  PointType4D targetPoint4D;
  PointType4D mappedPoint4D;

  EBSTransform4DType::Pointer ebs4D = EBSTransform4DType::New();
  TPSTransform4DType::Pointer tps4D = TPSTransform4DType::New();

  // Create landmark sets
  pointNumber = 0;
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
          ebs4D->GetSourceLandmarks()->SetPoint(pointNumber, sourcePoint4D);
          tps4D->GetSourceLandmarks()->SetPoint(pointNumber, sourcePoint4D);
          targetPoint4D[0] = 3*l;
          targetPoint4D[1] = 3*k;
          targetPoint4D[2] = 3*j;
          targetPoint4D[3] = 3*i;
          ebs4D->GetTargetLandmarks()->SetPoint(pointNumber, targetPoint4D);
          tps4D->GetTargetLandmarks()->SetPoint(pointNumber, targetPoint4D);
          pointNumber++;
          }
        }
      }
    }
  std::cout << "EBS 4D Test:" << std::endl;
  ebs4D->SetAlpha(0.25);
  ebs4D->ComputeWMatrix();
  for (i = 0; i < 16; i++)
  {
    ebs4D->GetSourceLandmarks()->GetPoint(i, &sourcePoint4D);
    ebs4D->GetTargetLandmarks()->GetPoint(i, &targetPoint4D);
    mappedPoint4D = ebs4D->TransformPoint(sourcePoint4D);
    std::cout << sourcePoint4D << " warps to: " << 
      mappedPoint4D << std::endl;
    if( mappedPoint4D.EuclideanDistanceTo( targetPoint4D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
  }
  std::cout << std::endl;

  std::cout << "TPS 4D Test:" << std::endl;
  tps4D->ComputeWMatrix();
  for (i = 0; i < 16; i++)
  {
    tps4D->GetSourceLandmarks()->GetPoint(i, &sourcePoint4D);
    tps4D->GetTargetLandmarks()->GetPoint(i, &targetPoint4D);
    mappedPoint4D = tps4D->TransformPoint(sourcePoint4D);
    std::cout << sourcePoint4D << " warps to: " << 
      mappedPoint4D << std::endl;
    if( mappedPoint4D.EuclideanDistanceTo( targetPoint4D ) > epsilon )
    {
      return EXIT_FAILURE;
    }
  }
  std::cout << std::endl;

  return EXIT_SUCCESS;

}


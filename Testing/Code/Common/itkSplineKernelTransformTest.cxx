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
  int pointNumber;

  // 2-D case
  int i, j;
  typedef ElasticBodySplineKernelTransform<double, 2> EBSTransform2DType;
  typedef ThinPlateSplineKernelTransform<double, 2> TPSTransform2DType;
  EBSTransform2DType* ebs2D;
  TPSTransform2DType* tps2D;
  typedef EBSTransform2DType::PointType PointType2D;
  PointType2D sourcePoint2D;
  PointType2D targetPoint2D;

  ebs2D = new EBSTransform2DType();
  tps2D = new TPSTransform2DType();

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
    targetPoint2D = ebs2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " warps to: " << 
      targetPoint2D << std::endl;
    }
  std::cout << std::endl;
  
  std::cout << "TPS 2D Test:" << std::endl;
  tps2D->ComputeWMatrix();
  for (i = 0; i < 4; i++)
    {
    tps2D->GetSourceLandmarks()->GetPoint(i, &sourcePoint2D);
    targetPoint2D = tps2D->TransformPoint(sourcePoint2D);
    std::cout << sourcePoint2D << " warps to: " << 
      targetPoint2D << std::endl;
    }
  std::cout << std::endl;
  delete ebs2D;
  delete tps2D;

  // 3-D case
  int k;
  typedef ElasticBodySplineKernelTransform<double, 3> EBSTransform3DType;
  typedef ThinPlateSplineKernelTransform<double, 3> TPSTransform3DType;
  EBSTransform3DType* ebs3D;
  TPSTransform3DType* tps3D;
  typedef EBSTransform3DType::PointType PointType3D;
  PointType3D sourcePoint3D;
  PointType3D targetPoint3D;

  ebs3D = new EBSTransform3DType();
  tps3D = new TPSTransform3DType();
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
    targetPoint3D = ebs3D->TransformPoint(sourcePoint3D);
    std::cout << sourcePoint3D << " warps to: " << 
      targetPoint3D << std::endl;
    }
  std::cout << std::endl;
  
  std::cout << "TPS 3D Test:" << std::endl;
  tps3D->ComputeWMatrix();
  for (i = 0; i < 8; i++)
    {
    tps3D->GetSourceLandmarks()->GetPoint(i, &sourcePoint3D);
    targetPoint3D = tps3D->TransformPoint(sourcePoint3D);
    std::cout << sourcePoint3D << " warps to: " << 
      targetPoint3D << std::endl;
    }
  std::cout << std::endl;
  delete ebs3D;
  delete tps3D;

  // 4-D case
  int l;
  typedef ElasticBodySplineKernelTransform<double, 4> EBSTransform4DType;
  typedef ThinPlateSplineKernelTransform<double, 4> TPSTransform4DType;
  EBSTransform4DType* ebs4D;
  TPSTransform4DType* tps4D;
  typedef EBSTransform4DType::PointType PointType4D;
  PointType4D sourcePoint4D;
  PointType4D targetPoint4D;

  ebs4D = new EBSTransform4DType();
  tps4D = new TPSTransform4DType();
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
    targetPoint4D = ebs4D->TransformPoint(sourcePoint4D);
    std::cout << sourcePoint4D << " warps to: " << 
      targetPoint4D << std::endl;
    }
  std::cout << std::endl;

  std::cout << "TPS 4D Test:" << std::endl;
  tps4D->ComputeWMatrix();
  for (i = 0; i < 16; i++)
    {
    tps4D->GetSourceLandmarks()->GetPoint(i, &sourcePoint4D);
    targetPoint4D = tps4D->TransformPoint(sourcePoint4D);
    std::cout << sourcePoint4D << " warps to: " << 
      targetPoint4D << std::endl;
    }
  std::cout << std::endl;
  delete ebs4D;
  delete tps4D;

  return 0;
}


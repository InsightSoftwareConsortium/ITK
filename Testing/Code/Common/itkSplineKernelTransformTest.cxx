/**
 * This tests the elastic body spline and thin plate spline 
 * transform classes by warping a unit cube into a cube with side length 3.
 * It performs the test for 2D, 3D, and 4D to ensure that the
 * class works in N dimensions
 */
#include "itkElasticBodySplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"

using namespace itk;
//using namespace std;

int main(int argc, char* argv[])
{

	// 2-D case
	int i, j;
	typedef ElasticBodySplineKernelTransform<double, 2> EBSTransform2DType;
  typedef ThinPlateSplineKernelTransform<double, 2> TPSTransform2DType;
	EBSTransform2DType::Pointer ebs2D;
  TPSTransform2DType::Pointer tps2D;
	typedef EBSTransform2DType::PointType PointType2D;
	PointType2D* sourcePoint2D;
	PointType2D* targetPoint2D;

	ebs2D = EBSTransform2DType::New();
  tps2D = TPSTransform2DType::New();
	// Create landmark sets
	for (i = 0; i < 2; i++)
	{
		for (j = 0; j < 2; j++)
		{
			sourcePoint2D = new PointType2D;
			(*sourcePoint2D)[0] = j;
			(*sourcePoint2D)[1] = i;
			ebs2D->Getp()->push_back(sourcePoint2D);
      tps2D->Getp()->push_back(sourcePoint2D);
			targetPoint2D = new PointType2D;
			(*targetPoint2D)[0] = 3*j;
			(*targetPoint2D)[1] = 3*i;
			ebs2D->Getq()->push_back(targetPoint2D);
      tps2D->Getq()->push_back(targetPoint2D);
		}
	}
	std::cout << "EBS 2D Test:" << std::endl;
	ebs2D->SetAlpha(0.25);
	ebs2D->ComputeW();
	for (i = 0; i < 4; i++)
	{
		std::cout << *((*ebs2D->Getp())[i]) << " warps to: " << 
						ebs2D->Transform(*((*ebs2D->Getp())[i])) << std::endl;
	}
	std::cout << std::endl;

	std::cout << "TPS 2D Test:" << std::endl;
	tps2D->ComputeW();
	for (i = 0; i < 4; i++)
	{
		std::cout << *((*tps2D->Getp())[i]) << " warps to: " << 
						tps2D->Transform(*((*tps2D->Getp())[i])) << std::endl;
	}
	std::cout << std::endl;

	// 3-D case
	int k;
	typedef ElasticBodySplineKernelTransform<double, 3> EBSTransform3DType;
  typedef ThinPlateSplineKernelTransform<double, 3> TPSTransform3DType;
	EBSTransform3DType::Pointer ebs3D;
  TPSTransform3DType::Pointer tps3D;
	typedef EBSTransform3DType::PointType PointType3D;
	PointType3D* sourcePoint3D;
	PointType3D* targetPoint3D;

	ebs3D = EBSTransform3DType::New();
  tps3D = TPSTransform3DType::New();
	// Create landmark sets
	for (i = 0; i < 2; i++)
	{
		for (j = 0; j < 2; j++)
		{
			for (k = 0; k < 2; k++)
			{
				sourcePoint3D = new PointType3D;
				(*sourcePoint3D)[0] = k;
				(*sourcePoint3D)[1] = j;
				(*sourcePoint3D)[2] = i;
				ebs3D->Getp()->push_back(sourcePoint3D);
        tps3D->Getp()->push_back(sourcePoint3D);
				targetPoint3D = new PointType3D;
				(*targetPoint3D)[0] = 3*k;
				(*targetPoint3D)[1] = 3*j;
				(*targetPoint3D)[2] = 3*i;
				ebs3D->Getq()->push_back(targetPoint3D);
				tps3D->Getq()->push_back(targetPoint3D);
			}
		}
	}
	std::cout << "EBS 3D Test:" << std::endl;
	ebs3D->SetAlpha(0.25);
	ebs3D->ComputeW();
	for (i = 0; i < 8; i++)
	{
		std::cout << *((*ebs3D->Getp())[i]) << " warps to: " << 
						ebs3D->Transform(*((*ebs3D->Getp())[i])) << std::endl;
	}
	std::cout << std::endl;

	std::cout << "TPS 3D Test:" << std::endl;
	tps3D->ComputeW();
	for (i = 0; i < 8; i++)
	{
		std::cout << *((*tps3D->Getp())[i]) << " warps to: " << 
						tps3D->Transform(*((*tps3D->Getp())[i])) << std::endl;
	}
	std::cout << std::endl;

	// 4-D case
	int l;
	typedef ElasticBodySplineKernelTransform<double, 4> EBSTransform4DType;
	EBSTransform4DType::Pointer ebs4D;
	typedef ThinPlateSplineKernelTransform<double, 4> TPSTransform4DType;
	TPSTransform4DType::Pointer tps4D;
	typedef EBSTransform4DType::PointType PointType4D;
	PointType4D* sourcePoint4D;
	PointType4D* targetPoint4D;

	ebs4D = EBSTransform4DType::New();
  tps4D = TPSTransform4DType::New();
	// Create landmark sets
	for (i = 0; i < 2; i++)
	{
		for (j = 0; j < 2; j++)
		{
			for (k = 0; k < 2; k++)
			{
				for (l = 0; l < 2; l++)
				{
					sourcePoint4D = new PointType4D;
					(*sourcePoint4D)[0] = l;
					(*sourcePoint4D)[1] = k;
					(*sourcePoint4D)[2] = j;
					(*sourcePoint4D)[3] = i;
					ebs4D->Getp()->push_back(sourcePoint4D);
					tps4D->Getp()->push_back(sourcePoint4D);
					targetPoint4D = new PointType4D;
					(*targetPoint4D)[0] = 3*l;
					(*targetPoint4D)[1] = 3*k;
					(*targetPoint4D)[2] = 3*j;
					(*targetPoint4D)[3] = 3*i;
					ebs4D->Getq()->push_back(targetPoint4D);
					tps4D->Getq()->push_back(targetPoint4D);
				}
			}
		}
	}
	std::cout << "EBS 4D Test:" << std::endl;
	ebs4D->SetAlpha(0.25);
	ebs4D->ComputeW();
	for (i = 0; i < 16; i++)
	{
		std::cout << *((*ebs4D->Getp())[i]) << " warps to: " << 
						ebs4D->Transform(*((*ebs4D->Getp())[i])) << std::endl;
	}
	std::cout << std::endl;

	std::cout << "TPS 4D Test:" << std::endl;
	tps4D->ComputeW();
	for (i = 0; i < 16; i++)
	{
		std::cout << *((*tps4D->Getp())[i]) << " warps to: " << 
						tps4D->Transform(*((*tps4D->Getp())[i])) << std::endl;
	}
	std::cout << std::endl;

	return 0;
}

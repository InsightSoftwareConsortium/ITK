#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkSphereSource.h"
#include <iostream>
#include <string>
#include <math.h>

int main(){

	typedef itk::SphereSource<itk::Mesh<float> >	fSphereSourceType;
	fSphereSourceType::Pointer	mySphereSource = fSphereSourceType::New();

	mySphereSource->SetCenter(0, 0, 0);
	mySphereSource->SetResolution(1, 10);
	mySphereSource->SetScale(1.0, 1.0, 1.0);

	mySphereSource->Modified();
	mySphereSource->Update();

	typedef itk::Mesh<float>::PointType		IPT;
//	itk::Mesh<float>::PointsContainerPointer      myoutput = mySphereSource->GetOutput()->GetPoints();
//	itk::Mesh<float>::PointsContainer::Iterator   m_output = myoutput->Begin();

	IPT*	pt_ptr;
	IPT		pt;
	pt_ptr = &pt;

  std::cout << "Testing itk::SphereSource "<< std::endl;
  for(int i=0; i<12; i++) {
	mySphereSource->GetOutput()->GetPoint(i, pt_ptr);
	std::cout << "Point1: " << pt[0] << ", " << pt[1] << ", "<< pt[2] << std::endl;
  }
  std::cout << "Test End "<< std::endl;
  return 0;

}


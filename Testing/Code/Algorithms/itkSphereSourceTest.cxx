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

	typedef itk::SphereSource<itk::Mesh<float>>	fSphereSourceType;
	fSPhereSourceType::Pointer	mySphereSource = fSphereSourceType::New();

	mySphereSource->SetCenter(0, 0, 0);
	mySphereSource->SetResolution(1, 10);
	mySphereSource->SetScalar(1.0, 1.0, 1.0);

	mySphereSource->Modified();
	mySphereSource->Update();

  
  return 0;

}


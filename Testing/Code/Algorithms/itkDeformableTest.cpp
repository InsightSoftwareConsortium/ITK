#include <iostream>
#include <string>
#include <math.h>

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkDeformableMeshTest.h"
#include "itkBalloonForceFilter.h"


using namespace itk;

typedef itk::DeformableMesh<float>  DMesh;
typedef itk::Mesh<float>  MyMesh;
typedef itk::BalloonForceFilter<MyMesh, MyMesh> BFilter;

int main(void)
{
  DMesh::Pointer m_Mesh(DMesh::New());
  BFilter::Pointer m_Filter = BFilter::New();
  MyMesh::Pointer force(MyMesh::New());
  MyMesh::Pointer displace(MyMesh::New());
  MyMesh::Pointer derive(MyMesh::New());
  
  m_Mesh->SetDefault();
  m_Mesh->SetCenter(0, 0, 0);
  m_Mesh->SetResolution(4, 9);
  m_Mesh->SetScale(1.0, 1.0, 1.0);
  m_Mesh->Allocate();
/*
  m_Filter->SetInput(m_Mesh);
  m_Filter->SetResolution(4, 9, 1);
  m_Filter->SetForces(force);
  m_Filter->SetDisplacements(displace);
  m_Filter->SetDerives(derive);
  m_Filter->Initialize();
  m_Filter->SetStiffnessMatrix();
  m_Filter->ComputeForce();
  m_Filter->ComputeDt();
  m_Filter->Advance();
  m_Filter->ComputeOutput();
*/  return 0;
}
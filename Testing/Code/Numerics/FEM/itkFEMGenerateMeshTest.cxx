/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMGenerateMeshTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMGenerateMesh.h"
#include "itkFEMElement2DC0LinearQuadrilateralStrain.h"
#include "itkFEMMaterialLinearElasticity.h"
#include "itkExceptionObject.h"
#include "itkFEMElement3DC0LinearHexahedronStrain.h"

using namespace std;
using namespace itk;
using namespace fem;

//
int itkFEMGenerateMeshTest(int, char*[])
{
  // Generate2DRectilinearMesh(m_Element,mySolver,MeshOriginV,MeshSizeV,ElementsPerDim);
  // Generate3DRectilinearMesh(m_Element,mySolver,MeshOriginV,MeshSizeV,ElementsPerDim);

  // Set up the solver object
  Solver S;
  LinearSystemWrapperVNL lsw;

  S.SetLinearSystemWrapper(&lsw);

  // Set up mesh dimensions
  vnl_vector<double> MeshOriginV;
  vnl_vector<double> MeshSizeV;
  vnl_vector<double> ElementsPerDim;

  MeshOriginV.resize(2);
  MeshSizeV.resize(2);
  ElementsPerDim.resize(2);

  for (unsigned int j=0; j<2; j++) {
      MeshOriginV[j]=0.0;
      MeshSizeV[j]=10;
      ElementsPerDim[j]=5.0;
  }

  // Create the material
  MaterialLinearElasticity::Pointer m;
  m = MaterialLinearElasticity::New();
  m->GN = 0;
  m->E = 1000.;
  m->A = 1.0;
  m->h = 1.0;
  m->I = 1.0;
  m->nu = 0.4;
  m->RhoC = 1.0;

  // Create the element type
  Element2DC0LinearQuadrilateralStrain::Pointer e1=Element2DC0LinearQuadrilateralStrain::New();
  e1->m_mat=dynamic_cast<MaterialLinearElasticity*>( m );

  try {
      Generate2DRectilinearMesh(e1,S,MeshOriginV,MeshSizeV,ElementsPerDim);
      cout << "Generated 2D rectilinear mesh\n";
  }
  catch (ExceptionObject&) {
      cout << "Could not generate 2D mesh - test FAILED\n";
      return EXIT_FAILURE;
  }

  MeshOriginV.resize(3);
  MeshSizeV.resize(3);
  ElementsPerDim.resize(3);

  for (unsigned int j=0;j<3;j++) {
      MeshOriginV[j]=0.;
      MeshSizeV[j]=10;
      ElementsPerDim[j]=5.;
  }

  Element3DC0LinearHexahedronStrain::Pointer e2=Element3DC0LinearHexahedronStrain::New();
  e2->m_mat=dynamic_cast<MaterialLinearElasticity*>(m);

  try {
      Generate3DRectilinearMesh(e2,S,MeshOriginV,MeshSizeV,ElementsPerDim);
      cout << "Generated 3D rectilinear mesh\n";
  }
  catch (ExceptionObject&) {
      cout << "Could not create 3D mesh - test FAILED\n";
      return EXIT_FAILURE;
  }

  cout << "Test PASSED!\n";
  return EXIT_SUCCESS;
}



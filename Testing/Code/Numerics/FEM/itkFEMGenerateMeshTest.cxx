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
// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMGenerateMesh.h"
#include "itkFEMElement2DC0LinearQuadrilateralStrain.h"
#include "itkFEMMaterialLinearElasticity.h"
#include "itkMacro.h"
#include "itkFEMElement3DC0LinearHexahedronStrain.h"


//
int itkFEMGenerateMeshTest(int, char*[])
{
  // Generate2DRectilinearMesh(m_Element,mySolver,MeshOriginV,MeshSizeV,ElementsPerDim);
  // Generate3DRectilinearMesh(m_Element,mySolver,MeshOriginV,MeshSizeV,ElementsPerDim);

  // Set up the solver object
  itk::fem::Solver S;
  itk::fem::LinearSystemWrapperVNL lsw;

  S.SetLinearSystemWrapper(&lsw);

  // Set up mesh dimensions
  vnl_vector<double> MeshOriginV;
  vnl_vector<double> MeshSizeV;
  vnl_vector<double> ElementsPerDim;

  MeshOriginV.set_size(2);
  MeshSizeV.set_size(2);
  ElementsPerDim.set_size(2);

  for ( unsigned int j=0; j<2; j++ )
    {
    MeshOriginV[j]=0.0;
    MeshSizeV[j]=10;
    ElementsPerDim[j]=5.0;
    }

  typedef  itk::fem::MaterialLinearElasticity  ElasticityType;
  // Create the material
  ElasticityType::Pointer m = ElasticityType::New();

  m->GN = 0;
  m->E = 1000.;
  m->A = 1.0;
  m->h = 1.0;
  m->I = 1.0;
  m->nu = 0.4;
  m->RhoC = 1.0;

  // Create the element type
  typedef itk::fem::Element2DC0LinearQuadrilateralStrain  StrainType;
  StrainType::Pointer e1 = StrainType::New();

  e1->m_mat = dynamic_cast< ElasticityType * >( m );

  try
    {
    itk::fem::Generate2DRectilinearMesh(e1,S,MeshOriginV,MeshSizeV,ElementsPerDim);
    std::cout << "Generated 2D rectilinear mesh" << std::endl;
    }
  catch ( itk::ExceptionObject& )
    {
    std::cerr << "Could not generate 2D mesh - test FAILED" << std::endl;
    return EXIT_FAILURE;
    }

  MeshOriginV.set_size(3);
  MeshSizeV.set_size(3);
  ElementsPerDim.set_size(3);

  for (unsigned int j=0;j<3;j++)
    {
    MeshOriginV[j]=0.;
    MeshSizeV[j]=10;
    ElementsPerDim[j]=5.;
    }

  itk::fem::Element3DC0LinearHexahedronStrain::Pointer e2=itk::fem::Element3DC0LinearHexahedronStrain::New();
  e2->m_mat=dynamic_cast<itk::fem::MaterialLinearElasticity*>(m);

  try
    {
    itk::fem::Generate3DRectilinearMesh(e2,S,MeshOriginV,MeshSizeV,ElementsPerDim);
    std::cout << "Generated 3D rectilinear mesh" << std::endl;
    }
  catch ( itk::ExceptionObject&)
    {
    std::cerr << "Could not create 3D mesh - test FAILED" << std::endl;
    return EXIT_FAILURE;
  }

  delete e1;
  delete m;
  delete e2;
  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}


